package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import scala.util.Random
import java.util.UUID.randomUUID

import cats.effect.concurrent.Ref
import org.http4s.client.Client
import org.http4s.server.middleware.Logger

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.


object GuessServer extends IOApp {

  object Model {
    final case class Game(id: String, min: Int, max: Int, attempts: Int, number: Int)

    object Game {
      def apply(min: Int, max: Int, maxAttempts: Int): Game =
        Game(
          randomUUID.toString,
          min,
          max,
          maxAttempts,
          Random.nextInt(max - min + 1) + min)
    }

    final case class Guess(number: Int)

    final case class Answer(message: String, comparison: Int, attemptsLeft: Int)

    object Answer {
      def apply(guessedNumber: Int, game: Game): Answer = {
        val attemptsMsg = s"You have ${game.attempts} attempt${if (game.attempts != 1) "s" else ""} left."

        game.number.compare(guessedNumber) match {
          case 1 => Answer(s"The number is bigger than your guess $guessedNumber." + attemptsMsg, 1, game.attempts)
          case 0 => Answer(s"You guessed the number $guessedNumber correctly!", 0, game.attempts)
          case -1 => Answer(s"The number is smaller than your guess $guessedNumber." + attemptsMsg, -1, game.attempts)
        }
      }
    }
  }

  import io.circe.syntax._
  import org.http4s.circe.CirceEntityCodec._
  import io.circe.generic.semiauto._
  import Model._

  // filter fields that shouldn't appear in external model
  implicit val gameDecoder: Decoder[Game] =
    Decoder.forProduct3("min", "max", "attempts")(Game.apply)
  implicit val gameEncoder: Encoder[Game] =
    Encoder.forProduct4("id", "min", "max", "attempts")(g => (g.id, g.min, g.max, g.attempts))

  implicit val guessDecoder: Decoder[Guess] = deriveDecoder[Guess]
  implicit val guessEncoder: Encoder[Guess] = deriveEncoder[Guess]

  implicit val evaluationDecoder: Decoder[Answer] = deriveDecoder[Answer]
  implicit val evaluationEncoder: Encoder[Answer] = deriveEncoder[Answer]

  def getAttempt(state: Ref[IO, Map[String, Game]], id: String, guess: Int): IO[Option[Game]] =
    state
      .updateAndGet(_.updatedWith(id)(_.map(game => game.copy(attempts =
        if (guess == game.number)
          if (game.attempts > 0) 0 else -1 // consider the game over if the correct number is guessed
        else game.attempts - 1))))
      .map(_.get(id))

  def gameRoutes(state: Ref[IO, Map[String, Game]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "guess" =>
      for {
        game <- req.as[Game]
        _ <- state.update(m => m.updated(game.id, game))
        cachedGame <- state.get.map(m => m.get(game.id))
        res <- Ok(cachedGame.asJson)
      } yield res

    case req @ POST -> Root / "guess" / id =>
      for {
        guess <- req.as[Guess]
        storedGame <- getAttempt(state, id, guess.number)
        res <- storedGame match {
          case Some(game) =>
            if (game.attempts >= 0) Ok(Answer(guess.number, game))
            else Gone("You have no attempts left")
          case None => NotFound(s"Game with id $id not found")
        }
      } yield res
  }

  private[http] def httpApp(state: Ref[IO, Map[String, Game]]) =
    Logger.httpApp(logHeaders = true, logBody = true)(gameRoutes(state).orNotFound)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      state <- Ref.of[IO, Map[String, Game]](Map.empty)
      exitCode <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 8080, host = "localhost")
        .withHttpApp(httpApp(state))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
  }
}

object GuessClient extends IOApp {

  object Model {
    final case class Game(id: Option[String], min: Int, max: Int, attempts: Int)

    final case class Guess(number: Int)

    final case class Answer(message: String, comparison: Int, attemptsLeft: Int)
  }

  import io.circe.generic.semiauto._
  import org.http4s.circe.CirceEntityCodec._
  import Model._

  implicit val gameDecoder: Decoder[Game] = deriveDecoder[Game]
  implicit val gameEncoder: Encoder[Game] =
    Encoder.forProduct3("min", "max", "attempts")(g => (g.min, g.max, g.attempts))

  implicit val answerDecoder: Decoder[Answer] = deriveDecoder[Answer]
  implicit val guessEncoder: Encoder[Guess] = deriveEncoder[Guess]

  private val uri = uri"http://localhost:8080"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def guess(client: Client[IO], gameId: String, min: Int, max: Int): IO[Answer] = {
    val numberGuessed = (min + max) / 2

    client.expect[Answer](Method.POST(Guess(numberGuessed), uri / "guess" / gameId)).flatMap {
      case Answer(_, comparison, attemptsLeft) if attemptsLeft > 0 => comparison match {
        // case 0 not considered here, because it's filtered above as attemptsLeft will be 0 if number is guessed
        case 1 => guess(client, gameId, numberGuessed, max)
        case -1 => guess(client, gameId, min, numberGuessed)
      }
      case answer @ Answer(_, _, _) => IO(answer)
    }
  }

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, _) =>
      for {
        game <- client.expect[Game](Method.POST(Game(None, 20, 50, 4), uri / "guess"))
        answer <- guess(client, game.id.get, game.min, game.max)
        _ <- printLine(answer.message)
      } yield ()
    }.as(ExitCode.Success)
}
