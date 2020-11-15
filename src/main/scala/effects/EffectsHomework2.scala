package effects

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework2 {
  import scala.concurrent.Future
  import scala.util.Try

  sealed abstract class IO[A] {
    // FIXME: Suspicious shadowing by a Type Parameter: A
    final case class Delay[A](thunk: () => A) extends IO[A]
    final case class Pure[A](a: A) extends IO[A]
    final case class Map[A, B](a: IO[A], f: A => B) extends IO[B]
    final case class FlatMap[A, B](a: IO[A], f: A => IO[B]) extends IO[B]
    final case class Redeem[A, B](a: IO[A], recover: Throwable => B, f: A => B) extends IO[B]
    final case class FlatRedeem[A, B](a: IO[A], recover: Throwable => IO[B], f: A => IO[B]) extends IO[B]

    private def run(): A = run(this)

    @tailrec
    def run(io: IO[A]): A = io match {
      case Delay(thunk) => thunk()

      case Pure(a) => a

      // the code below compiles and works but I'm not sure how to deal with these type errors:
      // FIXME: Type mismatch. Required: Nothing, found: Any
      case Map(a, f) => f(a.run())

      // FIXME: Type mismatch. Required: Nothing, found: Any
      case FlatMap(a, f) => run(f(a.run()))

      // FIXME: Type mismatch. Required: Any => NotInferredU, found: Nothing => A
      case Redeem(a, recover, f) => Try(a.run()).fold(recover, f)

      // FIXME: Type mismatch. Required: Any => NotInferredU, found: Nothing => IO[A]
      case FlatRedeem(a, recover, f) => run(Try(a.run()).fold(recover, f))
    }

    def map[B](f: A => B): IO[B] = Map(this, f)

    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())

    def attempt: IO[Either[Throwable, A]] = Redeem[A, Either[Throwable, A]](this, Left(_), Right(_))

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = Redeem(this, recover, map)

    def option: IO[Option[A]] = Redeem[A, Option[A]](this, _ => None, Some(_))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = FlatRedeem(this, f, Pure[AA])

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = FlatRedeem(this, recover, bind)

    def unsafeRunSync(): A = run(this)

    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(run(this))
  }

  object IO extends IO {

    def apply[A](body: => A): IO[A] = delay(body)

    def suspend[A](thunk: => IO[A]): IO[A] = Map(thunk, identity(_: A))

    def delay[A](body: => A): IO[A] = Delay(() => body)

    def pure[A](a: A): IO[A] = Pure(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(IO.raiseError, pure)

    def none[A]: IO[Option[A]] = pure(None)

    def raiseError[A](e: Throwable): IO[A] = Delay(() => throw e)

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option map pure getOrElse raiseError(orElse)

    def fromTry[A](t: Try[A]): IO[A] = t.fold(IO.raiseError, pure)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit

    val unit: IO[Unit] = pure(())
  }
}
