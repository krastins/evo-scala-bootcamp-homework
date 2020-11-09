package effects

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// tests should pass against the cats IO impl, you can check this by inverting the comments in the following 2 lines
import effects.EffectsHomework1.IO
//import cats.effect.IO


class EffectsHomework1Spec extends AnyFlatSpec with Matchers {
  val errorMsg = "oops"
  val npe = new NullPointerException(errorMsg)


  "map and flatMap" should "run the console example without premature side effects" in {
    val name: String = "Dog"
    val in = new ByteArrayInputStream(name.getBytes)
    val out = new ByteArrayOutputStream()
    Console.withIn(in) {
      Console.withOut(out) {
        def putStrLn(value: String): IO[Unit] = IO(println(value))
        val readLn: IO[String] = IO(scala.io.StdIn.readLine)

        val program: IO[Unit] = for {
          _ <- putStrLn("What's your name?")
          n <- readLn
          res <- putStrLn(s"Hello, $n!")
        } yield res

        out.toString should be("")
        program.unsafeRunSync()
        out.toString should be(s"What's your name?\nHello, $name!\n")
      }
    }
  }

  "*>" should "not compute if the first io doesn't succeed" in {
    assertThrows[NullPointerException](IO.raiseError(new NullPointerException()).*>(IO("that")).unsafeRunSync())
  }

  "*>" should "replace the result with given value" in {
    IO.pure("this").*>(IO("that")).unsafeRunSync() shouldBe "that"
  }

  "*>" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).*>(IO("that"))
      out.toString shouldBe("")
      io.unsafeRunSync()
      out.toString shouldBe("sidefx")
    }
  }

  "as" should "not compute if the first io doesn't succeed" in {
    assertThrows[NullPointerException](IO.raiseError(new NullPointerException()).as("that").unsafeRunSync())
  }

  "as" should "replace the result with given value" in {
    IO.pure("this").as("that").unsafeRunSync() shouldBe "that"
  }

  "as" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).as("that")
      out.toString shouldBe("")
      io.unsafeRunSync()
      out.toString shouldBe("sidefx")
    }
  }

  "void" should "replace the result with ()" in {
    IO.pure("this").void.unsafeRunSync() shouldBe ()
  }

  "void" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).void
      out.toString shouldBe("")
      io.unsafeRunSync()
      out.toString shouldBe("sidefx")
    }
  }

  "attempt" should "return successful value in a Right" in {
    IO.pure("yay").attempt.unsafeRunSync shouldBe Right("yay")
  }

  "attempt" should "return exception in a Left" in {
    IO.raiseError(npe).attempt.unsafeRunSync shouldBe Left(npe)
  }

  "attempt" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).attempt
      out.toString shouldBe ""
      io.unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "option" should "return successful value as Some" in {
    IO.pure("yay").option.unsafeRunSync shouldBe Some("yay")
  }

  "option" should "return none in case of a failure" in {
    IO.raiseError(npe).option.unsafeRunSync shouldBe None
  }

  "option" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).option
      out.toString shouldBe ""
      io.unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "handleErrorWith" should "handle errors" in {
    IO.pure("all good").handleErrorWith(_ => IO("NPE")).unsafeRunSync() shouldBe "all good"
    IO.raiseError(npe).handleErrorWith(_ => IO("NPE")).unsafeRunSync() shouldBe "NPE"
  }

  "handleErrorWith" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).handleErrorWith(_ => IO.unit)
      out.toString shouldBe("")
      io.unsafeRunSync()
      out.toString shouldBe("sidefx")
    }
  }

  "redeem" should "handle errors" in {
    IO.pure(1).redeem(_.getMessage, _.toString).unsafeRunSync() shouldBe "1"
    IO.raiseError(npe).redeem(_.getMessage, Integer.toString).unsafeRunSync() shouldBe errorMsg
  }

  "redeem" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).redeem(_.getMessage, _.toString)
      out.toString shouldBe ""
      io.unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "redeemWith" should "handle errors" in {
    val recoverMsg = (error: Throwable) => IO(error.getMessage)
    val bindToString = (i: Int) => IO(i.toString)

    IO.pure(1).redeemWith(recoverMsg, bindToString).unsafeRunSync() shouldBe "1"
    IO.raiseError(npe).redeemWith(recoverMsg, bindToString).unsafeRunSync() shouldBe errorMsg
  }

  "redeemWith" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO(print("sidefx")).redeemWith(_ => IO.unit, _ => IO.unit)
      out.toString shouldBe ""
      io.unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "unsafeToFuture" should "have side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      implicit val ec: ExecutionContext = ExecutionContext.global
      val io: Future[String] = IO({print("sidefx"); "res"}).unsafeToFuture()
      io.onComplete {
        case Success(value) =>
          out.toString shouldBe "sidefx"
          value shouldBe "res"
        case Failure(_) => fail("unsafeToFuture did not succeed")
      }
    }
  }

  "unsafeToFuture" should "handle errors" in {
    implicit val ec: ExecutionContext = ExecutionContext.global
    val io: Future[String] = IO.raiseError(npe).unsafeToFuture()
    io.onComplete {
      case Failure(exception) => exception shouldBe npe
      case Success(_) => fail("unsafeToFuture did not fail with exception")
    }
  }

  "suspend" should "return io" in {
    IO.suspend(IO("thing")).unsafeRunSync() shouldBe "thing"
  }

  "suspend" should "have no premature side effects" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      val io = IO.suspend(IO(print("sidefx")))
      out.toString shouldBe("")
      io.unsafeRunSync()
      out.toString shouldBe("sidefx")
    }
  }

  "fromEither" should "handle right values" in {
    IO.fromEither(Right("foo")).unsafeRunSync() shouldBe "foo"
  }

  "fromEither" should "handle left values" in {
    assertThrows[NullPointerException](IO.fromEither(Left(npe)).unsafeRunSync())
  }

  "fromOption" should "handle some values" in {
    IO.fromOption(Some("foo"))(npe).unsafeRunSync() shouldBe "foo"
  }

  "fromOption" should "handle none" in {
    assertThrows[NullPointerException](IO.fromOption(None)(npe).unsafeRunSync())
  }

  "none" should "be return empty option" in {
    assert(IO.none.unsafeRunSync().isEmpty)
  }

  "fromTry" should "handle some values" in {
    IO.fromTry(Try("foo")).unsafeRunSync() shouldBe "foo"
  }

  "fromTry" should "handle exceptions" in {
    assertThrows[NullPointerException](IO.fromTry(throw npe).unsafeRunSync())
  }

  "raiseUnless" should "raise error when cond is false" in {
    assertThrows[NullPointerException](IO.raiseUnless(cond = false)(npe).unsafeRunSync())
  }

  "raiseUnless" should "return unit when cond is true" in {
    IO.raiseUnless(cond = true)(npe).unsafeRunSync() should be ()
  }

  "raiseWhen" should "raise error when cond is true" in {
    assertThrows[NullPointerException](IO.raiseWhen(cond = true)(npe).unsafeRunSync())
  }

  "raiseWhen" should "return unit when cond is false" in {
    IO.raiseWhen(cond = false)(npe).unsafeRunSync() should be ()
  }

  "unlessA" should "preserve side effects when cond=false" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      IO.unlessA(cond = false)(IO(print("sidefx"))).unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "unlessA" should "return unit with no side effects when cond=true" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      IO.unlessA(cond = true)(IO(print("sidefx"))).unsafeRunSync() should be ()
      out.toString shouldBe empty
    }
  }

  "whenA" should "preserve side effects when cond=true" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      IO.whenA(cond = true)(IO(print("sidefx"))).unsafeRunSync()
      out.toString shouldBe "sidefx"
    }
  }

  "whenA" should "return unit with no side effects when cond=false" in {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      IO.whenA(cond = false)(IO(print("sidefx"))).unsafeRunSync() should be ()
      out.toString shouldBe empty
    }
  }
}