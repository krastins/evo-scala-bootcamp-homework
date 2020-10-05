package basics

import basics.ControlStructuresHomework.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    val value: Double
    val command: Command
  }

  final case class MyResult(value: Double, command: Command) extends Result {
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val command = x.split("\\s+").toList

    command match {
      case "divide" :: dividend :: divisor :: Nil => Right(Divide(dividend.toDouble, divisor.toDouble))
      case "divide" :: _ => Left(ErrorMessage("Invalid number of arguments for divide"))
      case ("sum" | "average" | "min" | "max") :: Nil => Left(ErrorMessage(s"Invalid number of arguments for ${command.head}"))
      case "sum" :: args => Right(Sum(args.map(_.toDouble)))
      case "average" :: args => Right(Average(args.map(_.toDouble)))
      case "min" :: args => Right(Min(args.map(_.toDouble)))
      case "max" :: args => Right(Max(args.map(_.toDouble)))
      case _ => Left(ErrorMessage(s"Invalid command"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend, divisor) =>
        if (divisor != 0) Right(MyResult(dividend / divisor, x))
        else Left(ErrorMessage("Division by zero"))
      case Command.Sum(numbers) => Right(MyResult(numbers.iterator.sum, x))
      case Command.Average(numbers) => Right(MyResult(numbers.iterator.sum / numbers.size, x))
      case Command.Min(numbers) => Right(MyResult(numbers.min, x))
      case Command.Max(numbers) => Right(MyResult(numbers.max, x))
    }
  }

  def renderResult(x: Result): String = {
    x.command match {
      case Divide(dividend, divisor) => s"$dividend divided by $divisor is ${x.value}"
      case Sum(numbers) => s"the sum of ${numbers.mkString(" ")} is ${x.value}"
      case Average(numbers) => s"the average of ${numbers.mkString(" ")} is ${x.value}"
      case Min(numbers) => s"the minimum of ${numbers.mkString(" ")} is ${x.value}"
      case Max(numbers) => s"the maximum of ${numbers.mkString(" ")} is ${x.value}"
    }
  }

  def process(x: String): String = {
    val result = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result

    result match {
      case Right(result) => renderResult(result)
      case Left(errorMessage) => "Error: " + errorMessage.value
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
