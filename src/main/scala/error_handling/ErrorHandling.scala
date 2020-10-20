package error_handling

import scala.util.Try
import java.time.{LocalDate, YearMonth}
import java.time.format.DateTimeFormatter

object ErrorHandling extends App {

  import cats.data.ValidatedNec
  import cats.syntax.all._

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  // adapted from https://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Functional_style
  object LuhnAlgorithm {
    def checksum(digits: Seq[Int]): Int = {
      digits.reverse.zipWithIndex.foldLeft(0) { case (sum, (digit, i)) =>
        if (i % 2 == 0) sum + digit
        else sum + (digit * 2) / 10 + (digit * 2) % 10
      } % 10
    }
  }


  final case class Name(value: String) extends AnyVal
  final case class CardNumber(value: String) extends AnyVal
  final case class SecurityCode(value: String) extends AnyVal


  case class PaymentCard(name: Name,
                         cardNumber: CardNumber,
                         expirationDate: LocalDate,
                         securityCode: SecurityCode)

  sealed trait ValidationError
  object ValidationError {
    final case object EmptyName extends ValidationError {
      override def toString: String = "Name cannot be empty"
    }
    final case object NonAlphabeticName extends ValidationError {
      override def toString: String = "Name must contain only alphabetic characters and spaces"
    }
    final case object CardNumberIsNotNumeric extends ValidationError {
      override def toString: String = "Card number must contain only digits"
    }
    final case object CardNumberLength extends ValidationError {
      override def toString: String = "Card number must be sixteen digits long"
    }
    final case object CardNumberInvalidChecksum extends ValidationError {
      override def toString: String = "Invalid card number: incorrect checksum"
    }
    final case object InvalidDateFormat extends ValidationError {
      override def toString: String = "Expiration date must be in the format \"MM/yy\""
    }
    final case object InvalidDate extends ValidationError {
      override def toString: String = "Expiration date is an invalid date"
    }
    final case object CardExpired extends ValidationError {
      override def toString: String = "Card has expired"
    }
    final case object SecurityCodeIsNotNumeric extends ValidationError {
      override def toString: String = "Security code is not numeric"
    }
    final case object SecurityCodeLength extends ValidationError {
      override def toString: String = "Security code is not three digits long"
    }
  }

  object PaymentCardValidator {

    import ValidationError._
    import LuhnAlgorithm._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(name: String,
                 number: String,
                 expirationDate: String,
                 securityCode: String,
                 todaysDate: LocalDate = LocalDate.now()
                ): AllErrorsOr[PaymentCard] = {
      def validateNameNotEmpty(name: String): AllErrorsOr[String] =
        if (!name.isEmpty) name.validNec
        else EmptyName.invalidNec

      def validateNameAlphabetic(name: String): AllErrorsOr[Name] =
        if (name.matches("^[a-zA-Z ]+$")) Name(name).validNec
        else NonAlphabeticName.invalidNec

      def validateCardNumberIsNumeric(cardNumber: String): AllErrorsOr[String] =
        if (cardNumber.forall(c => c.isDigit)) cardNumber.validNec
        else CardNumberIsNotNumeric.invalidNec

      def validateCardNumberLength(cardNumber: String): AllErrorsOr[String] =
        if (cardNumber.length == 16) cardNumber.validNec
        else CardNumberLength.invalidNec

      def validateChecksum(number: String): AllErrorsOr[CardNumber] =
        if (checksum(number.map(_.asDigit)) == 0) CardNumber(number).validNec
        else CardNumberInvalidChecksum.invalidNec

      // assuming the most common format of “MM/YY”
      def validateDateFormat(date: String): AllErrorsOr[String] =
        if (date.matches("^(0[1-9]|1[0-2])/?([0-9]{4}|[0-9]{2})$")) date.validNec
        else InvalidDateFormat.invalidNec

      def validateDate(date: String): AllErrorsOr[LocalDate] = {
        val formatter = DateTimeFormatter.ofPattern("MM/yy")

        val dateOption = Try(formatter.parse(date)).toOption
        if (dateOption.isDefined) YearMonth.from(dateOption.get).atEndOfMonth().validNec
        else InvalidDate.invalidNec
      }

      // cards are valid through the end of the month and until the end of the last day of the month
      def validateNotExpired(reference: LocalDate)(date: LocalDate): AllErrorsOr[LocalDate] =
        if (date == reference || date.isAfter(reference)) date.validNec
        else CardExpired.invalidNec

      def validateSecurityCodeIsNumeric(code: String): AllErrorsOr[String] =
        if (code.toIntOption.isDefined) code.validNec
        else SecurityCodeIsNotNumeric.invalidNec

      def validateSecurityCodeLength(code: String): AllErrorsOr[SecurityCode] = {
        // assuming not an AmEx card with 4 digit security code, since we have no way of distinguishing the issuer
        if (code.length === 3) SecurityCode(code).validNec
        else SecurityCodeLength.invalidNec
      }

      def validateName(name: String): AllErrorsOr[Name] = {
        validateNameNotEmpty(name).andThen(validateNameAlphabetic)
      }

      def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {
        validateCardNumberIsNumeric(number)
        .productR(validateCardNumberLength(number))
        .andThen(validateChecksum)
      }

      def validateExpirationDate(expirationDate: String): AllErrorsOr[LocalDate] = {
        validateDateFormat(expirationDate)
        .productR(validateDate(expirationDate))
        .andThen(validateNotExpired(todaysDate))
      }

      def validateSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] = {
        validateSecurityCodeIsNumeric(securityCode).andThen(validateSecurityCodeLength)
      }

      (validateName(name),
        validateCardNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode))
        .mapN(PaymentCard)
    }
  }
}
