package error_handling

import error_handling.ErrorHandling._
import error_handling.ErrorHandling.ValidationError._
import PaymentCardValidator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import java.time.LocalDate

import cats.syntax.all._


class ErrorHandlingSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "validator" should "return valid credit card" in {
    val now = LocalDate.of(2020, 10, 19)
    val res = validate("Foo Bar", "4242424242424242", "12/20", "123", now)
    res shouldBe PaymentCard(
      Name("Foo Bar"),
      CardNumber("4242424242424242"),
      LocalDate.of(2020, 12, 31),
      SecurityCode("123"))
      .validNec
  }

  it should "return valid card even on the last day of expiration date" in {
    val now = LocalDate.of(2020, 10, 19)
    val res = validate("Foo Bar", "4242424242424242", "10/20", "123", now)
    res shouldBe PaymentCard(
      Name("Foo Bar"),
      CardNumber("4242424242424242"),
      LocalDate.of(2020, 10, 31),
      SecurityCode("123"))
      .validNec
  }

  it should "accumulate errors" in {
    val now = LocalDate.of(2020, 10, 19)
    val res = validate("#cc", "4242424242424243", "10/01", "123456", now)
    res.leftMap(_.toList.toSet) shouldBe Set(
      NonAlphabeticName,
      CardNumberInvalidChecksum,
      CardExpired,
      SecurityCodeLength)
      .invalid
  }
}