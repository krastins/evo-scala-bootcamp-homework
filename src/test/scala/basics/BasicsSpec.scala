package basics

import org.scalatest.wordspec.AnyWordSpec
import basics.Basics._

class BasicsSpec extends AnyWordSpec {
  "gcd" should {
    "calculate the greatest common divisor" in {
      assert(gcd(8, 12) === 4)
      assert(gcd(4932, 3480) === 12)
      assert(gcd(736538, 97438) === 22)
      assert(gcd(489376225, 93753050) === 25)
      assert(gcd(65536, 131072) === 65536)
      assert(gcd(0, 5) === 5)
      assert(gcd(0, 0) === 0)
      }
    }

  "lcm" should {
    "calculate the least common multiple" in {
      assert(lcm(21, 6) === 42)
      assert(lcm(84, 32) === 672)
      assert(lcm(432, 264) === 4752)
      assert(lcm(0, 12) === 0)
      assert(lcm(0, 0) === 0)
    }
  }
}
