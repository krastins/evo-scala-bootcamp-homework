package basics

object Basics {
  def lcm(a: Int, b: Int): Int = {
    val divisor = gcd(a, b)
    if (divisor != 0) (a * b).abs / divisor
    else 0
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }
}
