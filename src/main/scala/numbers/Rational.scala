package numbers

import java.lang.Math.abs

/**
  * @author martin.gerlach
  */
class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = abs(gcd(x, y))

  def numer = x / g

  def denom = y / g

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def unary_- = new Rational(-this.numer, this.denom)

  def inv = new Rational(this.denom, this.numer)

  def +(that: Rational) = new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

  def -(that: Rational) = this + -that

  def *(that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

  def /(that: Rational) = this * that.inv

  override def toString = numer + "/" + denom

}
