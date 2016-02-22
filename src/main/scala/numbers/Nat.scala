package numbers

/**
  * Peano numbers
  *
  * @author martin.gerlach
  */
abstract class Nat {

  def isZero: Boolean

  def predecessor: Nat

  def successor = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def value: Int

  override def toString = value.toString
}

object Zero extends Nat {

  def isZero = true

  def predecessor: Nothing = throw new IllegalArgumentException("Zero.predecessor")

  def +(that: Nat) = that

  def -(that: Nat) = if (that.isZero) this else throw new IllegalArgumentException("Negative number")

  def value = 0
}

class Succ(n: Nat) extends Nat {

  def isZero = false

  def predecessor = n

  def +(that: Nat): Nat = new Succ(n + that) // Succ(this - 1 + that) = this - 1 + that + 1

  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor // (this - 1) - (that - 1)

  def value = 1 + predecessor.value
}