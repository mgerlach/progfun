package idealized.scala

/**
  * @author martin.gerlach
  */
abstract class Boolean {
  def ifThenElse[T](t: T, e: T): T

  def && (x: => Boolean): Boolean = ifThenElse(x, ffalse)
  def || (x: => Boolean): Boolean = ifThenElse(ttrue, x)
  def unary_! : Boolean = ifThenElse(ffalse, ttrue)

  def == (x: => Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: => Boolean): Boolean = ifThenElse(x.unary_!, x)

  def < (x: => Boolean): Boolean = ifThenElse(ffalse, x)
}

object ttrue extends Boolean {
  def ifThenElse[T](t: T, e: T): T = t
  override def toString = "ttrue"
}

object ffalse extends Boolean {
  def ifThenElse[T](t: T, e: T): T = e
  override def toString = "ffalse"
}