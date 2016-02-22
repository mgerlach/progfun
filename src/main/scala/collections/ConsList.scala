package collections

import java.util.NoSuchElementException

/**
  * Conslists
  *
  * @author martin.gerlach
  */
trait ConsList[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: ConsList[T]

  def prepend[U >: T](elem: U): ConsList[U] = new Cons(elem, this)
}

case class Cons[T](val head: T, val tail: ConsList[T]) extends ConsList[T] {
  def isEmpty = false

  override def toString = "[" + head + ", " + tail + "]"
}

object Nil extends ConsList[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString = "."
}

object ConsList {
  def apply[T]() = Nil

  def apply[T](e0: T) = Cons(e0, Nil)

  def apply[T](e0: T, e1: T) = Cons(e0, Cons(e1, Nil))
}