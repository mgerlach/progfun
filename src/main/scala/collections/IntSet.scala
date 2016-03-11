package collections

/**
  * @author martin.gerlach
  */
abstract class IntSet {

  def add(x: Int): IntSet
  def +(x:Int) = add(x)
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object EmptyIntSet extends IntSet {
  def add(x: Int) = new NonEmptyIntSet(x, EmptyIntSet, EmptyIntSet)
  def contains(x: Int) = false
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmptyIntSet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def add(x: Int) = {
    if (x < elem) new NonEmptyIntSet(elem, left add x, right)
    else if (x > elem) new NonEmptyIntSet(elem, left, right add x)
    else this
  }
  def contains(x: Int) = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def union(other: IntSet) = (left union (right union other)) add elem
  override def toString = "{" + left + elem + right + "}"
}
