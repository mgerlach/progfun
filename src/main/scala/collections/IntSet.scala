package collections

/**
  * @author martin.gerlach
  */
abstract class IntSet {

  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object EmptyIntSet extends IntSet {
  def incl(x: Int) = new NonEmptyIntSet(x, EmptyIntSet, EmptyIntSet)
  def contains(x: Int) = false
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmptyIntSet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) = {
    if (x < elem) new NonEmptyIntSet(elem, left incl x, right)
    else if (x > elem) new NonEmptyIntSet(elem, left, right incl x)
    else this
  }
  def contains(x: Int) = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def union(other: IntSet) = (left union (right union other)) incl elem
  override def toString = "{" + left + elem + right + "}"
}
