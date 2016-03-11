import collections._

val x: ConsList[String] = Nil
val set1 = EmptyIntSet.add(10).add(2).add(11).add(5).add(6)
val set2 = EmptyIntSet.add(100).add(2).add(4).add(11).add(3)
set1 union set2

collections.ConsList(1, 2)
collections.ConsList(1)
collections.ConsList()

// types example, result type is ConsList[IntSet]
def f(xs: ConsList[NonEmptyIntSet], x: EmptyIntSet.type) = xs prepend x

