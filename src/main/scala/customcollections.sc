import collections._

val x: ConsList[String] = Nil
val set1 = EmptyIntSet.incl(10).incl(2).incl(11).incl(5).incl(6)
val set2 = EmptyIntSet.incl(100).incl(2).incl(4).incl(11).incl(3)
set1 union set2

collections.ConsList(1, 2)
collections.ConsList(1)
collections.ConsList()

// types example, result type is ConsList[IntSet]
def f(xs: ConsList[NonEmptyIntSet], x: EmptyIntSet.type) = xs prepend x

