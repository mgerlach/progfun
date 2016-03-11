import collections._

val l0 = ConsList()

val l1 = ConsList(1)

val l2 = l1 prepend 2

l2.head

l2.tail

val l3 = l2 prepend 10

l2

val set1 = EmptyIntSet
val set11 = set1.add(1)
val set12 = set11.add(2)
val set13 = set12 + 0
val set1_ = set13 + 11 + 5 + 6

val set2_ = EmptyIntSet.add(100).add(2).add(4).add(11).add(3)

set1_ union set2_




