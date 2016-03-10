import collections._

val set1 = EmptyIntSet.incl(10).incl(2).incl(11).incl(5).incl(6)

val set2 = EmptyIntSet.incl(100).incl(2).incl(4).incl(11).incl(3)

set1 union set2

val l2 = collections.ConsList(1, 2)

val l1 = collections.ConsList(1)

val l0 = collections.ConsList()

l2.head
l2.tail
l2 prepend 10

