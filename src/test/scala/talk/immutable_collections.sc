// --- immutable linear sequences (e.g., List)
Nil
val emptyList : List[Nothing] = Nil


val list1cons = "e1" :: emptyList // cons operator (see Lisp, Scheme, etc.), only for List!
val list1left = emptyList :+ "e1"
val list1right = "e1" +: emptyList
//
val list2cons = "e1" :: "e2" :: Nil // List("e1", "e2")
val list2left = list1cons :+ "e2"
val list2left_methodCall = list1cons.:+("e2")
val list2right = "e2" +: list1cons
val list2right_methodCall = list1cons.+:("e2") // OOPS
val list2rightcons  = "e2" :: list1cons // same OOPS
list1cons +: "e2"
"e1" ++ "e2"
list1cons ::: list2cons
list1left ++ list2left
var list3 = list1cons
list3 :+= "e3" // e1, e3
list3
list1cons

//
//var list4 = list1left
//list4 +:= "e4" // e4, e1 !!!
//list4
//list1left
//
//// --- immutable indexed sequences (e.g. Vector, more efficient operations on larger data sets)
//
val emptyVector : Seq[Nothing] = Vector()

val vec1left = emptyVector :+ "e1"
val vec1right = "e1" +: emptyVector

val vec2left = vec1left :+ "e2"
val vec2right = "e2" +: vec1left // OOPS
//
//var vec3 = vec1left
//vec3 :+= "e3"
//vec3
//vec1left
//
//// --- immutable sets
//
//val emptySet : Set[Nothing] = Set()
//
//val set1left = emptySet + "e1" // ??? Set()e1 ... string repr... type confusion
//val emptyStrSet = emptySet.toSet[String]
//
//val set1leftBetter = emptyStrSet + "e1" // ahhh... Set(e1)
//val set1right = "e1" + emptyStrSet // does not work with Sets ... e1Set() string repr
//
//val set2left = set1leftBetter + "e2"
//
//set1leftBetter ++ set2left
//
//var set3 = set1leftBetter
//set3 += "e3"
//set3
//set1leftBetter
//
//// --- immutable maps
//
val m = Map.empty // == Map() == new Map()

val m1 = Map(1 -> "one")
val m1a = Map(1 -> "foobar")
val m1b = m1 ++ m1a

val m2 = Map(2 -> "two")
val m12 = m1 ++ m2

var m3 = m1
val m3a = m3
m3 += (3 -> "three")
m3
m1
m3a

var m4 = m1
val m4a = m4
m4 ++= m2
m4
m1
m4a
//
//
//
