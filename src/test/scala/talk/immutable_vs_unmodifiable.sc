import java.util
import java.util.Collections

// make a mutable collection unmodifiable (Java)

val modifiable: util.List[String] = new util.LinkedList()
modifiable.add("s1")
modifiable.add("s2")

val unmodifiable = Collections.unmodifiableList(modifiable)
// unmodifiable.add("s3")

// immutable
//
// val = final
val immutable: List[String] = Nil
immutable :+ "s1"
immutable
// immutable :+= "s2"
//
// var = non-final
var immutableVar: List[String] = Nil
immutableVar :+ "s1"
immutableVar
// the collections are still immutable!
val immutableVarBackup = immutableVar
immutableVar :+= "s1"
immutableVar
immutableVarBackup



