package offer3

import context.{Context, Global}

import scala.util.Try


/**
  * OO approach, Offer as case class containing a Map[String, Option[Any]]
  * Property accessors are (lazily initialized) instance properties
  * Property actions (ignore, reset, set) controlled by Option[Any]
  *
  * Adds get with additional optional generic list access function (plus int index overload as special case) for list properties.
  *
  * @author martin.gerlach
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  lazy val sku = new SimpleAccess[String]("sku")

  lazy val brand = new SimpleAccess[String]("brand")

  lazy val title = new ContextualSimpleAccess[String]("title")

  lazy val categoryPaths = new ListAccess[String]("categoryPaths")

  // reflective access - cannot parameterize the all map with type "[String, Type => Access[Any, Any, Any]]" as this would
  // require the Access type params to be covariant (i.e. Access[+T, +K1, +V]), making the "...Generic" function signatures
  // rather complex. So we use a private map with "Any" values and a public accessor using a type cast.

  // TODO use reflection/annotations for genericAccessFor?

  private lazy val all: Map[String, Any] = Map("sku" -> sku, "brand" -> brand, "categoryPaths" -> categoryPaths, "title" -> title)

  def genericAccessFor[T, K1, V](k0: String): Access[T, K1, V] = all(k0).asInstanceOf[Access[T, K1, V]]

  sealed abstract class Access[T, K1, V](k0: String) {

    // creates new lists or maps
    def setGeneric(c: Option[Context], k1: Option[K1])(v: V): Offer

    // adds to lists or maps
    def addGeneric(c: Option[Context], k1: Option[K1])(v: V): Offer

    // remove from inner maps, (if map is empty afterwards and for simple and lists == reset)
    def removeGeneric(c: Option[Context], k1: Option[K1]): Offer

    // exact get
    def getGeneric(c: Option[Context], k1: Option[K1], li: Option[List[V] => V]): Option[V]

    //    // moves up the context hierarchy if contextual, otherwise == get
    //    def findGeneric(c: Option[Context], k1: Option[K1]): Option[V]

    // remove k0 from map, meaning to completely ignore the key for PUT to OS
    def ignore: Offer = new Offer(m - k0)

    // add empty structure for k0 to indicate reset in OS
    def reset: Offer = new Offer(m updated(k0, None))

    def propertyOption: Option[Option[T]] = m.get(k0).asInstanceOf[Option[Option[T]]]

    protected def topLevelOrElse(initTopLevel: => T): T = propertyOption match {
      case None | Some(None) => initTopLevel
      case Some(Some(topLevel)) => topLevel
    }

    // TODO

    // filter => where to store filterReasons/Details?

    // map essentially loops through all values, possibly replacing them
  }

  class SimpleAccess[V](k0: String) extends Access[V, Nothing, V](k0) {

    def set(v: V): Offer = setGeneric(None, None)(v)

    def remove: Offer = removeGeneric(None, None)

    def get: Option[V] = getGeneric(None, None, None)

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = new Offer(m updated(k0, Option(v)))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = setGeneric(c, k1)(v)

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Offer = reset

    def getGeneric(c: Option[Context], k1: Option[Nothing], li: Option[List[V] => V]): Option[V] =
      propertyOption.flatMap(simpleOption => simpleOption)

    //    def findGeneric(c: Option[Context], k1: Option[Nothing]): Option[V] = getGeneric(c, k1)

  }

  class ListAccess[V](k0: String) extends Access[List[V], Nothing, V](k0) {

    def set(v: V): Offer = setGeneric(None, None)(v)

    def add(v: V): Offer = addGeneric(None, None)(v)

    def remove: Offer = removeGeneric(None, None)

    def get(li: List[V] => V): Option[V] = getGeneric(None, None, Option(li))

    def get(i: Int): Option[V] = get(l => l(i))

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = new Offer(m updated(k0, Option(List(v))))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = new Offer(m updated(k0, Option(topLevelOrElse(Nil) :+ v)))

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Offer = reset

    def getGeneric(c: Option[Context], k1: Option[Nothing], li: Option[List[V] => V]): Option[V] =
      propertyOption.flatMap(listOption => Try[Option[V]](listOption.map(li.getOrElse(l => l.head))).getOrElse(None))
  }


  class ContextualSimpleAccess[V](k0: String) extends Access[Map[Context, V], Nothing, V](k0) {

    def set(c: Context)(v: V): Offer = setGeneric(Option(c), None)(v)

    def add(c: Context)(v: V): Offer = addGeneric(Option(c), None)(v)

    def remove(c: Context): Offer = removeGeneric(Option(c), None)

    def get(c: Context): Option[V] = getGeneric(Option(c), None, None)

    //    def find(c: Context): Option[V] = findGeneric(Option(c), None)

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = new Offer(m updated(k0, Option(Map[Context, V](c.getOrElse(Global) -> v))))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Offer = new Offer(m updated(k0, Option(topLevelOrElse(Map()) + (c.getOrElse(Global) -> v))))

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Offer = propertyOption match {
      case None => reset
      case Some(None) => Offer.this
      case Some(Some(contextMap)) =>
        val updatedMap = contextMap - c.getOrElse(Global)
        if (updatedMap.isEmpty) reset else new Offer(m updated(k0, Option(updatedMap)))
    }

    def getGeneric(c: Option[Context], k1: Option[Nothing], li: Option[List[V] => V]): Option[V] =
      propertyOption.flatMap(ctxOption => ctxOption.flatMap(ctxMap => ctxMap.get(c.getOrElse(Global))))

  }

}