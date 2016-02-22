package offer2

import context.{Context, Global}

sealed trait PropertyAction[+T] {
  def v: T
}

case object ResetProperty extends PropertyAction[Nothing] {
  def v = throw new NoSuchElementException("ResetProperty.v")
}

case class SetProperty[T](val v: T) extends PropertyAction[T]

/**
  * Functional approach, Offer is just a type alias for Map[String, PropertyAction[Any]]
  * Property accessors obtained as results of functions on offer type (Map)
  * Property actions (ignore, reset, set) controlled by custom trait PropertyAction[Any] (s. a.)
  *
  * @author martin.gerlach
  */
object Offer {

  type Type = Map[String, PropertyAction[Any]]

  def create = Map[String, PropertyAction[Any]]()

  def sku(offer: Type) = new SimpleAccess[String](offer, "sku")

  def brand(offer: Type) = new SimpleAccess[String](offer, "brand")

  def title(offer: Type) = new ContextualSimpleAccess[String](offer, "title")

  def categoryPaths(offer: Type) = new ListAccess[String](offer, "categoryPaths")

  // reflective access - cannot parameterize the all map with type "[String, Type => Access[Any, Any, Any]]" as this would
  // require the Access type params to be covariant (i.e. Access[+T, +K1, +V]), making the "...Generic" function signatures
  // rather complex. So we use a private map with "Any" values and a public accessor using a type cast.

  // TODO use reflection/annotations for genericAccessFor?

  private val all: Map[String, Any] = Map("sku" -> sku _, "brand" -> brand _, "categoryPaths" -> categoryPaths _, "title" -> title _)

  def genericAccessFor[T, K1, V](offer: Type, k0: String) = all(k0).asInstanceOf[Type => Access[T, K1, V]](offer)

  sealed abstract class Access[T, K1, V](offer: Type, k0: String) {

    // creates new lists or maps
    def setGeneric(c: Option[Context], k1: Option[K1])(v: V): Type

    // adds to lists or maps
    def addGeneric(c: Option[Context], k1: Option[K1])(v: V): Type

    // remove from inner maps, (if map is empty afterwards and for simple and lists == reset)
    def removeGeneric(c: Option[Context], k1: Option[K1]): Type

    //    // exact get
    //    def getGeneric(c: Option[Context], k1: Option[K1]): Option[V]
    //
    //    // moves up the context hierarchy if contextual, otherwise == get
    //    def findGeneric(c: Option[Context], k1: Option[K1]): Option[V]

    // remove k0 from map, meaning to completely ignore the key for PUT to OS
    def ignore: Type = offer - k0

    // add empty structure for k0 to indicate reset in OS
    def reset: Type = offer updated(k0, ResetProperty)

    def propertyAction: Option[PropertyAction[T]] = offer.get(k0).asInstanceOf[Option[PropertyAction[T]]]

    protected def topLevelOrElse(initTopLevel: => T): T = propertyAction match {
      case None | Some(ResetProperty) => initTopLevel
      case Some(SetProperty(topLevel)) => topLevel
    }

    // TODO

    // filter => where to store filterReasons/Details?

    // map essentially loops through all values, possibly replacing them
  }

  class SimpleAccess[V](offer: Type, k0: String) extends Access[V, Nothing, V](offer, k0) {

    def set(v: V): Type = setGeneric(None, None)(v)

    def remove: Type = removeGeneric(None, None)

    //    def get: Option[V] = getGeneric(None, None)

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = offer updated(k0, SetProperty(v))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = setGeneric(c, k1)(v)

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Type = reset

    //    def getGeneric(c: Option[Context], k1: Option[Nothing]): Option[V] = getTopLevel
    //
    //    def findGeneric(c: Option[Context], k1: Option[Nothing]): Option[V] = getGeneric(c, k1)

  }

  class ListAccess[V](offer: Type, k0: String) extends Access[List[V], Nothing, V](offer, k0) {

    def set(v: V): Type = setGeneric(None, None)(v)

    def add(v: V): Type = addGeneric(None, None)(v)

    def remove: Type = removeGeneric(None, None)

    //    def get: Option[V] = getGeneric(None, None)

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = offer updated(k0, SetProperty(List(v)))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = offer updated(k0, SetProperty(topLevelOrElse(Nil) :+ v))

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Type = reset
  }


  class ContextualSimpleAccess[V](offer: Type, k0: String) extends Access[Map[Context, V], Nothing, V](offer, k0) {

    def set(c: Context)(v: V): Type = setGeneric(Option(c), None)(v)

    def add(c: Context)(v: V): Type = addGeneric(Option(c), None)(v)

    def remove(c: Context): Type = removeGeneric(Option(c), None)

    //    def get(c: Context): Option[V] = getGeneric(Option(c), None)
    //
    //    def find(c: Context): Option[V] = findGeneric(Option(c), None)

    def setGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = offer updated(k0, SetProperty(Map[Context, V](c.getOrElse(Global) -> v)))

    def addGeneric(c: Option[Context], k1: Option[Nothing])(v: V): Type = offer updated(k0, SetProperty(topLevelOrElse(Map()) + (c.getOrElse(Global) -> v)))

    def removeGeneric(c: Option[Context], k1: Option[Nothing]): Type = propertyAction match {
      case None => reset
      case Some(ResetProperty) => offer
      case Some(SetProperty(contextMap)) =>
        val updatedMap = contextMap - c.getOrElse(Global)
        if (updatedMap.isEmpty) reset else offer updated(k0, SetProperty(updatedMap))
    }
  }

}