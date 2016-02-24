package offer4

import context.{Context, Global}

import scala.util.Try

trait Access[T, V] {

  /**
    * @return the most recent value passed to accept(V), if any.
    */
  def latest: Option[V]

  /**
    * Consumes a value by adding it to the underlying data structure of the current T
    *
    * @param v value
    * @return new T containing v
    */
  def accept(v: V): T
}

case class MapAccess[K, V](m: Map[K, V], k: K) extends Access[Map[K, V], V] {

  def latest = m.get(k)

  def accept(v: V) = m updated(k, v)
}

case class ListAccess[V](l: List[V]) extends Access[List[V], V] {

  def latest = Try(Option(l.last)).getOrElse(None) // return last, no/empty list yields in None

  def accept(v: V) = l :+ v // append
}

case class CompositeAccess[T, V1, V2](a1: Access[T, V1], a2: Access[V1, V2]) extends Access[T, V2] {

  def latest = a2.latest

  def accept(v: V2) = a1.accept(a2.accept(v))
}

/**
  * Another OO approach, concentrating on composing accessor/modification functions
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  lazy val sku = TopLevelAccess[String]("sku")

  lazy val title = TopLevelAccess[Map[Context, String]]("title")

  def title(c: Context): Access[Offer, String] =
    CompositeAccess(title, MapAccess(title.latest.getOrElse(Map.empty), c))

  lazy val categoryPaths = TopLevelAccess[List[String]]("categoryPaths")

  lazy val categoryPath: Access[Offer, String] =
    CompositeAccess(categoryPaths, ListAccess(categoryPaths.latest.getOrElse(Nil)))

  lazy val images = TopLevelAccess[Map[Context, List[String]]]("images")

  def images(c: Context): Access[Offer, List[String]] =
    CompositeAccess(images, MapAccess(images.latest.getOrElse(Map.empty), c))

  def image(c: Context): Access[Offer, String] = {
    val ctxImages = images(c)
    CompositeAccess(ctxImages, ListAccess(ctxImages.latest.getOrElse(Nil)))
  }

  lazy val price = TopLevelAccess[Map[Context, Int]]("price")

  def price(c: Context): Access[Offer, Int] =
    CompositeAccess(price, MapAccess(price.latest.getOrElse(Map.empty), c))

  // GENERIC ACCESS (e.g., CSV mapping) for sequentially adding single values

  // TODO add optional map key as second function param

  // TODO can we use annotations on the vals/defs above and scan those for necessary params and string converters
  // (to build the accessors map/function, 

  private lazy val accessors: String => (Option[Context] => Any) = Map(
    sku.k -> (c => sku),
    title.k -> (c => title(c.getOrElse(Global))),
    categoryPaths.k -> (c => categoryPath),
    images.k -> (c => image(c.getOrElse(Global))),
    price.k -> (c => price(c.getOrElse(Global)))
  )

  private def accessor(k: String)(c: Option[Context]) = accessors(k)(c).asInstanceOf[Access[Offer, Any]]

  def acceptRaw: String => ((Option[Context], String) => Offer) = Map(
    price.k -> ((c: Option[Context], v: String) => Try((v.toDouble * 100).toInt).map(i => accessor(price.k)(c).accept(i)).getOrElse {
      println("Price conversion error: " + v)
      this // ignore invalid price strings, return unmodified offer
    })
  ).withDefault(k => (c, v) => accessor(k)(c).accept(v)) // default taking strings without conversion

  def latest(k: String)(c: Option[Context]): Option[Any] = accessor(k)(c).latest

  case class TopLevelAccess[V](k: String) extends Access[Offer, V] {

    /**
      * Returns an Option containing the most recent value passed to consume(T), if any.
      * A return value of None (Option) can mean that nothing has been set OR clear() was called to clear the property in OS.
      * Use isClear() to find out (more explicit and nicer with composite Access objects than working with Optional of Optional).
      *
      * @return latest value for our key (offer property), or None if the property is to be ignored or cleared (use isClear() to find out)
      */
    def latest = m.getOrElse(k, None).asInstanceOf[Option[V]]

    def accept(v: V) = Offer(m updated(k, Option(v))) // outer Option indicates modification of property, inner indicates that a value is set

    def clear = Offer(m updated(k, None))

    // check for k -> None (indicated by Some(None) returned by m.get(k)) which indicates to clear the property in OS
    def isClear = m.get(k).exists(o => o.isEmpty)

    def remove = Offer(m - k) // if the complete binding is removed, the property is ignored in OS
  }

}

object Offer {
  def create = new Offer()
}

