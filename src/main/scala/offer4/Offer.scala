package offer4

import context.{Context, Global}
import paymentmethod.PaymentMethod

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

  lazy val shippingCosts = TopLevelAccess[Map[Context, Map[PaymentMethod, Int]]]("shippingCosts")

  def shippingCosts(c: Context): Access[Offer, Map[PaymentMethod, Int]] = CompositeAccess(shippingCosts, MapAccess(shippingCosts.latest.getOrElse(Map.empty), c))

  def shippingCosts(c: Context, p: PaymentMethod): Access[Offer, Int] = {
    val ctxShippingCosts = shippingCosts(c)
    CompositeAccess(ctxShippingCosts, MapAccess(ctxShippingCosts.latest.getOrElse(Map.empty), p))
  }

  lazy val shippingComponents = TopLevelAccess[Map[Context, Map[PaymentMethod, List[Int]]]]("shippingComponents")

  def shippingComponents(c: Context): Access[Offer, Map[PaymentMethod, List[Int]]] = CompositeAccess(shippingComponents, MapAccess(shippingComponents.latest.getOrElse(Map.empty), c))

  def shippingComponents(c: Context, p: PaymentMethod): Access[Offer, List[Int]] = {
    val ctxShippingComponents = shippingComponents(c)
    CompositeAccess(ctxShippingComponents, MapAccess(ctxShippingComponents.latest.getOrElse(Map.empty), p))
  }

  def shippingComponent(c: Context, p: PaymentMethod): Access[Offer, Int] = {
    val ctxPaymShippingComponents = shippingComponents(c, p)
    CompositeAccess(ctxPaymShippingComponents, ListAccess(ctxPaymShippingComponents.latest.getOrElse(Nil)))
  }

  lazy val attributes = TopLevelAccess[Map[String, List[String]]]("attributes")

  def attributes(a: String): Access[Offer, List[String]] = CompositeAccess(attributes, MapAccess(attributes.latest.getOrElse(Map.empty), a))

  def attribute(a: String): Access[Offer, String] = {
    val attrs = attributes(a)
    CompositeAccess(attrs, ListAccess(attrs.latest.getOrElse(Nil)))
  }

  // GENERIC ACCESS (e.g., CSV mapping) for sequentially adding single values

  // TODO can we use annotations on the vals/defs above and scan those for necessary params and string converters
  // (to build the accessors map/function, and converting from Strings to values and keys)

  private lazy val accessors: String => ((Option[Context], Option[Any]) => Any) = Map(
    sku.k -> ((c, k1) => sku),
    title.k -> ((c, k1) => title(c.getOrElse(Global))),
    categoryPaths.k -> ((c, k1) => categoryPath),
    images.k -> ((c, k1) => image(c.getOrElse(Global))),
    price.k -> ((c, k1) => price(c.getOrElse(Global))),
    attributes.k -> ((c, a) => attribute(a.get.asInstanceOf[String])), // TODO what about the nse/cc exceptions? Better drop the values? How? Nop-Accessor with Try?
    shippingComponents.k -> ((c, pm) => shippingComponent(c.getOrElse(Global), pm.get.asInstanceOf[PaymentMethod])), // s.a.
    shippingCosts.k -> ((c, pm) => shippingCosts(c.getOrElse(Global), pm.get.asInstanceOf[PaymentMethod])) // s.a.
  )

  private def accessor(k: String)(c: Option[Context], k1: Option[Any]) = accessors(k)(c, k1).asInstanceOf[Access[Offer, Any]]

  def acceptRaw: String => ((Option[Context], Option[Any]) => String => Offer) = Map(
    // TODO factor out price conversion stuff / or turn into a real function treating price, shippingXXX in the same way
    price.k -> ((c: Option[Context], k1: Option[Any]) => (v: String) => Try((v.toDouble * 100).toInt).map(i => accessor(price.k)(c, k1).accept(i)).getOrElse {
      println("Price conversion error: " + v)
      this // ignore invalid price strings, return unmodified offer
    })

  ).withDefault(k => (c: Option[Context], k1: Option[Any]) => (v: String) => accessor(k)(c, k1).accept(v)) // default taking strings without conversion

  def latest(k: String)(c: Option[Context], k1: Option[Any]): Option[Any] = accessor(k)(c, k1).latest

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

