package offer5

import de.idealo.services.core.bean.PaymentMethod
import de.idealo.services.core.config.ContextRegistryConfiguration
import de.idealo.services.core.context.Context

import scala.util.Try

/**
  * Property access API.
  *
  * @tparam T type of class owning the property represented by the concrete Access instance
  * @tparam V type of property values at the nesting level of the represented property
  */
sealed trait Access[T, V] {

  /**
    * @return the most recent value passed to accept(V), if any; in case of lists, the last list element (or None if no or empty list).
    */
  def latest: Option[V]

  /**
    * Consumes a value by adding it to the underlying data structure of the current T; in case of lists, the value is appended to the list.
    *
    * @param v value
    * @return new T containing v
    */
  def accept(v: V): T

  // acceptJsonNode(?)
  // acceptRaw(String) -> on conv error, do not change the objects (Try(...).getOrElse(...) / .recover { case ...})
}

/**
  * Access to maps.
  *
  * @param m underlying map
  * @param k key of binding to be read/manipulated by the MapAccess instance
  * @tparam K type of map keys
  * @tparam V type of map values
  */
abstract class MapAccess[K, V](m: Map[String, V], k: K, toKeyString: K => String = (k: Any) => k.toString) extends Access[Map[String, V], V] {

  private val keyString: String = toKeyString(k)

  def latest = m.get(keyString)

  def accept(v: V) = m updated(keyString, v)
}

case class StringMapAccess[V](m: Map[String, V], s: String) extends MapAccess[String, V](m, s, s => s)

case class ContextMapAccess[V](m: Map[String, V], c: Context) extends MapAccess[Context, V](m, c, c => c.getName)

case class PaymentMethodMapAccess[V](m: Map[String, V], p: PaymentMethod) extends MapAccess[PaymentMethod, V](m, p, p => p.name)

/**
  * Access to lists. Mind that latest() will always return the last list element, if the list is non-empty
  *
  * @param l underlying list (must not be null)
  * @tparam V type of list elements
  */
case class ListAccess[V](l: List[V]) extends Access[List[V], V] {

  def latest = if (l.nonEmpty) Option(l.last) else None

  def accept(v: V) = l :+ v // append
}

/**
  * Composes two Access instances for use with complex properties. E.g., for a Map[K, List[V] ], a MapAccess instance
  * and a ListAccess instance can be combined to form an Access instance accepting single list elements and inserting them,
  * using the ListAccess instance, into the list bound to the map key defined by the MapAccess instance.
  *
  * @param a1 high level access
  * @param a2 low level access
  * @tparam T  type of class owning the property represented by the concrete Access instance
  * @tparam V1 low level type at a1's level - and high level type at a2's level
  * @tparam V2 low level type at a2's level - and value type for the composite
  */
case class CompositeAccess[T, V1, V2](a1: Access[T, V1], a2: Access[V1, V2]) extends Access[T, V2] {

  def latest = a2.latest

  def accept(v: V2) = a1.accept(a2.accept(v))
}

/**
  * Offer built on a map, where the values are wrapped in Option instances. No map binding means the offer property
  * represented by the map key will not be modified by OS. A map binding with an empty optional means the offer property
  * will be cleared by OS.
  *
  * @param m the map of offer propery options representing the offer
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  lazy val brand = TopLevelAccess[String]("brand")

  lazy val sku = TopLevelAccess[String]("sku")

  lazy val title = TopLevelAccess[Map[String, String]]("title")

  def title(c: Context): Access[Offer, String] =
    new CompositeAccess(title, ContextMapAccess(title.latest.getOrElse(Map.empty), c))

  lazy val categoryPaths = TopLevelAccess[List[String]]("categoryPaths")

  lazy val categoryPath: Access[Offer, String] =
    CompositeAccess(categoryPaths, ListAccess(categoryPaths.latest.getOrElse(Nil)))

  //  lazy val images = TopLevelAccess[Map[Context, List[String]]]("images")
  //
  //  def images(c: Context): Access[Offer, List[String]] =
  //    CompositeAccess(images, MapAccess(images.latest.getOrElse(Map.empty), c))
  //
  //  def image(c: Context): Access[Offer, String] = {
  //    val ctxImages = images(c)
  //    CompositeAccess(ctxImages, ListAccess(ctxImages.latest.getOrElse(Nil)))
  //  }
  //
  lazy val price = TopLevelAccess[Map[String, Int]]("price")

  def price(c: Context): Access[Offer, Int] =
    CompositeAccess(price, ContextMapAccess(price.latest.getOrElse(Map.empty), c))

  //
  //  lazy val shippingCosts = TopLevelAccess[Map[Context, Map[PaymentMethod, Int]]]("shippingCosts")
  //
  //  def shippingCosts(c: Context): Access[Offer, Map[PaymentMethod, Int]] =
  //    CompositeAccess(shippingCosts, MapAccess(shippingCosts.latest.getOrElse(Map.empty), c))
  //
  //  def shippingCosts(c: Context, p: PaymentMethod): Access[Offer, Int] = {
  //    val ctxShippingCosts = shippingCosts(c)
  //    CompositeAccess(ctxShippingCosts, MapAccess(ctxShippingCosts.latest.getOrElse(Map.empty), p))
  //  }
  //
  //  lazy val shippingComponents = TopLevelAccess[Map[Context, Map[PaymentMethod, List[Int]]]]("shippingComponents")
  //
  //  def shippingComponents(c: Context): Access[Offer, Map[PaymentMethod, List[Int]]] =
  //    CompositeAccess(shippingComponents, MapAccess(shippingComponents.latest.getOrElse(Map.empty), c))
  //
  //  def shippingComponents(c: Context, p: PaymentMethod): Access[Offer, List[Int]] = {
  //    val ctxShippingComponents = shippingComponents(c)
  //    CompositeAccess(ctxShippingComponents, MapAccess(ctxShippingComponents.latest.getOrElse(Map.empty), p))
  //  }
  //
  //  def shippingComponent(c: Context, p: PaymentMethod): Access[Offer, Int] = {
  //    val ctxPaymShippingComponents = shippingComponents(c, p)
  //    CompositeAccess(ctxPaymShippingComponents, ListAccess(ctxPaymShippingComponents.latest.getOrElse(Nil)))
  //  }
  //
  //  lazy val attributes = TopLevelAccess[Map[String, List[String]]]("attributes")
  //
  //  def attributes(a: String): Access[Offer, List[String]] =
  //    CompositeAccess(attributes, StringMapAccess(attributes.latest.getOrElse(Map.empty), a))
  //
  //  def attribute(a: String): Access[Offer, String] = {
  //    val attrs = attributes(a)
  //    CompositeAccess(attrs, ListAccess(attrs.latest.getOrElse(Nil)))
  //  }

  // GENERIC ACCESS (e.g., CSV mapping) for sequentially adding single values
  // Always need k (offer property name or key), c (context, optional, default none),
  // m (map key, e.g. String (attribute name) or PaymentMethod (shipping), with runtime type checking)

  def acceptRaw(k: String)(c: Option[String] = Option(defaultContext.getName), m: Option[String] = None)(v: String): Offer =
    k match {
      case pricePropertiesRE() => acceptWithPriceConversion(k)(c.flatMap(cs => Option(contextRegistry.getContext(cs))), m)(v)
      case _ => accessor(k)(c.flatMap(cs => Option(contextRegistry.getContext(cs))), m).accept(v)
    }

  def latest(k: String)(c: Option[String] = Option(defaultContext.getName), m: Option[String] = None): Option[Any] =
    accessor(k)(c.flatMap(cs => Option(contextRegistry.getContext(cs))), m).latest

  private val contextRegistry = new ContextRegistryConfiguration().contextRegistry

  private val defaultContext = contextRegistry.getGlobal

  private val pricePropertiesRE = s"${price.k}".r //  s"${price.k}|${shippingComponents.k}|${shippingCosts.k}".r

  private def acceptWithPriceConversion(k: String)(c: Option[Context], m: Option[Any])(v: String): Offer =
    Try((v.toDouble * 100).toInt).map(i => accessor(k)(c, m).accept(i)).recover {
      case e: NumberFormatException =>
        println("Price conversion error for field: " + k + ", raw value: " + v + "(" + e + ")")
        this // ignore invalid price strings, return unmodified offer
      case t =>
        throw new IllegalStateException("should not be here...", t)
    }.get

  private def accessor(k: String)(c: Option[Context], m: Option[Any]) =
    accessors(k)(c, m).asInstanceOf[Access[Offer, Any]]

  // TODO can this somehow be compiled with reflection over annotated accessor methods above? Also along with necessary converters? How about the "typed keys"?
  private lazy val accessors: String => ((Option[Context], Option[Any]) => Any) = Map(
    brand.k -> ((c, m) => brand),
    sku.k -> ((c, m) => sku),
    title.k -> ((c, m) => title(c.getOrElse(defaultContext))),
    categoryPaths.k -> ((c, m) => categoryPath),
    //    images.k -> ((c, m) => image(c.getOrElse(defaultContext))),
    price.k -> ((c, m) => price(c.getOrElse(defaultContext)))
    //    attributes.k -> ((c, m) => accessorWithTypedKey[String](m, attribute)),
    //    shippingComponents.k -> ((c, m) => accessorWithTypedKey[PaymentMethod](m, p => shippingComponent(c.getOrElse(defaultContext), p))),
    //    shippingCosts.k -> ((c, m) => accessorWithTypedKey[PaymentMethod](m, p => shippingCosts(c.getOrElse(defaultContext), p)))
  )

  // Option[Any] => M or dummy accessor with warning
  private def accessorWithTypedKey[M](m: Option[Any], accessorMethod: M => Access[Offer, _]): Access[Offer, _] =
    Try(accessorMethod(m.get.asInstanceOf[M])).recover {
      case e: NoSuchElementException =>
        println("Empty map key")
        NoAccess
      case e: ClassCastException =>
        println("Wrong map key type(" + e + ")")
        NoAccess
      case t =>
        throw new IllegalStateException("should not be here...", t)
    }.get

  // special stuff

  //  /**
  //    * Sums up Int lists in shippingComponents structure and stores the sums in shippingCosts for each combination of
  //    * Context and PaymentMethod (replacing any existing bindings for the according keys, keeping bindings with other keys)
  //    *
  //    * @param clearShippingCosts if shippingCosts should be cleared before the operation
  //    * @return new offer with summed up shippingComponents stored in shippingCosts
  //    */
  //  def sumUpShippingComponents(clearShippingCosts: Boolean = true): Offer = {
  //    val o = if (clearShippingCosts) this.shippingCosts.remove else this
  //    o.shippingComponents.latest match {
  //      case None => o
  //      case Some(sComp) => (for {
  //        ctx <- sComp.keys
  //        pm <- sComp(ctx).keys
  //        sum = sComp(ctx)(pm).sum
  //      } yield (ctx, pm, sum))
  //        .foldLeft(o)((oAccumulated, tuple) => tuple match {
  //          case (ctx, pm, sum) => oAccumulated.shippingCosts(ctx, pm).accept(sum)
  //        })
  //    }
  //  }

  /**
    * Access instance ignoring any values passed to accept(), returning the unmodified Offer (Offer.this),
    * and throwing an exception when calling latest().
    */
  private case object NoAccess extends Access[Offer, Any] {

    def accept(v: Any) = Offer.this

    def latest = throw new NoSuchElementException("NoAccess.latest")
  }

  /**
    * Access class for top level offer properties adding methods for clearing and ignoring the properties.
    *
    * @param k top level property name, e.g. title, sku
    * @tparam V top level type, e.g. String, Map[Context, String], List[String].
    */
  case class TopLevelAccess[V](k: String) extends Access[Offer, V] {

    /**
      * The most recent value passed to accept(V), if any.
      * A return value of None (Option) can mean that nothing has been set OR clear() was called to clear the property in OS.
      * Use isClear() to find out (more explicit and nicer with composite Access objects than working with Options of Options).
      *
      * @return latest value for our key (offer property), or None if the property is to be ignored or cleared (use isClear() to find out)
      */
    def latest = m.getOrElse(k, None).asInstanceOf[Option[V]]

    def accept(v: V) = Offer(m updated(k, Option(v)))

    def clear = Offer(m updated(k, None))

    // check for k -> None (indicated by Some(None) returned by m.get(k)) which indicates to clear the property in OS
    def isClear = m.get(k).exists(o => o.isEmpty)

    def remove = Offer(m - k) // if the complete binding is removed, the property is ignored in OS
  }
}

object Offer {
  def create = new Offer()
}
