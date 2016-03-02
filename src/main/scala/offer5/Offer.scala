package offer5

import de.idealo.services.core.bean.PaymentMethod
import de.idealo.services.core.config.ContextRegistryConfiguration
import de.idealo.services.core.context.Context

import scala.annotation.tailrec

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

  /**
    * Consumes a string by adding it to the underlying data structure of the current T; in case of lists, the value is appended to the list.
    * If V is not String, implementations must convert the given String to a V instance.
    *
    * @param vs string value
    * @return new T containing (possibly converted) vs
    */
  def acceptString(vs: String): T
}

/**
  * Access to maps.
  *
  * @param m           underlying map
  * @param k           key of binding to be read/manipulated by the MapAccess instance
  * @param toValueType String to V converter for acceptString(String)
  * @param toKeyString K to String converter used internally to store String keys, default uses toString()
  * @tparam K type of map keys
  * @tparam V type of map values
  */
abstract class MapAccess[K, V](m: Map[String, V], k: K, toValueType: String => V, toKeyString: K => String = (k: Any) => k.toString) extends Access[Map[String, V], V] {

  private val keyString: String = toKeyString(k)

  def latest = m.get(keyString)

  def accept(v: V) = m updated(keyString, v)

  def acceptString(vs: String) = accept(toValueType(vs))
}

// MapAccess classes with concrete key type

case class StringMapAccess[V](m: Map[String, V], s: String,
                              toValueType: String => V = (s: String) => throw new UnsupportedOperationException("M[String, V] String => V"))
  extends MapAccess[String, V](m, s, toValueType)

case class ContextMapAccess[V](m: Map[String, V], c: Context,
                               toValueType: String => V = (s: String) => throw new UnsupportedOperationException("M[Context,V] String => V"))
  extends MapAccess[Context, V](m, c, toValueType, c => c.getName)

case class PaymentMethodMapAccess[V](m: Map[String, V], p: PaymentMethod,
                                     toValueType: String => V = (s: String) => throw new UnsupportedOperationException("M[PaymentMethod, V] String => V"))
  extends MapAccess[PaymentMethod, V](m, p, toValueType, p => p.name)

/**
  * Access to lists. Mind that latest() will always return the last list element, if the list is non-empty
  *
  * @param l           underlying list (must not be null)
  * @param toValueType String to V converter for acceptString(String), default throws UnsupportedOperationException
  * @tparam V type of list elements
  */
case class ListAccess[V](l: List[V], toValueType: String => V = (s: String) => throw new UnsupportedOperationException("L[V] String => V")) extends Access[List[V], V] {

  def latest = if (l.nonEmpty) Option(l.last) else None

  def accept(v: V) = l :+ v // append

  def acceptString(vs: String) = accept(toValueType(vs))
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

  def acceptString(vs: String) = a1.accept(a2.acceptString(vs))
}

/**
  * Offer built on a map, where the values are wrapped in Option instances. No map binding means the offer property
  * represented by the map key will not be modified by OS. A map binding with an empty optional means the offer property
  * will be cleared by OS.
  *
  * @param m the map of offer property options representing the offer
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  // TYPED ACCESS

  lazy val brand = TopLevelAccess[String]("brand", identity)

  lazy val sku = TopLevelAccess[String]("sku", identity)

  lazy val title = TopLevelAccess[Map[String, String]]("title")

  def title(c: Context): Access[Offer, String] =
    new CompositeAccess(title, ContextMapAccess(title.latest.getOrElse(Map.empty), c, identity))

  lazy val categoryPaths = TopLevelAccess[List[String]]("categoryPaths")

  lazy val categoryPath: Access[Offer, String] =
    CompositeAccess(categoryPaths, ListAccess(categoryPaths.latest.getOrElse(Nil), identity))

  lazy val images = TopLevelAccess[Map[String, List[String]]]("images")

  def images(c: Context): Access[Offer, List[String]] =
    CompositeAccess(images, ContextMapAccess(images.latest.getOrElse(Map.empty), c))

  def image(c: Context): Access[Offer, String] = {
    val ctxImages = images(c)
    CompositeAccess(ctxImages, ListAccess(ctxImages.latest.getOrElse(Nil), identity)) // could add URL validation in toValueType func param
  }

  lazy val price = TopLevelAccess[Map[String, Int]]("price")

  def price(c: Context): Access[Offer, Int] =
    CompositeAccess(price, ContextMapAccess(price.latest.getOrElse(Map.empty), c, rawToIntPrice))

  lazy val shippingCosts = TopLevelAccess[Map[String, Map[String, Int]]]("shippingCosts")

  def shippingCosts(c: Context): Access[Offer, Map[String, Int]] =
    CompositeAccess(shippingCosts, ContextMapAccess(shippingCosts.latest.getOrElse(Map.empty), c))

  def shippingCosts(c: Context, p: PaymentMethod): Access[Offer, Int] = {
    val ctxShippingCosts = shippingCosts(c)
    CompositeAccess(ctxShippingCosts, PaymentMethodMapAccess(ctxShippingCosts.latest.getOrElse(Map.empty), p, rawToIntPrice))
  }

  lazy val shippingComponents = TopLevelAccess[Map[String, Map[String, List[Int]]]]("shippingComponents")

  def shippingComponents(c: Context): Access[Offer, Map[String, List[Int]]] =
    CompositeAccess(shippingComponents, ContextMapAccess(shippingComponents.latest.getOrElse(Map.empty), c))

  def shippingComponents(c: Context, p: PaymentMethod): Access[Offer, List[Int]] = {
    val ctxShippingComponents = shippingComponents(c)
    CompositeAccess(ctxShippingComponents, PaymentMethodMapAccess(ctxShippingComponents.latest.getOrElse(Map.empty), p))
  }

  def shippingComponent(c: Context, p: PaymentMethod): Access[Offer, Int] = {
    val ctxPaymShippingComponents = shippingComponents(c, p)
    CompositeAccess(ctxPaymShippingComponents, ListAccess(ctxPaymShippingComponents.latest.getOrElse(Nil), rawToIntPrice))
  }

  lazy val attributes = TopLevelAccess[Map[String, List[String]]]("attributes")

  def attributes(a: String): Access[Offer, List[String]] =
    CompositeAccess(attributes, StringMapAccess(attributes.latest.getOrElse(Map.empty), a))

  def attribute(a: String): Access[Offer, String] = {
    val attrs = attributes(a)
    CompositeAccess(attrs, ListAccess(attrs.latest.getOrElse(Nil), identity))
  }

  // GENERIC (RAW) ACCESS

  def acceptRaw(k: String)(keys: Seq[String] = Nil)(v: Any): Offer = {
    val a = accessor(k)(keys)
    v match {
      case vs: String => a.acceptString(vs) // from CSV
      case _ => a.accept(v) // from typed source like JSON or eBay API, needs to be converted explicitly (e. g. * 100)
    }
  }

  def latest(k: String)(keys: Seq[String] = Nil): Option[Any] =
    accessor(k)(keys).latest

  // TODO more generic, from reflection?
  lazy val topLevel = Map(
    brand.k -> brand,
    sku.k -> sku,
    title.k -> title,
    categoryPaths.k -> categoryPaths,
    images.k -> images,
    price.k -> price,
    shippingCosts.k -> shippingCosts,
    shippingComponents.k -> shippingComponents,
    attributes.k -> attributes)

  private def rawToIntPrice(priceString: String): Int = (priceString.toDouble * 100).toInt

  private lazy val contextRegistry = new ContextRegistryConfiguration().contextRegistry

  private lazy val defaultContext = contextRegistry.getGlobal

  @tailrec
  private def extractTypedKey[K](keys: Seq[String], i: Int, toKeyType: String => K, default: => K): K =
    keys match {
      case Nil => default
      case head :: tail if i <= 0 => Option(toKeyType(head)).getOrElse(default)
      case head :: tail => extractTypedKey(tail, i - 1, toKeyType, default)
    }

  private def extractContext(keys: Seq[String], i: Int): Context =
    extractTypedKey(keys, i, contextRegistry.getContext, defaultContext)

  private def extractPaymentMethod(keys: Seq[String], i: Int): PaymentMethod =
    extractTypedKey(keys, i, PaymentMethod.valueOf, throw new IllegalArgumentException("Cannot extract payment method from pos " + i + " of " + keys))

  private def extractAttribute(keys: Seq[String], i: Int): String =
    extractTypedKey(keys, i, identity, throw new IllegalArgumentException("Cannot extract attribute from pos " + i + " of " + keys))

  private def accessor(k: String)(keys: Seq[String]) = accessors(k)(keys).asInstanceOf[Access[Offer, Any]]

  private lazy val accessors: String => Seq[String] => Any = Map(
    brand.k -> (keys => brand),
    sku.k -> (keys => sku),
    title.k -> (keys => title(extractContext(keys, 0))),
    categoryPaths.k -> (keys => categoryPath),
    images.k -> (keys => image(extractContext(keys, 0))),
    price.k -> (keys => price(extractContext(keys, 0))),
    shippingCosts.k -> (keys => shippingCosts(extractContext(keys, 0), extractPaymentMethod(keys, 1))),
    shippingComponents.k -> (keys => shippingComponent(extractContext(keys, 0), extractPaymentMethod(keys, 1))),
    attributes.k -> (keys => attribute(extractAttribute(keys, 0))))

  // special stuff

  /**
    * Sums up Int lists in shippingComponents structure and stores the sums in shippingCosts for each combination of
    * Context and PaymentMethod (replacing any existing bindings for the according keys, keeping bindings with other keys)
    *
    * @param clearShippingCosts if shippingCosts should be cleared before the operation (default is false)
    * @return new offer with summed up shippingComponents stored in shippingCosts
    */
  def sumUpShippingComponents(clearShippingCosts: Boolean = false): Offer = {
    val o = if (clearShippingCosts) this.shippingCosts.remove else this
    o.shippingComponents.latest match {
      case None => o
      case Some(sComp) => (for {
        ctx <- sComp.keys // keys are Strings for easiser JSON serialization, need to use acceptRaw below
        pm <- sComp(ctx).keys // keys are Strings for easiser JSON serialization, need to use acceptRaw below
        sum = sComp(ctx)(pm).sum
      } yield (ctx, pm, sum))
        .foldLeft(o)((oAccumulated, tuple) => tuple match {
          case (ctx, pm, sum) => oAccumulated.acceptRaw("shippingCosts")(Seq(ctx, pm))(sum)
        })
    }
  }

  /**
    * Access class for top level offer properties adding methods for clearing and ignoring the properties.
    *
    * @param k           top level property name, e.g. title, sku
    * @param toValueType String to V converter, default throws UnsupportedOperationException
    * @tparam V top level type, e.g. String, Map[Context, String], List[String].
    */
  case class TopLevelAccess[V](k: String, toValueType: String => V = (s: String) => throw new UnsupportedOperationException("TopLevel[V] String => V")) extends Access[Offer, V] {

    /**
      * The most recent value passed to accept(V), if any.
      * A return value of None (Option) can mean that nothing has been set OR clear() was called to clear the property in OS.
      * Use isClear() to find out (more explicit and nicer with composite Access objects than working with Options of Options).
      *
      * @return latest value for our key (offer property), or None if the property is to be ignored or cleared (use isClear() to find out)
      */
    def latest = m.getOrElse(k, None).asInstanceOf[Option[V]]

    def accept(v: V) = Offer(m updated(k, Option(v)))

    def acceptString(vs: String) = accept(toValueType(vs))

    def clear = Offer(m updated(k, None))

    // check for k -> None (indicated by Some(None) returned by m.get(k)) which indicates to clear the property in OS
    def isClear = m.get(k).exists(o => o.isEmpty)

    def remove = Offer(m - k) // if the complete binding is removed, the property is ignored in OS
  }

}

object Offer {
  def create = new Offer()
}
