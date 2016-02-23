package offer4

import context.{Context, Global}

import scala.util.Try

// TODO replace R, W by V and use Option[V] as return value for latest()

trait Access[T, R, W] {

  /**
    * @return the most recent value passed to consume(W), if any.
    */
  def latest: R

  /**
    * Consumes a value by adding it to the underlying data structure of the current T
    * @param v value
    * @return new T containing v
    */
  def consume(v: W): T
}

case class MapAccess[K, V](m: Map[K, V], k: K) extends Access[Map[K, V], Option[V], V] {

  def latest = m.get(k)

  def consume(v: V) = m updated(k, v)
}

case class ListAccess[V](l: List[V]) extends Access[List[V], Option[V], V] {

  def latest = Try(Option(l.last)).getOrElse(None) // return last, no/empty list yields in None

  def consume(v: V) = l :+ v // append
}

// TODO if we are more relaxed about types, can the composites be condensed into some function using recursion?

case class CompositeAccess2[T, R1, W1, R2, W2](a1: Access[T, R1, W1], next: Access[T, R1, W1] => Access[W1, R2, W2]) extends Access[T, R2, W2] {
  lazy val a2: Access[W1, R2, W2] = next(a1)

  def latest = a2.latest

  def consume(v: W2) = a1.consume(a2.consume(v))
}

case class CompositeAccess3[T, R1, W1, R2, W2, R3, W3](a1: Access[T, R1, W1], next1: Access[T, R1, W1] => Access[W1, R2, W2], next2: Access[W1, R2, W2] => Access[W2, R3, W3]) extends Access[T, R3, W3] {

  lazy val a2: Access[W1, R2, W2] = next1(a1)

  lazy val a3: Access[W2, R3, W3] = next2(a2)

  def latest = a3.latest

  def consume(v: W3) = a1.consume(a2.consume(a3.consume(v)))
}

/**
  * Another OO approach, concentrating on composing accessor/modification functions
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  lazy val sku = TopLevelAccess[String]("sku")

  lazy val title = TopLevelAccess[Map[Context, String]]("title")

  // TODO define the composites as types

  def title(c: Context): Access[Offer, Option[String], String] =
    CompositeAccess2[Offer, Option[Map[Context, String]], Map[Context, String], Option[String], String](
      title,
      tla => MapAccess[Context, String](tla.latest.getOrElse(Map[Context, String]()), c))

  lazy val categoryPaths = TopLevelAccess[List[String]]("categoryPaths")

  lazy val categoryPath: Access[Offer, Option[String], String] =
    CompositeAccess2[Offer, Option[List[String]], List[String], Option[String], String](
      categoryPaths,
      tla => ListAccess[String](tla.latest.getOrElse(Nil)))

  lazy val images = TopLevelAccess[Map[Context, List[String]]]("images")

  def images(c: Context): Access[Offer, Option[List[String]], List[String]] =
    CompositeAccess2[Offer, Option[Map[Context, List[String]]], Map[Context, List[String]], Option[List[String]], List[String]](
      images,
      tla => MapAccess[Context, List[String]](tla.latest.getOrElse(Map[Context, List[String]]()), c))

  def image(c: Context): Access[Offer, Option[String], String] =
    CompositeAccess3[Offer, Option[Map[Context, List[String]]], Map[Context, List[String]], Option[List[String]], List[String], Option[String], String](
      images,
      tla => MapAccess[Context, List[String]](tla.latest.getOrElse(Map[Context, List[String]]()), c),
      a2 => ListAccess[String](a2.latest.getOrElse(Nil)))

  // GENERIC ACCESS (e.g., CSV mapping) for sequentially adding single values

  // TODO add optional map key as second function param
  // TODO this is for strings (CSV mapping only) -> how can we add conversion functions, e.g. for prices

  lazy val all: Map[String, Option[Context] => Any] = Map(
    sku.k -> (c => sku),
    title.k -> (c => title(c.getOrElse(Global))),
    categoryPaths.k -> (c => categoryPath),
    images.k -> (c => image(c.getOrElse(Global))))

  def genericAccessFor[T](k: String)(c: Option[Context]) = all(k)(c).asInstanceOf[Access[Offer, Option[String], String]]

  case class TopLevelAccess[T](k: String) extends Access[Offer, Option[T], T] {

    /**
      * Returns an Option containing the most recent value passed to consume(T), if any.
      * A return value of None (Option) can mean that nothing has been set OR clear() was called to clear the property in OS.
      * Use isClear() to find out (more explicit and nicer with composite Access objects than working with Optional of Optional).
      *
      * @return latest value for our key (offer property), or None if the property is to be ignored or cleared (use isClear() to find out)
      */
    def latest = m.getOrElse(k, None).asInstanceOf[Option[T]]

    def consume(v: T) = Offer(m updated(k, Option(v))) // outer Option indicates modification of property, inner indicates that a value is set

    def clear = Offer(m updated(k, None))

    // check for k -> None (indicated by Some(None) returned by m.get(k)) which indicates to clear the property in OS
    def isClear = m.get(k).exists(o => o.isEmpty)

    def remove = Offer(m - k) // if the complete binding is remove, the property is ignored in OS
  }

}

object Offer {
  def create = new Offer()
}

