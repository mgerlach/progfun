package offer4

import context.Context

import scala.util.Try

trait Access[T, R, W] {
  def get: R

  def set(v: W): T

  def clear: T

  def remove: T
}

case class MapAccess[K, V](m: Map[K, V], k: K) extends Access[Map[K, V], Option[V], V] {
  def get = m.get(k)

  def set(v: V) = m updated(k, v)

  def clear = Map()

  def remove = m - k
}

case class ListAccess[V](l: List[V]) extends Access[List[V], Option[V], V] {
  def get = Try(Option(l.last)).getOrElse(None)

  def set(v: V) = l :+ v // append

  def clear = Nil

  def remove = Try(l.init).getOrElse(Nil) // remove last, empty stays empty
}

case class CompositeAccess[T, R1, W1, R2, W2](a1: Access[T, R1, W1], inner: Access[T, R1, W1] => Access[W1, R2, W2]) extends Access[T, R2, W2] {
  lazy val a2: Access[W1, R2, W2] = inner(a1)

  def get = a2.get

  def set(v: W2) = a1.set(a2.set(v))

  def clear = a1.clear

  def remove = a1.remove
}

/**
  * Another OO approach, concentrating on composing accessor/modification functions
  */
case class Offer(m: Map[String, Option[Any]]) {

  def this() = this(Map())

  lazy val sku = TopLevelAccess[String]("sku")

  def title(c: Context): Access[Offer, Option[String], String] =
    CompositeAccess[Offer, Option[Option[Map[Context, String]]], Map[Context, String], Option[String], String](
      TopLevelAccess[Map[Context, String]]("title"),
      tla => MapAccess[Context, String](tla.get.getOrElse(Option(Map[Context, String]())).get, c))

  lazy val categoryPaths: Access[Offer, Option[String], String] =
    CompositeAccess[Offer, Option[Option[List[String]]], List[String], Option[String], String](
      TopLevelAccess[List[String]]("categoryPaths"),
      tla => ListAccess[String](tla.get.getOrElse(Option(Nil)).get))

  def images(c: Context): Access[Offer, Option[String], String] =
    CompositeAccess[Offer, Option[Option[Map[Context, List[String]]]], List[String], Option[String], String](
      CompositeAccess[Offer, Option[Option[Map[Context, List[String]]]], Map[Context, List[String]], Option[List[String]], List[String]](
        TopLevelAccess[Map[Context, List[String]]]("images"),
        tla => MapAccess[Context, List[String]](tla.get.getOrElse(Option(Map[Context, List[String]]())).get, c)),
      ca => ListAccess[String](ca.get.getOrElse(Option(Map[Context, List[String]]())).get.getOrElse(c, Nil))
    )

  case class TopLevelAccess[T](k: String) extends Access[Offer, Option[Option[T]], T] {
    def get = m.get(k).asInstanceOf[Option[Option[T]]]

    def set(v: T) = Offer(m updated(k, Option(v)))

    def clear = Offer(m updated(k, None))

    def remove = Offer(m - k)
  }

}

object Offer {
  def create = new Offer()
}

