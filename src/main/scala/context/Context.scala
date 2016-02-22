package context

import scala.annotation.tailrec

sealed trait Context {
  def name: String

  def parent: Context

  override def toString = name
}

object Context {
  @tailrec
  def findContextValue[T](contextMap: Map[Context, T], c: Context): Option[T] = {
    contextMap.get(c) match {
      case None => if (c == Global) None else findContextValue(contextMap, c.parent)
      case Some(t) => Some(t)
    }
  }
}

case object Global extends Context {
  val name = "0"

  def parent = throw new NoSuchElementException("0.parent")
}

case object DE extends Context {
  val name = "DE"
  val parent = Global
}
