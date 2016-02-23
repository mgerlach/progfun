import context._
import offer4.Offer

import scala.util.Try

val o1 = Offer.create

val o2 = o1.sku.consume("SKU")

o2.categoryPaths.latest
o2.categoryPath.latest
o2.categoryPaths.isClear

val o3 = o2.categoryPath.consume("C1")
o3.categoryPaths.latest
o3.categoryPath.latest
val o4 = o3.categoryPath.consume("C2")
// lists -> only the last element is returned...
o4.categoryPaths.latest
o3.categoryPaths.latest

val o3c = o3.categoryPaths.clear
o3c.categoryPaths.latest
o3c.categoryPaths.isClear
o3c.m.get("categoryPaths")


val o5 = o3.title(DE).consume("Titel").title(Global).consume("title")
o5.title(DE).latest.getOrElse("Meeeeeep!")
Try(o5.TopLevelAccess[Map[Context,String]]("title").latest.get.get(DE).get).getOrElse("Meeeeeep!")
val o6 = o5.image(Global).consume("http://image1_0")
  .image(Global).consume("http://image2_0")
  .image(DE).consume("http://image1_DE")
o6.images.clear
o6.images.remove
o6.images.isClear

Try(o6.images.latest.get).getOrElse("Meeeeeep!")

// generic

val imagesAccess = o6.genericAccessFor[String]("images") _

imagesAccess(Option(DE)).consume("yet another image")