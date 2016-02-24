import context._
import offer4.Offer

import scala.util.Try

val o1 = Offer.create

val o2 = o1.sku.accept("SKU")

o2.categoryPaths.latest
o2.categoryPath.latest
o2.categoryPaths.isClear

val o3 = o2.categoryPath.accept("C1")
o3.categoryPaths.latest
o3.categoryPath.latest
val o4 = o3.categoryPath.accept("C2")
// lists -> only the last element is returned...
o4.categoryPaths.latest
o3.categoryPaths.latest

val o3c = o3.categoryPaths.clear
o3c.categoryPaths.latest
o3c.categoryPaths.isClear
o3c.m.get("categoryPaths")


val o5 = o4.title(DE).accept("Titel").title(Global).accept("title")
o5.title(DE).latest.getOrElse("Meeeeeep!")
Try(o5.TopLevelAccess[Map[Context, String]]("title").latest.get.get(DE).get).getOrElse("Meeeeeep!")
val o6 = o5.image(Global).accept("http://image1_0")
  .image(Global).accept("http://image2_0")
  .image(DE).accept("http://image1_DE")
o6.images.clear
o6.images.remove
o6.images.isClear

Try(o6.images.latest.get).getOrElse("Meeeeeep!")

// generic

val consumeImages: (Option[Context], String) => Offer =
  o6.acceptRaw("images")

val o7 = consumeImages(Option(DE), "NochnBild")

val o8 = o7.acceptRaw("price")(Option(DE), "100")

o8.price(DE).latest.map(x => x.getClass)
o8.latest("price")(Option(DE)).exists(o => o.isInstanceOf[Int])
o8.acceptRaw("price")(Option(Global), "xxx")
o8.latest("sku")(None)
val o10 = Offer.create
val a = o10.title(DE)
val b = o10.title(DE)
val o11 = a.accept("titel")
val o12 = b.accept("xxxxx")
val o13 = o11.title(DE).accept("yyyyy")
