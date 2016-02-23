import context._
import offer4.{ListAccess, MapAccess, Offer}

val o1 = Offer.create

val o2 = o1.sku.set("SKU")

val o3 = o2.categoryPaths.set("C1")

val o4 = o3.categoryPaths.set("C2")

o4.categoryPaths.get

o3.categoryPaths.get

val o5 = o4.title(DE).set("Titel").title(Global).set("title")

o5.TopLevelAccess[Map[Context,String]]("title").get.get.get.get(DE).get