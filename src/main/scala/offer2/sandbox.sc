import context.{DE, Global}
import offer2.Offer
import offer2.Offer._

val o2_0: Offer.Type = Offer.create

val o2_1 = sku(o2_0).set("test")
sku(o2_1).propertyAction

val o2_2 = brand(o2_1).reset
brand(o2_2).propertyAction

// unsafe!
val o2_3 = genericAccessFor(o2_0, "sku").setGeneric(None, None)(0)
val o2_10 = categoryPaths(o2_2).add("c1")
val o2_11 = categoryPaths(o2_10).set("c2")
val o2_12 = categoryPaths(o2_11).add("c3")
val o2_4 = title(o2_12).add(DE)("titel")
val o2_5 = title(o2_4).set(Global)("title")
//title(o2_5).get(Global)
//title(o2_5).get(DE)
//title(o2_5).find(Global)
//title(o2_5).find(DE)
val o2_6 = title(o2_5).add(DE)("Titel")