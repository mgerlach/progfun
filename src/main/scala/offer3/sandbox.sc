import context.{DE, Global}
import offer3.Offer

val o2_0: Offer = new Offer

val o2_1 = o2_0.sku.set("test")
o2_1.sku.propertyOption // None => ignore, Some => set/reset
o2_1.sku.get            // None => ignore or reset, Some => set

val o2_2 = o2_1.brand.reset
o2_2.brand.propertyOption
o2_2.brand.get

// unsafe!
val o2_3 = o2_0.genericAccessFor("sku").setGeneric(None, None)(0)
o2_2.categoryPaths.get(0)
val o2_10 = o2_2.categoryPaths.add("c1")
val o2_11 = o2_10.categoryPaths.set("c2")
val o2_12 = o2_11.categoryPaths.add("c3")
o2_12.categoryPaths.get(l => l.head)
o2_12.categoryPaths.get(l => l(1))
o2_12.categoryPaths.get(2) // Exception => None
val o2_4 = o2_12.title.add(DE)("titel")
val o2_5 = o2_4.title.set(Global)("title")
o2_5.title.get(Global)
o2_5.title.get(DE)
//title(o2_5).find(Global)
//title(o2_5).find(DE)
val o2_6 = o2_5.title.add(DE)("Titel")