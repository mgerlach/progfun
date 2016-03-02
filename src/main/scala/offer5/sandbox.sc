import de.idealo.services.core.bean.PaymentMethod._
import de.idealo.services.core.config.ContextRegistryConfiguration
import offer5.{Json, Offer}
val contextRegistry = new ContextRegistryConfiguration().contextRegistry
val Global = contextRegistry.getGlobal
val DE = contextRegistry.getContext("DE")
val o1 = Offer.create
val o2 = o1.sku.accept("SKU")
val o3 = o2.title(DE).accept("Titel").title(Global).accept("title_g")
// val o3s = o2.title("DE").accept("Titel").title("0").accept("title")
val o4 = o3.brand.clear
Json.serialize(o3)
Json.serialize(o4)
print("----")
val o6 = o4.acceptRaw("title")(List("DE_"))("title")
print("----")
val o7 = o6.acceptRaw("categoryPaths")()("C1").categoryPath.accept("C2")
print("----")
o7.acceptRaw("price")_
val o8 = o7.acceptRaw("price")(List("DE"))("20")
print("----")
o8.image(DE)
print("----")
val o8a = o8.image(DE).accept("img1").image(DE).accept("img2").image(Global).accept("imgG")
val o8b = o8a.acceptRaw("shippingCosts")(List("DE", "cod"))("2.50")
val o8c = o8b.shippingComponent(DE, pp).accept(100)
  .shippingComponent(DE, pp).accept(200)
  .shippingComponents(Global, pp).accept(List(300, 400))
val o9 = o8c.acceptRaw("attributes")(List("bla"))("blubb")
  .acceptRaw("attributes")(List("bla"))("blubber")
  .acceptRaw("attributes")(List("foo"))("bar")
val ser9 = Json.serialize(o9.sumUpShippingComponents())
val o10 = Json.deserialize[Offer](ser9)
val ser10 = Json.serialize(o10)
assert(ser9 == ser10, "ser -> deser -> ser should not change")
o10.sku.latest
o10.latest("sku")()
o10.latest("shippingCosts")(Seq("DE", "pp"))
o10.attributes("foo").latest
o10.latest("attributes")(Seq("foo"))

//val o4 = o3.categoryPath.accept("C1")
//val o5 = o4.categoryPath.accept("C2")
//val o6 = o5.image(Global).accept("http://image1_0")
//  .image(Global).accept("http://image2_0")
//  .image(DE).accept("http://image1_DE")
//val consumeImages: (Option[Context], Option[Any]) => String => Offer =
//  o6.acceptRaw("images")
//val o7 = consumeImages(Option(DE), None)("NochnBild")
//val o8 = o7.acceptRaw("price")(Option(DE), None)("100")
//// map multiple raw values (csv cells) to shipping components for one context and payment method
//val o9 = o8.shippingComponent(DE, pp).accept(100)
//  .shippingComponent(DE, pp).accept(200)
//  .shippingComponents(Global, pp).accept(List(300, 400))
//// add up multiple mappings for one context and payment method to one amount
//// specific context/paym
//val o10 = o9.shippingComponents(DE, pp).latest match {
//  case None => o9
//  case Some(shippingComponents) =>
//    o9.shippingCosts(DE, pp).accept(shippingComponents.sum)
//}
//val o11 = o9.sumUpShippingComponents(false)
//val o12 = o11.attribute("Color").accept("red").attribute("Color").accept("green")
//val o13 = o12.acceptRaw("shippingCosts")(Option(DE), Option(cod))("1.99")

