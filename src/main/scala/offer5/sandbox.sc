import de.idealo.services.core.config.ContextRegistryConfiguration
import offer5.{Json, Offer}
val contextRegistry = new ContextRegistryConfiguration().contextRegistry
val Global = contextRegistry.getGlobal
val DE = contextRegistry.getContext("DE")
val o1 = Offer.create
val o2 = o1.sku.accept("SKU")
val o3 = o2.title(DE).accept("Titel").title(Global).accept("title")

val o4 = o2.title("DE").accept("Titel").title("0").accept("title")

val o5 = o2.acceptRaw("title")(c = Option("DE"))("Titel")

// TODO npe for unknown context should fall back to default ("0") with warning

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


Json.serialize(o3)
Json.serialize(o4)
Json.deserialize[Offer]("{\"categoryPaths\":[\"C1\",\"C2\"],\"sku\":\"SKU\",\"shippingCosts\":{\"DE\":{\"pp\":300,\"cod\":199},\"0\":{\"pp\":700}},\"price\":{\"DE\":10000},\"attributes\":{\"Color\":[\"red\",\"green\"]},\"shippingComponents\":{\"DE\":{\"pp\":[100,200]},\"0\":{\"pp\":[300,400]}},\"title\":{\"DE\":\"Titel\",\"0\":\"title\"},\"images\":{\"0\":[\"http://image1_0\",\"http://image2_0\"],\"DE\":[\"http://image1_DE\",\"NochnBild\"]}}")