import de.idealo.services.core.bean.PaymentMethod._
import de.idealo.services.core.config.ContextRegistryConfiguration
import de.idealo.services.core.context._
import offer4.{Json, Offer}

import scala.util.Try
val contextRegistry = new ContextRegistryConfiguration().contextRegistry
val Global = contextRegistry.getGlobal
val DE = contextRegistry.getContext("DE")
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
val consumeImages: (Option[Context], Option[Any]) => String => Offer =
  o6.acceptRaw("images")
val o7 = consumeImages(Option(DE), None)("NochnBild")
val o8 = o7.acceptRaw("price")(Option(DE), None)("100")
o8.price(DE).latest.map(x => x.getClass)
o8.latest("price")(Option(DE), None).exists(o => o.isInstanceOf[Int])
o8.acceptRaw("price")(Option(Global), None)("xxx")
o8.latest("sku")()
// map multiple raw values (csv cells) to shipping components for one context and payment method
val o9 = o8.shippingComponent(DE, pp).accept(100)
  .shippingComponent(DE, pp).accept(200)
  .shippingComponents(Global, pp).accept(List(300, 400))
// add up multiple mappings for one context and payment method to one amount
// specific context/paym
val o10 = o9.shippingComponents(DE, pp).latest match {
  case None => o9
  case Some(shippingComponents) =>
    o9.shippingCosts(DE, pp).accept(shippingComponents.sum)
}
val o11 = o9.sumUpShippingComponents(false)
val o12 = o11.attribute("Color").accept("red").attribute("Color").accept("green")
// generic
o12.latest("shippingCosts")(Option(DE), Option(pp))
val o13 = o12.acceptRaw("shippingCosts")(Option(DE), Option(cod))("1.99")
o13.latest("shippingCosts")(Option(DE), Option(cod)).contains(199)
// default params for ctx and m
o13.acceptRaw("sku")()("newSKU")
o13.acceptRaw("title")(c = Option(DE))("NEUER DE TITEL")
o13.acceptRaw("attributes")(m = Option("x"))("attr")
// wrong use of m
o13.acceptRaw("attributes")(Option(DE), Option(1.0))("1.23")
o13.acceptRaw("shippingCosts")(Option(Global), Option(1.0))("1.99")
o13.acceptRaw("shippingComponents")(Option(Global), None)("1.99")
Json.serialize(o13)
Json.deserialize[Offer]("{\"categoryPaths\":[\"C1\",\"C2\"],\"sku\":\"SKU\",\"shippingCosts\":{\"DE\":{\"pp\":300,\"cod\":199},\"0\":{\"pp\":700}},\"price\":{\"DE\":10000},\"attributes\":{\"Color\":[\"red\",\"green\"]},\"shippingComponents\":{\"DE\":{\"pp\":[100,200]},\"0\":{\"pp\":[300,400]}},\"title\":{\"DE\":\"Titel\",\"0\":\"title\"},\"images\":{\"0\":[\"http://image1_0\",\"http://image2_0\"],\"DE\":[\"http://image1_DE\",\"NochnBild\"]}}")