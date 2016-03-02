package offer5

import java.lang.reflect.{ParameterizedType, Type}
import java.util.Map.Entry

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, Version}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.collection.JavaConverters._

/**
  * @author martin.gerlach (Json object inspired by something found on StackOverflow)
  */
object Json {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(new SimpleModule("ImmutableOffer", new Version(0, 0, 1, null, null, null))
    .addSerializer(classOf[Offer], OfferSerializer)
    .addSerializer(classOf[Option[Any]], OptionToNullableMapBindingSerializer)
    .addDeserializer(classOf[Offer], OfferDeserializer))

  def serialize(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def deserialize[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty)
      m.runtimeClass
    else new ParameterizedType {
      def getRawType = m.runtimeClass

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }
}

// for top level properties wrapped in Option, serialized as single-entry map with nullable value binding
object OptionToNullableMapBindingSerializer extends StdSerializer[Option[Any]](classOf[Option[Any]]) {
  def serialize(value: Option[Any], jgen: JsonGenerator, provider: SerializerProvider): Unit =
    jgen.writeObject(value.map(v => Map("value" -> v)).getOrElse(Map("value" -> null)))
}

// serializes just the Offer's data map
object OfferSerializer extends StdSerializer[Offer](classOf[Offer]) {
  def serialize(value: Offer, jgen: JsonGenerator, provider: SerializerProvider): Unit = jgen.writeObject(value.m)
}

// recursively assembles an offer from JSON tree using foldLeft and Offer.acceptRaw(String)(Seq[String])(Any)
object OfferDeserializer extends JsonDeserializer[Offer]() {

  // simple types that we distinguish inside an Offer. Int for prices, String otherwise.
  private def typedNodeValue(n: JsonNode): Any =
    if (n.isInt)
      n.intValue()
    else if (n.isTextual)
      n.textValue()
    else
      throw new IllegalArgumentException("Unexpected JSON node " + n)

  private def mapOfferField(o: Offer, k: String, fieldNode: JsonNode): Offer = {

    // collects map keys until leaf node and build recursive structure of calls to o.acceptRaw(k)(map keys)(leaf node value)
    // for all combinations of map keys and leaf nodes.
    def mapLeafNodes(o: Offer, keys: Seq[String], n: JsonNode): Offer = {
      if (n.isObject)
        n.fields().asScala.foldLeft(o)((oAcc, field: Entry[String, JsonNode]) => mapLeafNodes(oAcc, keys :+ field.getKey, field.getValue))
      else if (n.isArray)
        n.elements().asScala.foldLeft(o)((oAcc, element: JsonNode) => mapLeafNodes(oAcc, keys, element))
      else
        o.acceptRaw(k)(keys)(typedNodeValue(n))
    }

    // Top level node should have a value field, which can be null
    Option(fieldNode.findValue("value")).map(fieldValueNode =>
      if (fieldValueNode.isNull)
        o.topLevel(k).clear // serializes to k: { value: null }
      else
        mapLeafNodes(o, List(), fieldValueNode)
    ).getOrElse(o.topLevel(k).clear) // no value field at top level, should not happen, but treat as null/clear
  }

  def deserialize(p: JsonParser, ctx: DeserializationContext): Offer = {
    val offerNode: JsonNode = p.getCodec.readTree(p)
    // printNode("Offer", offerNode, 0)
    offerNode.fields().asScala.foldLeft(Offer.create)((oAcc, field: Entry[String, JsonNode]) =>
      mapOfferField(oAcc, field.getKey, field.getValue))
  }

  //  private def printNode(name: String, n: JsonNode, depth: Int): Unit = {
  //    for (d <- 0.until(depth)) print(".")
  //    print(name + ": " + n.getNodeType + " = ")
  //    if (n.isArray) {
  //      println("[")
  //      for (elem <- n.elements().asScala) printNode("", elem, depth + 1)
  //      for (d <- 0.until(depth)) print(".")
  //      println("]")
  //    } else {
  //      println(n.asText())
  //      for (child <- n.fields().asScala) printNode(child.getKey, child.getValue, depth + 1)
  //    }
  //  }
}
