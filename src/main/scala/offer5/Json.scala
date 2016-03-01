package offer5

import java.lang.reflect.{ParameterizedType, Type}
import java.util.Map.Entry

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, Version}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import de.idealo.services.core.context.Context
import de.idealo.services.core.mapper.ContextSerializer

import scala.collection.JavaConverters._

/**
  * @author martin.gerlach
  */
object Json {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(new SimpleModule("ImmutableOffer", new Version(0, 0, 1, null, null, null))
    .addKeySerializer(classOf[Context], new ContextSerializer())
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

object OfferSerializer extends StdSerializer[Offer](classOf[Offer]) {

  def serialize(value: Offer, jgen: JsonGenerator, provider: SerializerProvider): Unit = jgen.writeObject(value.m)
}

object OfferDeserializer extends JsonDeserializer[Offer]() {


  def deserialize(p: JsonParser, ctxt: DeserializationContext): Offer = {
    val n: JsonNode = p.getCodec.readTree(p)

    // printNode("Offer", n, 0)

    def parseTopLevel(o: Offer, k: String, n: JsonNode): Offer = {

      def iterateMapKeys(o: Offer, n: JsonNode, keys: Seq[String]): Offer = {
        if (n.isObject)
          n.fields().asScala.foldLeft(o)((oAcc, e) => iterateMapKeys(oAcc, e.getValue, keys :+ e.getKey))
        else if (n.isArray)
          n.elements().asScala.foldLeft(o)((oAcc, e) => oAcc.acceptRaw(k)(keys: _*)(e.asText)) // TODO e.asText or typed? Converter? Where?
        else
          o.acceptRaw(k)(keys: _*)(n.asText)
      }

      // Top level
      Option(n.findValue("value")).map(n =>
        if (n.isNull)
          o.topLevel(k).clear
        // TODO ... somehow need an extra accessor level at top level, would be much nicer
        else
          iterateMapKeys(o, n, List())
      ).getOrElse(o.topLevel(k).clear) // no value field at top level, should not happen, but treat as null/clear
    }

    n.fields().asScala.foldLeft(Offer.create)((oAcc, me: Entry[String, JsonNode]) =>
      parseTopLevel(oAcc, me.getKey, me.getValue))
  }


  def printNode(name: String, n: JsonNode, depth: Int): Unit = {
    for (d <- 0.until(depth)) print(".")
    print(name + ": " + n.getNodeType + " = ")
    if (n.isArray) {
      println("[")
      for (elem <- n.elements().asScala) printNode("", elem, depth + 1)
      for (d <- 0.until(depth)) print(".")
      println("]")
    } else {
      println(n.asText())
      for (child <- n.fields().asScala) printNode(child.getKey, child.getValue, depth + 1)
    }
  }
}
