package enum

/**
  * @author martin.gerlach
  */
trait Enum[A] {
  trait Value { self: A =>
    myValues :+= this
  }
  private var myValues = List.empty[A]
  def values = myValues
  def forName(name: String) = myValues.find(name == _.toString)
}

