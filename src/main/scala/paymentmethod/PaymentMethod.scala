package paymentmethod

/**
  * @author martin.gerlach
  */
object PaymentMethod extends enum.Enum[PaymentMethod]
sealed trait PaymentMethod extends PaymentMethod.Value
case object cbc extends PaymentMethod
case object cod extends PaymentMethod
case object pp extends PaymentMethod