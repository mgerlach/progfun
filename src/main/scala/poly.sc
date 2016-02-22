// Map exp -> coeff
class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  //  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  //  def adjust(term: (Int, Double)) = {
  //    val (exp, coeff) = term
  //    exp -> (coeff + terms(exp))
  //  }

  def +(other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp).mkString(" + ")
}

val p1 = new Poly(0 -> 2, 1 -> 1, 2 -> 3.1)
val p2 = new Poly((1, 2), (2, 1), 4 -> 8)

p1 + p2