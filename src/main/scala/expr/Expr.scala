package expr

/**
  * @author martin.gerlach
  */
trait Expr {
  def eval: Int = this match {
    case Num(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }
  def show: String = {
    def pshow(e: Expr): String = e match {
      case Num(_) => e.show
      case Sum(_, _) => "( " + e.show + " )"
      case Prod(_, _) => e.show
    }
    this match {
      case Num(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Prod(e1, e2) => pshow(e1) + " * " + pshow(e2)
    }
  }
}

case class Num(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr
