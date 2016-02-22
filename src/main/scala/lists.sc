def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (xs1, Nil) => xs1
    case (Nil, ys1) => ys1
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }
def l = merge(List(1, 3), List(0, 2, 6))
l.map(x => 2 * x)
def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList1(ys)
}
def squareList(xs: List[Int]): List[Int] =
  xs map (y => y * y)
squareList1(l)
squareList(l)

//should give
//  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (a, b) = xs span (x1 => x1 == x)
    a :: pack(b)
}
val data: List[String] = List("a", "a", "a", "b", "c", "c", "a")
pack(data)
def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (a, b) = xs span (x1 => x1 == x)
    (x, a.length) :: encode(b)
}
encode(data)
def encode2[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))
encode2(data)
def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) (f(_) :: _)
mapFun[Int, Int](List(1, 2, 3, 4), 2 * _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((x: T, l: Int) => l + 1)
lengthFun(List(1, 2, 3))