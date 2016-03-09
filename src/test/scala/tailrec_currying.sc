import scala.annotation.tailrec

def factorial(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc else loop(acc * n, n - 1)
  loop(1, n)
}

factorial(5)

// @tailrec would not work here!
def factNonTailRec(n: Int): Int =
  if (n == 0) 1 else n * factNonTailRec(n - 1)
factNonTailRec(5)

def mapReduce[T, U](map: T => U, combine: (U, U) => U, unit: U)(input: Seq[T]): U = {
  @tailrec
  def iter(acc: U, input: Seq[T]): U = {
    if (input isEmpty)
      acc
    else
      iter(combine(acc, map(input head)), input tail)
  }
  iter(unit, input)
  // non tailrec
  //  if (input == Nil) // isEmpty
  //    unit
  //  else
  //    combine(map(input.head), mapReduce(map, combine, unit)(input.tail))
}
def productInt(f: Int => Int)(a: Int, b: Int) =
  mapReduce[Int, Int](f, (x, y) => x * y, 1)((a to b).toStream)
def multInts = productInt(x => x) _
def fact(x: Int) = multInts(1, x)
fact(5)

















