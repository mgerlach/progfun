

def isPrime(n: Int): Boolean =
  (2 until n) forall (d => n % d != 0)
isPrime(1)
isPrime(2)
isPrime(3)
isPrime(4)
isPrime(5)
isPrime(10)
isPrime(11)
isPrime(127)
val n = 7
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
  isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct1(xs: List[Double], ys: List[Double]): Double =
  (xs zip ys).map { case (x, y) => x * y }.sum
scalarProduct1(List(2, 1, 3), List(1, 2, 3))
def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum
scalarProduct(List(2, 1, 3), List(1, 2, 3))
def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}

// check for new queen in row row and column col
// queens: columns of already placed queens for rows row-1, row-2, ... 0
def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  (row - 1 to 0 by -1) zip queens forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

val x = queens(8)

x take 3 map show mkString "\n"