def from(n: Int): Stream[Int] = n #:: from(n + 1)

val nats = from(0)
val mul4s = nats map (_ * 4)

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s filter (_ % s.head != 0))

def primes(num: Int): List[Int] =
  sieve(from(2)) take num toList

primes(1000)

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(2) take 10 toList

sqrtStream(25) take 10 toList

def sqrtWithPrecision(precision: Double)(x: Double) = {
  def isGoodEnough(guess: Double) =
    math.abs(guess * guess - x) / x < precision
  sqrtStream(x).filter(isGoodEnough).head
}

def sqrt6 = sqrtWithPrecision(0.000001) _

sqrt6(10)

