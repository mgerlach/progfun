import numbers._

val one = Zero.successor
val two = one + one
val three = two + one
val four = three + one

three - two
two - Zero
one - one
four - three

val half = new Rational(1, 2)
val third = new Rational(1, 3)
val twothird = new Rational(2) * third
half + third
half - third
third - half
third * half
twothird * twothird
new Rational(6, 8)
-half
-(-half)

