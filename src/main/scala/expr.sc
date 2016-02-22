import expr._

def e = Sum(Prod(Num(2), Sum(Num(5), Num(6))), Prod(Sum(Num(3), Num(10)), Num(4)))

e.eval

e.show
