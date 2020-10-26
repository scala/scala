package tastytest

object SymbollicEnums:

  enum Ops:
    case +,*

  enum Expr[T]:
    case Literal(x: T)
    case Infix(op: Ops, l: Expr[T], r: Expr[T])

    def +(that: Expr[T]) = Infix(Ops.+, this, that)
    def *(that: Expr[T]) = Infix(Ops.*, this, that)

  final implicit class IntroduceOps[T](x: T):
    def lit: Expr[T] = Expr.Literal(x)

  trait Interpreter[T]:
    val zero: Expr[T]
    val one: Expr[T]
    extension (e: Expr[T]) def interpret: T

  class Laws[T](using intp: Interpreter[T]):
    import intp._

    def additiveIdentity(n: Expr[T])                                    = (n + zero).interpret == n.interpret
    def additiveCommutativity(n: Expr[T], m: Expr[T])                   = (n + m).interpret == (m + n).interpret
    def additiveAssociativity(n: Expr[T], m: Expr[T], o: Expr[T])       = (n + (m + o)).interpret == ((n + m) + o).interpret
    def multiplicativeIdentity(n: Expr[T])                              = (n * one).interpret == n.interpret
    def multiplicativeCommutativity(n: Expr[T], m: Expr[T])             = (n * m).interpret == (m * n).interpret
    def multiplicativeAssociativity(n: Expr[T], m: Expr[T], o: Expr[T]) = (n * (m * o)).interpret == ((n * m) * o).interpret

    def multiplicativeDistributivity(n: Expr[T], m: Expr[T], o: Expr[T]) =
      (n * (m + o)).interpret == ((n * m) + (n * o)).interpret && ((n + m) * o).interpret == ((n * o) + (m * o)).interpret
