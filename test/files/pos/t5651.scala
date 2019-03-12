object Test {
  trait Exp[+T]
  case class Const[T](t: T) extends Exp[T]
  implicit def pure[T](t: T): Exp[T] = Const(t)
  case class LiftTuple2[A1, A2](t1: Exp[A1], t2: Exp[A2]) extends Exp[(A1, A2)]
  implicit def tuple2ToTuple2ExpPrime[ArgFOO1, A2, E1 <% Exp[ArgFOO1], E2 <% Exp[A2]](tuple: (E1, E2)): LiftTuple2[ArgFOO1, A2] = LiftTuple2[ArgFOO1, A2](tuple._1, tuple._2)

  val a = pure(1)
  val b = pure("")
  val c = pure(2)
  def asExp[T](t: Exp[T]) = t //an evaluation context triggering implicit conversions
  tuple2ToTuple2ExpPrime(((a, b), c))
  asExp(tuple2ToTuple2ExpPrime( ((a, b), c) ))
  asExp(((a, b), c)) //does not compile
}
