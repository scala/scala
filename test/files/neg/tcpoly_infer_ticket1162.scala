object Test {
  trait Expression[A,B] 

  case class Lift[A,B,F[_]]() extends Expression[F[A],F[B]]
  
  def simplify[A,B]: Expression[A,B] = Lift[A,B]()
}

