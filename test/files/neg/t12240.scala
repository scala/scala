//> using options -Xfatal-warnings -Xlint:strict-unsealed-patmat
//

object Test {

  //see also pos/t12240.scala

  class IrrefutableNameBasedResult[Result](r: Result) {
    def isEmpty: false = false
    def get: Result = r
  }

  object IrrefutableIdentityExtractor {
    def unapply[A](a: A): IrrefutableNameBasedResult[A] = new IrrefutableNameBasedResult(a)
  }

  object IrrefutableSeqExtractor {
    def unapplySeq[A](a: A) = new IrrefutableNameBasedResult(List(a))
  }

  def guardedNonExhaustive(x: Int) = x match {
    case IrrefutableIdentityExtractor(_) if false => "non-exhaustive"
  }

  def guardedSeqNonExhaustive(x: Int) = x match {
    case IrrefutableSeqExtractor(_*) if false => "non-exhaustive"
  }

  //status quo:
  //should be in pos/t12240.scala but isn't exhaustive per
  //per https://github.com/scala/bug/issues/12252
  def reported(v: Vector[String]) = v match {
    case Vector() => "empty"
    case Vector(_) => "one"
    case Vector(_, _, _*) => "scalac doesn't know that this is exhaustive"
  }

}