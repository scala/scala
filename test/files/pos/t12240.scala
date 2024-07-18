//> using options -Xfatal-warnings -Xlint:strict-unsealed-patmat
//

object Test {

  //original reports

  def originalReported(v: Vector[String]) = v match {
    case Vector() => "empty"
    case Vector(_) => "one"
    case Vector(_*) => "this pattern is irrefutable"
  }

  def originalMinimized(v: Vector[String]) = v match {
    case Vector(_*) => "this pattern is irrefutable"
  }

  //broader applicability

  class IrrefutableNameBasedResult[Result](r: Result) {
    def isEmpty: false = false
    def get: Result = r
  }

  object IrrefutableIdentityExtractor {
    def unapply[A](a: A) = new IrrefutableNameBasedResult(a)
  }

  object IrrefutableSeqExtractor {
    def unapplySeq[A](a: A) = new IrrefutableNameBasedResult(List(a))
  }

  def nameBasedPatternIsExhaustive(x: Int) = x match {
    case IrrefutableIdentityExtractor(_) => "exhaustive"
  }

  def nameBasedSeqIsExhaustive(x: Int) = x match {
    case IrrefutableSeqExtractor(_*) => "exhaustive"
  }

  //status quo:
  //should be in neg/t12240.scala but isn't exhaustive per
  //per https://github.com/scala/bug/issues/12252

  def reported(v: Vector[String]) = v match {
    case Vector() => "empty"
    case Vector(_) => "one"
    case Vector(_, _, _*) => "this ought to be exhaustive"
    case Vector(_*) => "scalac doesn't know was already exhaustive"
  }

  //status quo:
  //should be in neg/t12240.scala, but the unreachable code isn't reported
  //per https://github.com/scala/bug/issues/12251
  def nameBasedPatternUnreachable(x: Int) = x match {
    case IrrefutableIdentityExtractor(_) => "exhaustive"
    case _ => "unreachable"
  }

  def nameBasedSeqUnreachable(x: Int) = x match {
    case IrrefutableSeqExtractor(_*) => "exhaustive"
    case _ => "unreachable"
  }

}