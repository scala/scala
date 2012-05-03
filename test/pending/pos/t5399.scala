class Test {
  class A[T]
  class B[T](val a: A[T])

  case class CaseClass[T](x: T)

  def break(existB: B[_]) = CaseClass(existB.a) match { case CaseClass(_) => }
}
