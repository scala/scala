import scala.language.higherKinds

object TestExplicit {
  trait TC[A]
  def fTt[A,E[X] <: List[X]](a: A)(implicit tt: TC[E[A]]) = a
  implicit def tc[T]: TC[T] = ???

  // Typechecking results in SOE in TypeVar.isGround
  fTt(1)(tc)
  // fun = TestExplicit.this.fTt[Int, E](1)
  // args = TestExplicit.this.tc[E[Int]]
  // argTpes.head.instantiateTypeParams = TC[?E#1[Int]]
  // formals.head.instantiateTypeParams = TC[?E#2[Int]]
  //   (where ?E#1 and ?E#2 as distinct AppliedTypeVars that resulted
  //    from separate applications of type args to the same HKTypeVar, ?E)
  //
  // As we check if the argument conforms to the formal, we would have
  // AppliedTypeVars sharing the same TypeConstraints on the LHS and RHS,
  // which leads to a cyclic constraint.
}

object TestImplicit    {
  trait TC[A]
  def fTt[A,E[X] <: List[X]](a: A)(implicit tt: TC[E[A]]) = a
  implicit def tc[T]: TC[T] = ???

  // Oddly enough, this one works.
  fTt(1)
}
