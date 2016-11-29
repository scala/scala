trait F1 {
  def apply(a: List[String]): String
  def f1 = "f1"
}

object Test extends App {
  // Wrap the sam-targeting function in a context where the expected type is erased (identity's argument type erases to Object),
  // so that Erasure can't tell that the types actually conform by looking only
  // at an un-adorned Function tree and the expected type
  // (because a function type needs no cast it the expected type is a SAM type),
  //
  // A correct implementation of Typers/Erasure tracks a Function's SAM target type directly
  // (currently using an attachment for backwards compat),
  // and not in the expected type (which was the case in my first attempt),
  // as the expected type may lose its SAM status due to erasure.
  // (In a sense, this need not be so, but erasure drops type parameters,
  //  so that identity's F1 type argument cannot be propagated to its argument type.)
  def foo = identity[F1]((as: List[String]) => as.head)

  // check that this doesn't CCE's
  foo.f1
}
