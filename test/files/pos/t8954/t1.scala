package scala.foo

// 1. a class about to be made final
@deprecatedInheritance class A {
  def foo(): Unit = ???
}

// 1.1:
// - no inheritance warning because same file
// - no "override non-deprecated member" because @deprecatedInheritance
class B2 extends A {
  @deprecated("","") override def foo(): Unit = ???
}
