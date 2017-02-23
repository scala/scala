package bippy {
  trait Bippy[A] extends Any
}
package foo {
  package object unrelated {
    implicit def bippyDingo[A](x: bippy.Bippy[A]): AnyRef = Nil
  }
  package unrelated {
    trait Unrelated
  }
}

object Test {
  trait Dingo extends Any with bippy.Bippy[foo.unrelated.Unrelated]

  def f(x: Dingo): AnyRef = x   // fail - no conversion to AnyRef
  def f2(x: Dingo): Object = x   // fail - no conversion to Object
  var x: Int = null             // fail - no conversion from Null
}
