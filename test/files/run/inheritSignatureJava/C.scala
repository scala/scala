class C[A] {
  def foo(a: A): String = "(C)" + a
}

class Sub extends C[String] {
  @scala.annotation.inheritSignature
  override def foo(a: String): String = "(Sub)" + super.foo(a)
}

class Buk extends Sub {
  final def superFoo(a: String) = "(Buk)" + super[Sub].foo(a)
}
