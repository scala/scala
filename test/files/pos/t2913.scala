class A {
  def foo(a: Int) = 0
}

class RichA {
  def foo(a: String) = 0
  def foo(a: String, b: String) = 0
  def foo() = 0
}

object Test {

  implicit def AToRichA(a: A) = new RichA

  val a = new A
  a.foo()
  a.foo(1)

  a.foo("")       // Without implicits, a type error regarding invalid argument types is generated at `""`. This is
                  // the same position as an argument, so the 'second try' typing with an Implicit View is tried,
                  // and AToRichA(a).foo("") is found.
                  //
                  // My reading of the spec "7.3 Views" is that `a.foo` denotes a member of `a`, so the view should
                  // not be triggered.
                  //
                  // But perhaps the implementation was changed to solve See https://lampsvn.epfl.ch/trac/scala/ticket/1756

  a.foo("a", "b") // Without implicits, a type error regarding invalid arity is generated at `foo(<error>"", "")`.
                  // Typers#tryTypedApply:3274 only checks if the error is as the same position as `foo`, `"a"`, or `"b"`.
                  // None of these po
}
