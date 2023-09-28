abstract class A {
  val t: AnyRef
  object X {
    def foo: t.type = t
  }
}
