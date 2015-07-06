package scala.foo

// 1.2 deprecated children should be fine...
@deprecated("", "") class B extends A {

  // 1.3 and shouldn't trigger the
  // "overriding non-deprecated parent" warning
  override def foo(): Unit = ???
}

@deprecated("","") class F {
  // 1.4 a class inside a deprecated class should work too
  class G extends A
}

// 2. a method about to be made final
class C {
  @deprecatedOverriding def foo(): Unit = ???
}

// 2.1 overriding with a deprecated def should be fine
// and also should not trigger the "deprecation is useless"
// warning
class D extends C {
  @deprecated("","") override def foo(): Unit = ???
}

// 2.2 overriding from a deprecated class should be fine
@deprecated("","") class E extends C {
  override def foo(): Unit = ???
}

// 2.3 overriding from deeper inside a deprecated class
// should work too
@deprecated("","") class H {
  class I extends C {
    override def foo(): Unit = ???
  }
}
