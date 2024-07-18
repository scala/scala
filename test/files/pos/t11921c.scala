//> using options -Xsource:3

// test/scaladoc/resources/t5784.scala

package test.templates {
  object `package` {
    type String = java.lang.String
    val String = new StringCompanion
    class StringCompanion { def boo = ??? }
  }

  trait Base {
    type String = test.templates.String
    type T <: Foo
    val T: FooExtractor
    trait Foo { def foo: Int }
    trait FooExtractor { def apply(foo: Int): Unit; def unapply(t: Foo): Option[Int] }
  }

  trait Api extends Base {
    override type T <: FooApi
    trait FooApi extends Foo { def bar: String }
  }
}
