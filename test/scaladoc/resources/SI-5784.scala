package test.templates {
  object `package` {
    /** @template */
    type String = java.lang.String
    val String = new StringCompanion
    class StringCompanion { def boo = ??? }
  }

  /** @contentDiagram */
  trait Base {
    /** @template */
    type String = test.templates.String
    /** @template
     * @inheritanceDiagram */
    type T <: Foo
    val T: FooExtractor
    trait Foo { def foo: Int }
    trait FooExtractor { def apply(foo: Int); def unapply(t: Foo): Option[Int] }
  }

  /** @contentDiagram */
  trait Api extends Base {
    /** @template
     *  @inheritanceDiagram */
    override type T <: FooApi
    trait FooApi extends Foo { def bar: String }
  }
}
