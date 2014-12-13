package accessibility {

  class Foo {
    private def secretPrivate(): Unit = ()
    private[this] def secretPrivateThis(): Unit = ()

    protected def secretProtected(): Unit

    protected[accessibility] def secretProtectedInPackage(): Unit

    def secretPublic(): Unit

    def someTests(other: Foo) {
      other./*!*/secretPrivate // should be all but secretThis

      this./*!*/secretProtected // should hit five completions
    }
  }

  class AccessibilityChecks extends Foo {
    def someTests {
      this./*!*/ // should not list secretPrivate*
    }
  }

  class UnrelatedClass {
    def someTests(foo: Foo) {
      foo./*!*/ // should list public and protected[accessibility]
    }
  }

}

package other {
  class SomeChecsk {
    def foo(o: accessibility.Foo) {
      o./*!*/ // should only match secretPublic
    }
  }
}