package test

final class Test extends PackageProtected {
  def bar = foo
}

package another {
  object Main {
    def bug(t: Test) {
      // Can always be replicated.
      println(t.foo)
    }
  }
}