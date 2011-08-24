package test

final class Test extends PackageProtected {
  def bar = foo
}

package another {
  object Main {
    def t1(t: Test) {
      // Can always be replicated.
      println(t.foo)
    }
    def t2(t: Test) {
      // Conditions to replicate: must use -optimise, class Test must be final
      println(t.bar)
      //@noinline is a usable workaround
    }
    def main(args: Array[String]) {
      t1(new Test)
      t2(new Test)
    }
  }
}
