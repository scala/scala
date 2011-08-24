package foo {
  class Foo { }
  object Foo {
    def bippy(x: Int) = x
  }
}

package bar {
  class Bippy extends foo.Foo {
    def bippy(x: Int) = x
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new bar.Bippy bippy 5
  }
}
