package foo

package object bar {
  type IOException = Object
  type Bippy[T] = List[T]
}

package object baz {
  type Bippy[+T] = List[T]
}

package baz {
  import java.io._
  import foo.bar._

  object Test {
    def f = new IOException // genuinely different
  }
}

package baz2 {
  import bar._
  import baz._

  object Test2 {
    def f: Bippy[Int] = ???
  }
}
