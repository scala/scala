package foo

package object bar {
  type IOException = java.io.IOException
}

package baz {
  import java.io._
  import foo.bar._

  object Test {
    def f = new IOException
  }
}
