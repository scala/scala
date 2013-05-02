package object foo {
  implicit class Bar[T](val x: T) extends AnyVal {
    def bippy = 1
  }
}

package foo {
  object Baz {
    def main(args: Array[String]): Unit = {
      "abc".bippy
    }
  }
}
