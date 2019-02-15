package a {
  object `package` {
    implicit def foo: String = ""
  }
}

object Test {
  import a._
  implicitly[String]
}