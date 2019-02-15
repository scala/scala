package a {
  package object b {
    implicit def foo: String = ""
  }
  package b {
    object Test {
      import b._

      foo
      implicitly[String]
    }
  }
}
