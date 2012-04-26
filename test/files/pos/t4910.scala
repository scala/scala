class A {
  implicit object foo
  // it compiles if we uncomment this
  // implicit val bar = foo
  implicitly[foo.type]
}
