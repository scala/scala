package nodescala

class NodeScalaSuite {
  "".rich

  // This is here only to prove that the presentation compiler is instantiated with the
  // correct `sourcepath` value (if it wasn't, you would see a `not found: type Foo` in
  // the test's output
  println(new Foo())
}
