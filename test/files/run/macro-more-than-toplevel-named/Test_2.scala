object Test extends App {
  Macros.addMethod(classOf[C], "foo", (x: Int) => x + 2)
  println(new C().foo(2))
}

class C
