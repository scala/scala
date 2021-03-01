object Test {
  def main(args: Array[String]) {
    println(new C().foo(null, 0))
    println(Macro.reflectorConstructor("dynamic"))
    println(Macro.reflectorTrait("dynamic"))
  }
}

class C {
  def foo(p1: Object, p2: Int): String = {
    Macro.reflector("dynamic")
    privateFoo(p1, p2)
  }

  private def privateFoo(p1: Object, p2: Int): String = {
    Macro.reflector("dynamic")
  }
}
