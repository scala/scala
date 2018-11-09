object Test {
  implicit class C(self: String)(implicit val foo: String = "default")

  def main(args: Array[String]) {
    println("".foo)
    println(C("").foo)
    println(new C("").foo)
    println(C("")("explicit").foo)
    println(new C("")("explicit").foo)
  }
}
