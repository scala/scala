trait Foo {
  def getFoo() = "foo"
}

class Sub extends Foo {
  def getBar() = "bar"
}

object Test {
  def main(args: Array[String]): Unit = {
    val ms = classOf[Sub].getDeclaredMethods
    assert(ms forall (x => !x.isBridge), ms mkString " ")
  }
}
