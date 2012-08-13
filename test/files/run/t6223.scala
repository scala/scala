class Foo[@specialized(Int) A](a:A) {
  def bar[@specialized(Int) B](f:A => B) = new Foo(f(a))
}

object Test {
  def main(args:Array[String]) {
    val f = new Foo(333)
    val ms = f.getClass().getDeclaredMethods()
    ms.foreach(m => println(m.getName))
  }
}
