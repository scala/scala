object Test {
  def main(args: Array[String]): Unit = {
    var test: String = null
    val fun1: Int => () => Unit = foo(test) _
    val fun2: Int => () => Unit = foo(test)(_)
    val fun3: Int => () => Unit = {
      lazy val eta1: String = test
      (dummy: Int) => foo(eta1)(dummy)
    }
    val fun4: Int => () => Unit = {
      val eta1: () => String = () => test
      (dummy: Int) => foo(eta1())(dummy)
    }
    test = "some string"
    fun1(1)()
    fun2(1)()
    fun3(1)()
    fun4(1)()

    val f: (String, Int*) => Unit = m(2, 3)
    f("", 5, 6)
  }

  def foo(s: => String)(dummy: Int) = () => println(s)

  def m(a: Int*)(z: String, b: Int*) {
    println(a.toList)
    println(b.toList)
  }
}
