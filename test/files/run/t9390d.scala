class C { // C is not serializable
  def foo: () => Any = {
    { () => class UseOuterInConstructor { C.this.toString }; new UseOuterInConstructor : Any}
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val f = c.foo
    f() // Doesn't NPE, as we didn't elide the outer instance in the constructor call.
  }
}
