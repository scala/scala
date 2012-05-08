trait B {
  this: Test.type =>

  def y = new { def f() = () }
  def fail() = y.f()
}
object Test extends B {
  def main(args: Array[String]): Unit = fail()
}