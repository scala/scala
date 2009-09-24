case class S1(n: Int) { }
case class S2(n: Int, p: Int) { }
class S3 { }
object S3 {
  def foo() = "abc"
  def bar[T](x: T): T = x
}
