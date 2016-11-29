import scala.annotation._

trait C[@specialized U] {
  @unspecialized
  def foo(u: U): U
  @unspecialized
  def bar[A](u: U) = u
}

object Test {
  def main(args: Array[String]) {
    val declared = classOf[C[_]].getDeclaredMethods.sortBy(_.getName)
    println(declared.mkString("\n"))
    object CInt extends C[Int] { def foo(i: Int) = i }
    object CAny extends C[Any] { def foo(a: Any) = a }
    assert(CInt.foo(1) == 1)
    assert(CAny.foo("") == "")
  }
}
