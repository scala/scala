object Foo { def unapply: Option[Int] = Some(42) }
object Foo2 { def unapply(): Option[Int] = Some(42) }


object Test {
  def main(args: Array[String]): Unit = {
    val Foo(x1) = 1
    val Foo2(y2) = 2
    ()
  }
}
