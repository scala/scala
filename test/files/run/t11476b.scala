case class Foo(a: Int*) { def lengthCompare(i: Int) = 1 }
object FooExtractor {
  def unapplySeq(x$0: Foo): Option[Seq[Int]] = if (x$0.==(null))
    None
  else
    Some[Seq[Int]](x$0.a);
}

object Test {
  def main(args: Array[String]): Unit = {
     val x = Foo(1) match { case FooExtractor(a) => a }
  }
}
