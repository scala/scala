case class C()

object arrays2 {

  def main(args: Array[String]): Unit = {
    val a: Array[Array[C]] = new Array[Array[C]](2)
    a(0) = new Array[C](2)
    a(0)(0) = new C()
  }
}

// #2422
object arrays4 {
  val args = Array[String]("World")
  "Hello %1$s".format(args: _*)
}
/*
test/files/pos/arrays2.scala:15: warning: Passing an explicit array value to a Scala varargs method is deprecated (since 2.13.0) and will result in a defensive copy; Use the more efficient non-copying ArraySeq.unsafeWrapArray or an explicit toIndexedSeq call
  "Hello %1$s".format(args: _*)
                      ^
one warning found
*/

// #2461
object arrays3 {
  def apply[X](xs : X*) : java.util.List[X] = java.util.Arrays.asList(xs: _*)
}
