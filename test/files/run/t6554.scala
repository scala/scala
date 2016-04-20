trait T1[A] {
  def minBy[B](b: B): A = ???
}

// The second trait is needed to make sure there's a forwarder generated in Bar.
// otherwise Bar.minBy is just the inherited default method from T1.
trait T2[A] { self: T1[A] =>
  override def minBy[B](b: B): A = ???
}

class Bar extends T1[Int] with T2[Int]

object Test extends App {
   val sigs = classOf[Bar].getDeclaredMethods.map(m => s"${m.toString} / ${m.toGenericString}").sorted
   println(sigs.mkString("\n"))
}
// Was public java.lang.Object Bar.minBy(java.lang.Object) / public <B> int Bar.minBy(B)
