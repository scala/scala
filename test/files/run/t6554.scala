trait Foo[A] {
  def minBy[B](b: B): A = ???
}
 
class Bar extends Foo[Int]

object Test extends App {
   val sigs = classOf[Bar].getDeclaredMethods.map(m => s"${m.toString} / ${m.toGenericString}").sorted
   println(sigs.mkString("\n"))
}
// Was public java.lang.Object Bar.minBy(java.lang.Object) / public <B> int Bar.minBy(B)
