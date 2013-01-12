abstract class A( val someAs: A* ) {
  override def toString = someAs.length + " As"
}
object B extends A(null, null, null)

object Test {
  def main(args: Array[String]): Unit = {
    println(B)
  }
}
