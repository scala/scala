class A
trait B[T <: B[T]] extends A
class C extends B[C] { override def toString = "C" }
class D extends B[D] { override def toString = "D" }

class E {
  val ys = List(List(new C), Stream(new D))
}

object Test {
  def trav = List(List(), Stream())

  def main(args: Array[String]): Unit = {
    val f = (new E).ys _
    var xs: Set[List[_ <: Seq[B[_]]]] = Set()
    xs += f()
    println(xs)
  }
}
