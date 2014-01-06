trait Fooz[Q <: Array[_]] {
  def f0(x: Q) = x.length
}

object Test extends Fooz[Array[Int]] {
  val f1 = new Fooz[Array[String]] { }
  val f2 = new Fooz[Array[Int]] { }
  val f3 = new Fooz[Array[Any]] { }
  val f4 = new Fooz[Array[_]] { }

  def main(args: Array[String]): Unit = {
    println(f1.f0(Array[String]("a", "b")))
    println(f2.f0((1 to 1000).toArray))
    println(f3.f0((1 to 1000).toArray[Any]))
    println(f4.f0(('a' to 'z').toArray))
  }
}
