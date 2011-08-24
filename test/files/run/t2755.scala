// Test cases: the only place we can cut and paste without crying
// ourself to sleep.
object Test {
  def f1(a: Any)  = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }
  def f2(a: Array[_])  = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }
  def f3[T](a: Array[T]) = a match {
    case x: Array[Int]      => x(0)
    case x: Array[Double]   => 2
    case x: Array[Float]    => x.sum.toInt
    case x: Array[String]   => x.size
    case x: Array[AnyRef]   => 5
    case x: Array[_]        => 6
    case _                  => 7
  }


  def main(args: Array[String]): Unit = {
    println(f1(Array(1, 2, 3)))
    println(f1(Array(1.0, -2.0, 3.0, 1.0)))
    println(f1(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f1((1 to 4).toArray map (_.toString)))
    println(f1(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f1(Array(1L)))
    println(f1(null))

    println(f2(Array(1, 2, 3)))
    println(f2(Array(1.0, -2.0, 3.0, 1.0)))
    println(f2(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f2((1 to 4).toArray map (_.toString)))
    println(f2(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f2(Array(1L)))
    println(f2(null))

    println(f3(Array(1, 2, 3)))
    println(f3(Array(1.0, -2.0, 3.0, 1.0)))
    println(f3(Array(1.0f, 2.0f, 3.0f, -3.0f)))
    println(f3((1 to 4).toArray map (_.toString)))
    println(f3(new Array[Any](10)))    // should match as Array[AnyRef]
    println(f3(Array(1L)))
    println(f3(null))
  }
}
