
object Test {
  type TagP[X]    = { type Tag = X }
  type Tag = { type Tag = X forSome { type X } }
  type TArray[T] = Array[T] with TagP[Any]

  implicit def liftToTag1[T](xs: Array[T]): TArray[T] = xs.asInstanceOf[TArray[T]]
  implicit def liftToTag2[T](xs: Array[T]): Array[T] with Tag = xs.asInstanceOf[Array[T] with Tag]

  class Foo[T] {
    var x1: Array[T] with Tag = null
    var x2: TArray[T] = null
    var x3: TArray[T] with Array[T] = null

    // var x1: (Array[T] with Tag) forSome { type T } = Array()
    // var x2: Array[Array[T] with Tag] = Array()
    // var x3: Array[Array[(Array[T] with Tag) forSome { type T }] with Tag] = Array(Array())
  }

  def f1 = {
    val foo = new Foo[Int]
    foo.x1 = Array(1) ; foo.x2 = Array(1) ; foo.x3 = Array(1)
    (foo.x1, foo.x2, foo.x3, foo.x1(0) + foo.x2(0) + foo.x3(0))
  }
  def f2 = {
    val foo = new Foo[String]
    (foo.x1, foo.x2, foo.x3)
  }
  def f3 = {
    val foo = new Foo[Array[Array[Int]]]
    (foo.x1, foo.x2, foo.x3)
  }
  def f4[T](x: Array[Array[T]]) = {
    val foo = new Foo[Array[T]]
    foo.x1 = x
    (foo.x1, foo.x2, foo.x3, foo.x1(0)(0))
  }

  def main(args: Array[String]): Unit = {
    Test.getClass.getDeclaredMethods.map(m => s"${m.getName}: ${m.getGenericReturnType}").sorted foreach println
    println(f1)
    println(f2)
    println(f3)
    println(f4[Int](Array(Array(1))))
    println(f4[String](Array(Array(""))))
  }
}
