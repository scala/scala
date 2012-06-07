case class Foo[T](a: Boolean, b: Byte, c: Short, d: Char, e: Int, f: Long, g: Double, h: Float, i: AnyRef, j: T) { }

object Test {
  def mkFoo[T](x: T) = Foo[T](true, -1, -1, 100, -5, -10, 500d, 500f, Nil, x)

  def main(args: Array[String]): Unit = {
    val foo1 = mkFoo[Double](5.0d)
    val foo2 = mkFoo[Long](5l)

    List(foo1, foo2, foo1.##, foo2.##, foo1 == foo2) foreach println

    println("## method 1: " + foo1.##)
    println("## method 2: " + foo2.##)
    println("   Murmur 1: " + scala.util.hashing.MurmurHash3.productHash(foo1))
    println("   Murmur 2: " + scala.util.hashing.MurmurHash3.productHash(foo2))
  }
}

object Timing {
  var hash = 0
  def mkFoo(i: Int) = Foo(i % 2 == 0, i.toByte, i.toShort, i.toChar, i, i, 1.1, 1.1f, this, this)

  def main(args: Array[String]): Unit = {
    val reps = if (args.isEmpty) 100000000 else args(0).toInt
    val start = System.nanoTime

    println("Warmup.")
    1 to 10000 foreach mkFoo

    hash = 0
    1 to reps foreach (i => hash += mkFoo(i).##)

    val end = System.nanoTime
    println("hash = " + hash)
    println("Elapsed: " + ((end - start) / 1e6) + " ms.")
  }
}
