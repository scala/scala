object Test extends App {
  val s0: Stream[Int] = Stream.empty
  println(s0.take(1))
  println(s0.takeWhile(_ > 0))
  println(s0.lengthCompare(-5) > 0)
  println(s0.lengthCompare(0) == 0)
  println(s0.lengthCompare(5) < 0)
  println

  val s1 = Stream.cons(1, Stream.empty)
  println(s1.toArray.deep)
  println(s1.take(1))
  println(s1.take(2))
  println(s1.drop(1))
  println(s1.drop(2))
  println(s1.drop(-1))
  println(s1.dropWhile(_ > 0))
  println(s1.lengthCompare(-5) > 0)
  println(s1.lengthCompare(0) > 0)
  println(s1.lengthCompare(1) == 0)
  println(s1.lengthCompare(5) < 0)
  println

  val s2 = s1.append(Stream.cons(2, Stream.empty))
  println(s2.toArray.deep)
  println(s2.drop(1))
  println(s2.drop(2))
  println(s2.drop(-1))
  println(s2.dropWhile(_ > 0))
  println(s2.lengthCompare(-5) > 0)
  println(s2.lengthCompare(0) > 0)
  println(s2.lengthCompare(1) > 0)
  println(s2.lengthCompare(2) == 0)
  println(s2.lengthCompare(5) < 0)
  println

  val s3 = Stream.range(1, 1000) //100000 (ticket #153: Stackoverflow)
  println(s3.length)

  // ticket #153
  def powers(x: Int) = if ((x&(x-1)) == 0) Some(x) else None
  println(s3.flatMap(powers).reverse.head)

  // large enough to generate StackOverflows (on most systems)
  // unless the following methods are tail call optimized.
  val size = 100000

  // test tail recursive methods
  println(Stream.from(1).take(size).last)
  println(Stream.from(1).drop(size))
  println(Stream.from(1).filter(_ > size).take(5))
  println(Stream.from(1).take(size).forall(_ >= 0))
  println(Stream.from(1).exists(_ > size))
  Stream.from(1).take(size).foreach( x => () )
  println(Stream.from(1).take(size).foldLeft(0)(_ + _))
  val arr = new Array[Int](size)
  Stream.from(1).take(size).copyToArray(arr, 0)

  println

  // ticket #6415
  lazy val x = { println("evaluated"); 1 }
  val s4 = 0 #:: x #:: Stream.empty

  println(s4.isDefinedAt(0))
}
