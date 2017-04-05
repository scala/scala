class MySeq[T](val i: Int) {
  def map1[U](f: T => U): MySeq[U] = new MySeq[U](10)
  def map2[U](f: T => U): MySeq[U] = new MySeq[U](20)
}

class MyMap[A, B](i: Int) extends MySeq[(A, B)](i) {
  def map1[C](f: (A, B) => C): MySeq[C] = new MySeq[C](11)
  def map1[C, D](f: (A, B) => (C, D)): MyMap[C, D] = new MyMap[C, D](12)
  def map1[C, D](f: ((A, B)) => (C, D)): MyMap[C, D] = new MyMap[C, D](13)
  def map3[U](f: PartialFunction[(A, B), U]): MySeq[U] = new MySeq[U](30)

  def foo(f: Function2[Int, Int, Int]): Unit = ()
  def foo[R](pf: PartialFunction[(A, B), R]): MySeq[R] = new MySeq[R](100)
}

object Test extends App {
  val m = new MyMap[Int, String](0)

  // These ones already worked because they are not overloaded:
  m.map2 { case (k, v) => k - 1 }
  m.map3 { case (k, v) => k - 1 }

  // These already worked because preSelectOverloaded eliminated the non-applicable overload:
  // (The still pick the same overload; no previously legal code changes semantics)
  assert(m.map1(t => t._1).i == 10)
  assert(m.map1((kInFunction, vInFunction) => kInFunction - 1).i == 11)
  val r1 = m.map1(t => (t._1, 42.0))
  val r1t: MyMap[Int, Double] = r1
  assert(r1.i == 13)

  // These worked because they are not case literals (and the argument types are known for overload resolution):
  assert(m.map1({ case (k, v) => k - 1 }: PartialFunction[(Int, String), Int]).i == 10)
  assert(m.map2({ case (k, v) => k - 1 }: PartialFunction[(Int, String), Int]).i == 20)

  // These ones did not work before, now always picks tupled version over Function2 version:
  assert(m.map1 { case (k, v) => k }.i == 10)
  val r2 = m.map1 { case (k, v) => (k, k*10) }
  val r2t: MyMap[Int, Int] = r2
  assert(r2.i == 13)
  val r3 = m.foo { case (k, v) => k - 1 }
  val r3t: MySeq[Int] = r3

  // Used to be ambiguous but overload resolution now favors PartialFunction
  def h[R](pf: Function2[Int, String, R]): Int = 1
  def h[R](pf: PartialFunction[(Double, Double), R]): Int = 2
  assert(h { case (a: Double, b: Double) => 42: Int } == 2)
}
