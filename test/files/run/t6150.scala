


import collection._



object Test extends App {
  
  val cbf1 = implicitly[generic.CanBuildFrom[Vector[Int], Int, IndexedSeq[Int]]]
  val cbf2 = implicitly[generic.CanBuildFrom[immutable.IndexedSeq[Int], Int, IndexedSeq[Int]]]
  val cbf3 = implicitly[generic.CanBuildFrom[IndexedSeq[Int], Int, IndexedSeq[Int]]]
  
  def check[C](v: C) = {
    assert(v == Vector(1, 2, 3, 4))
    assert(v.isInstanceOf[Vector[_]])
  }
  
  val v = immutable.Vector(1, 2, 3)
  
  check(v.:+(4)(cbf1))
  check(v.:+(4)(cbf2))
  check(v.:+(4)(cbf3))
  
  val iiv: immutable.IndexedSeq[Int] = immutable.Vector(1, 2, 3)
  
  check(iiv.:+(4)(cbf2))
  check(iiv.:+(4)(cbf3))
  
  val iv: IndexedSeq[Int] = immutable.Vector(1, 2, 3)
  
  check(iv.:+(4)(cbf3))
  
}
