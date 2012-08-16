object Test {
  import collection.{ immutable, mutable, generic }
  def TheOneTrueCBF = collection.IndexedSeq.ReusableCBF

  val cbf1 = implicitly[generic.CanBuildFrom[immutable.Vector[Int], Int, collection.IndexedSeq[Int]]]
  val cbf2 = implicitly[generic.CanBuildFrom[immutable.IndexedSeq[Int], Int, collection.IndexedSeq[Int]]]
  val cbf3 = implicitly[generic.CanBuildFrom[collection.IndexedSeq[Int], Int, collection.IndexedSeq[Int]]]

  val cbf4 = implicitly[generic.CanBuildFrom[immutable.Vector[Int], Int, immutable.IndexedSeq[Int]]]
  val cbf5 = implicitly[generic.CanBuildFrom[immutable.Vector[Int], Int, immutable.Vector[Int]]]
  val cbf6 = implicitly[generic.CanBuildFrom[immutable.IndexedSeq[Int], Int, immutable.IndexedSeq[Int]]]

  def check[C](v: C) = {
    assert(v == Vector(1, 2, 3, 4))
    assert(v.isInstanceOf[Vector[_]])
  }
  def checkRealMccoy(x: AnyRef) = {
    assert(x eq TheOneTrueCBF, cbf1)
  }

  val v = immutable.Vector(1, 2, 3)
  val iiv: immutable.IndexedSeq[Int] = immutable.Vector(1, 2, 3)
  val iv: IndexedSeq[Int] = immutable.Vector(1, 2, 3)

  def main(args: Array[String]): Unit = {
    List(cbf1, cbf2, cbf3, cbf4, cbf5, cbf6) foreach checkRealMccoy
    check(v.:+(4)(cbf1))
    check(v.:+(4)(cbf2))
    check(v.:+(4)(cbf3))

    check(iiv.:+(4)(cbf2))
    check(iiv.:+(4)(cbf3))

    check(iv.:+(4)(cbf3))
  }
}
