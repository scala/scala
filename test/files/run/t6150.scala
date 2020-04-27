object Test {
  import collection.{ immutable, mutable, generic }

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

  val v = immutable.Vector(1, 2, 3)
  val iiv: immutable.IndexedSeq[Int] = immutable.Vector(1, 2, 3)
  val iv: IndexedSeq[Int] = immutable.Vector(1, 2, 3)

  def main(args: Array[String]): Unit = {
    val allCbf = List[generic.CanBuildFrom[Vector[Int],Int,scala.collection.IndexedSeq[Int]]](cbf1, cbf2, cbf3, cbf4, cbf5, cbf6)
    allCbf foreach {
      cbf =>
        check(v.:+(4)(cbf))
    }
    val immIndexSeqCbf = List[generic.CanBuildFrom[immutable.IndexedSeq[Int],Int,scala.collection.IndexedSeq[Int]]](cbf2, cbf3, cbf6)
    immIndexSeqCbf foreach {
      cbf =>
        check(v.:+(4)(cbf))
        check(iiv.:+(4)(cbf))
    }
    val collIndexSeqCbf = List[generic.CanBuildFrom[collection.IndexedSeq[Int],Int,scala.collection.IndexedSeq[Int]]](cbf3)
    collIndexSeqCbf foreach {
      cbf =>
        check(v.:+(4)(cbf))
        check(iiv.:+(4)(cbf))
        check(iv.:+(4)(cbf))
    }
  }
}
