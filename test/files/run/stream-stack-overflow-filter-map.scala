import collection.{IterableOps, BuildFrom}

object Test extends App {
  def testStreamPred(s: LazyList[Int])(p: Int => Boolean): Unit = {
    val res1 = s withFilter p
    val res2 = s filter p

    val expected = s.toSeq filter p

    val fMapped1 = res1.flatMap(Seq(_))
    val fMapped2 = res2.flatMap(Seq(_))
    assert(fMapped1 == fMapped2)
    assert(fMapped1.toSeq == expected)

    val mapped1 = res1 map (_ + 1)
    val mapped2 = res2 map (_ + 1)
    assert(mapped1 == mapped2)
    assert(mapped1.toSeq == (expected map (_ + 1)))

    assert((res1 map identity).toSeq == res2.toSeq)
  }

  def testStream(s: LazyList[Int]): Unit = {
    testStreamPred(s)(_ => false)
    testStreamPred(s)(_ => true)
    testStreamPred(s)(_ % 2 == 0)
    testStreamPred(s)(_ % 3 == 0)
  }

  //Reduced version of the test case - either invocation used to cause a stack
  //overflow before commit 80b3f433e5536d086806fa108ccdfacf10719cc2.
  val resFMap = (1 to 10000).to(LazyList) withFilter (_ => false) flatMap (Seq(_))
  val resMap = (1 to 10000).to(LazyList) withFilter (_ => false) map (_ + 1)

  //Complete test case for withFilter + map/flatMap, as requested by @axel22.
  for (j <- (0 to 3) :+ 10000) {
    val stream = (1 to j).to(LazyList)
    assert(stream.toSeq == (1 to j).toSeq)
    testStream(stream)
  }
}
