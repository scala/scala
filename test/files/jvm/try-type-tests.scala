import scala.util.{Try, Success, Failure}

// tests the basic combinators on Try
trait TryStandard {

	def testForeachSuccess(): Unit = {
		val t = Success(1)
		var res = 0
		t.foreach(x => res = x * 10)
		assert(res == 10)
	}

	def testForeachFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		t.foreach(x => assert(false))
	}

	def testFlatMapSuccess(): Unit = {
		val t = Success(1)
		val n = t.flatMap(x => Try(x * 10))
		assert(n.get == 10)
	}

	def testFlatMapFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.flatMap{ x => assert(false); Try() }
	}

	def testMapSuccess(): Unit = {
		val t = Success(1)
		val n = t.map(x => x * 10)
		assert(n.get == 10)
	}

	def testMapFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.map(x => assert(false))
	}

	def testFilterSuccessTrue(): Unit = {
		val t = Success(1)
		val n = t.filter(x => x > 0)
		assert(n.get == 1)
	}

	def testFilterSuccessFalse(): Unit = {
		val t = Success(1)
		val n = t.filter(x => x < 0)
		n match {
			case Success(v) => assert(false)
			case Failure(e: NoSuchElementException) => assert(true)
		}
	}

	def testFilterFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.filter{ x => assert(false); true }
	}

	def testRescueSuccess(): Unit = {
		val t = Success(1)
		t.rescue{ case x => assert(false); Try() }
	}

	def testRescueFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.rescue{ case x => Try(1) }
		assert(n.get == 1)
	}

	def testRecoverSuccess(): Unit = {
		val t = Success(1)
		t.recover{ case x => assert(false); 99 }
	}

	def testRecoverFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.recover{ case x => 1 }
		assert(n.get == 1)
	}

	def testFlattenSuccess(): Unit = {
		val f = Failure(new Exception("foo"))
		val t = Success(f)
		assert(t.flatten == f)
	}

	def testFailedSuccess(): Unit = {
		val t = Success(1)
		val n = t.failed
		n match {
			case Failure(e: UnsupportedOperationException) => assert(true)
			case _ => assert(false)
		}
	}

	def testFailedFailure(): Unit = {
		val t = Failure(new Exception("foo"))
		val n = t.failed
		n match {
			case Success(e: Exception) => assert(true)
			case _ => assert(false)
		}
	}

	testForeachSuccess()
	testForeachFailure()
	testFlatMapSuccess()
	testFlatMapFailure()
	testMapSuccess()
	testMapFailure()
	testFilterSuccessTrue()
	testFilterSuccessFalse()
	testFilterFailure()
	testRescueSuccess()
	testRescueFailure()
	testRecoverSuccess()
	testRecoverFailure()
	testFlattenSuccess()
	testFailedSuccess()
	testFailedFailure()
}

// tests that implicit conversions from Try to Either behave as expected
trait TryImplicitConversionTry2Either {

	def testTry2RightMap(): Unit = {
		val t = Success(1)
		val n = t.right.map(x => x * 100)
		assert(n == Right(100))
	}

	def testTry2LeftMap(): Unit = {
		val e = new Exception("foo")
		val t = Failure(e)
		val n = t.left.map(x => x)
		assert(n == Left(e))
	}

	def testTry2FoldSuccess(): Unit = {
		val t = Success(1)
		val n = t.fold(x => assert(false), y => y * 200)
		assert(n == 200)
	}

	def testTry2FoldFailure(): Unit = {
		val e = new Exception("foo")
		val t = Failure(e)
		val n = t.fold(x => x, y => assert(false))
		assert(n == e)
	}

	def testTry2SwapSuccess(): Unit = {
		val t = Success(1)
		val n = t.swap
		assert(n == Left(1))
	}

	def testTry2SwapFailure(): Unit = {
		val e = new Exception("foo")
		val t = Failure(e)
		val n = t.swap
		assert(n == Right(e))
	}

	// def testTry2MergeSucccess(): Unit = {
	// 	val t: Try[Int] = Success(1)
	// 	val n = (t: Either[Any, Any]).t.merge // connecting two implicit conversions
	// 	assert(n == 1)
	// }

	// def testTry2MergeFailure(): Unit = {
	// 	val e = new Exception("foo")
	// 	val t = Failure(e)
	// 	val n = (t: Either[Any, Any]).merge // connecting two implicit conversions
	// 	assert(n == e)
	// }

	testTry2RightMap()
	testTry2LeftMap()
	testTry2FoldSuccess()
	testTry2FoldFailure()
	testTry2SwapSuccess()
	testTry2SwapFailure()
	// testTry2MergeSucccess()
	// testTry2MergeFailure()
}

// tests that implicit conversions from Either to Try behave as expected
trait TryImplicitConversionEither2Try {

	def testRight2FilterSuccessTrue(): Unit = {
	  def expectsTry[U <% Try[Int]](rght: U): Try[Int] = {
			val n = rght.filter(x => x > 0) // this should be converted to a Try
			n
	  }
		val r = Right(1)
    val n = expectsTry(r)
		assert(n == Success(1))
	}

	def testRight2FilterSuccessFalse(): Unit = {
	  def expectsTry[U <% Try[Int]](rght: U): Try[Int] = {
			val n = rght.filter(x => x < 0) // this should be converted to a Try
			n
	  }
		val r = Right(1)
    val n = expectsTry(r)
    n match {
	case Failure(e: NoSuchElementException) => assert(true)
	case _ => assert(false)
    }
	}

	def testLeft2FilterFailure(): Unit = {
	  def expectsTry[U <% Try[Int]](rght: U): Try[Int] = {
			val n = rght.filter(x => x > 0) // this should be converted to a Try
			n
	  }
		val r = Left(new Exception("foo"))
    val n = expectsTry(r)
    n match {
	case Failure(e: Exception) => assert(true)
	case _ => assert(false)
    }
	}

	def testRight2GetSuccess(): Unit = {
	  def expectsTry[U <% Try[Int]](rght: U): Int = {
			val n = rght.get // this should be converted to a Try
			n
	  }
		val r = Right(1)
    val n = expectsTry(r)
    assert(n == 1)
	}

	testRight2FilterSuccessTrue()
	testRight2FilterSuccessFalse()
	testLeft2FilterFailure()
	testRight2GetSuccess()
}

object Test
extends App
with TryStandard
with TryImplicitConversionTry2Either
with TryImplicitConversionEither2Try {
  System.exit(0)
}