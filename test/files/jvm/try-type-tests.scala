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
    val n = t.flatMap{ x => assert(false); Try(()) }
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
      case _          => assert(false)
    }
  }

  def testFilterFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.filter{ x => assert(false); true }
  }

  def testRescueSuccess(): Unit = {
    val t = Success(1)
    t.recoverWith{ case x => assert(false); Try(()) }
  }

  def testRescueFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.recoverWith{ case x => Try(1) }
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

  def testSuccessTransform(): Unit = {
    val s = Success(1)
    val succ = (x: Int) => Success(x * 10)
    val fail = (x: Throwable) => Success(0)
    assert(s.transform(succ, fail).get == 10)
  }

  def testFailureTransform(): Unit = {
    val f = Failure(new Exception("foo"))
    val succ = (x: Int) => Success(x * 10)
    val fail = (x: Throwable) => Success(0)
    assert(f.transform(succ, fail).get == 0)
  }

  def testSuccessEither(): Unit = {
    val t = Success(1)
    assert(t.toEither.isRight)
  }

  def testFailureEither(): Unit = {
    val t = Failure(new Exception("foo"))
    assert(t.toEither.isLeft)
  }

  def testFoldSuccess(): Unit = {
    val t = Success(1)
    val res = t.fold("Throws " + _, "Returns " + _)
    assert(res == "Returns 1")
  }

  def testFoldFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val res = t.fold("Throws " + _, "Returns " + _)
    assert(res == "Throws java.lang.Exception: foo")
  }

  def testFoldSuccessFailure(): Unit = {
    val t = Success(1)
    val res = t.fold("Throws " + _, _ => throw new Exception("foo"))
    assert(res == "Throws java.lang.Exception: foo")
  }

  def testFoldFailureFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val res = try {
      t.fold(_ => throw new Exception("bar"), "Returns " + _)
    } catch {
      case e: Throwable => "Throws " + e
    }
    assert(res == "Throws java.lang.Exception: bar")
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
  testSuccessTransform()
  testFailureTransform()
  testSuccessEither()
  testFailureEither()
  testFoldSuccess()
  testFoldFailure()
  testFoldSuccessFailure()
}

object Test
extends App
with TryStandard {
  System.exit(0)
}
