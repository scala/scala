package scala.util

import org.junit.Test
import org.junit.Assert.{assertEquals, assertSame, assertTrue}

import scala.tools.testkit.AssertUtil.{assertThrows, fail}

class TryTest {
  /* Test Try's withFilter method, which was added along with the fix for scala/bug#6455 */
  @Test def withFilterFail(): Unit = {
    val fail = for (x <- Try(1) if x > 1) yield x
    assertTrue(fail.isFailure)
  }

  @Test def withFilterSuccess(): Unit = {
    val successfulOne = for (x <- Try(1) if x >= 1) yield x
    assertEquals(Success(1), successfulOne)
  }

  @Test def withFilterFlatMap(): Unit = {
    val successFlatMap = for (x <- Try(1) if x >= 1; y <- Try(2) if x < y) yield x
    assertEquals(Success(1), successFlatMap)
  }

  @Test def withFilterForeach(): Unit = {
    var ok = false
    for (x <- Try(1) if x == 1) ok = x == 1
    assertTrue(ok)
  }

  class MyException extends Exception
  val e = new Exception("this is an exception")

  @Test def `catch exceptions and lift into the Try type`(): Unit = {
    assertEquals(Success(1), Try[Int](1))
    assertEquals(Failure(e), Try[Int] { throw e })
  }

  @Test def recoverWith(): Unit = {
    assertEquals(Success(1), Success(1).recoverWith { case _ => Success(2) })
    assertEquals(Success(2), Failure(e).recoverWith { case _ => Success(2) })
    assertEquals(Failure(e), Failure(e).recoverWith { case _ => Failure(e) })
  }

  @Test def getOrElse(): Unit = {
    assertEquals(1, Success(1) getOrElse 2)
    assertEquals(2, Failure(e) getOrElse 2)
  }

  @Test def orElse(): Unit = {
    assertEquals(Success(1), Success(1) orElse Success(2))
    assertEquals(Success(2), Failure(e) orElse Success(2))
  }

  @Test def `map when there is no exception`(): Unit = {
    assertEquals(Success(2), Success(1).map(1 + _))
    assertEquals(Failure(e), Failure[Int](e).map(1 + _))
  }

  @Test def `map when there is an exception`(): Unit = {
    assertEquals(Failure(e), Success(1).map(_ => throw e))
    val e2 = new Exception
    assertEquals(Failure(e), Failure[Int](e).map(_ => throw e2))
  }

  @deprecated("ThreadDeath is deprecated on JDK 20", "")
  @Test def `map when there is a fatal exception`(): Unit = {
    val e3 = new ThreadDeath
    assertThrows[ThreadDeath] {
      Success(1) map (_ => throw e3)
    }
  }

  @Test def `flatMap when there is no exception`(): Unit = {
    assertEquals(Success(2), Success(1).flatMap(x => Success(1 + x)))
    assertEquals(Failure(e), Failure[Int](e).flatMap(x => Success(1 + x)))
  }

  @Test def `flatMap when there is an exception`(): Unit = {
    assertEquals(Failure(e), Success(1).flatMap[Int](_ => throw e))

    val e2 = new Exception
    assertEquals(Failure(e), Failure[Int](e).flatMap[Int](_ => throw e2))
  }
  @deprecated("ThreadDeath is deprecated on JDK 20", "")
  @Test def `flatMap when there is a fatal exception`(): Unit = {
    val e3 = new ThreadDeath
    assertThrows[ThreadDeath] {
      Success(1).flatMap[Int](_ => throw e3)
    }
  }
  @Test def `flatten is a Success(Success)`(): Unit = {
    assertEquals(Success(1), Success(Success(1)).flatten)
  }
  @Test def `flatten is a Success(Failure)`(): Unit = {
    val e = new Exception
    assertEquals(Failure(e), Success(Failure(e)).flatten)
  }
  @Test def `flatten is a Throw`(): Unit = {
    val e = new Exception
    assertEquals(Failure(e), Failure[Try[Int]](e).flatten)
  }
  @Test def `for with no Failure values`(): Unit = {
    val result = for {
      i <- Success(1)
      j <- Success(1)
    } yield (i + j)
    assertEquals(Success(2), result)
  }
  @Test def `for with Failure values throws before`(): Unit = {
    val result = for {
      i <- Failure[Int](e)
      j <- Success(1)
    } yield (i + j)
    assertEquals(Failure(e), result)
  }
  @Test def `for with Failure values throws after`(): Unit = {
    val result = for {
      i <- Success(1)
      j <- Failure[Int](e)
    } yield (i + j)
    assertEquals(Failure(e), result)
  }
  @Test def `for with Failure values returns the FIRST Failure`(): Unit = {
    val e2 = new Exception
    val result = for {
      i <- Failure[Int](e)
      j <- Failure[Int](e2)
    } yield (i + j)
    assertEquals(Failure(e), result)
  }

  @Test def testForeachSuccess(): Unit = {
    val t = Success(1)
    var res = 0
    t.foreach(x => res = x * 10)
    assertEquals(10, res)
  }

  @Test def testForeachFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    t.foreach(x => fail(t.toString))
  }

  @Test def testFlatMapSuccess(): Unit = {
    val t = Success(1)
    val n = t.flatMap(x => Try(x * 10))
    assertEquals(10, n.get)
  }

  @Test def testFlatMapFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.flatMap { x => fail(t.toString); Try(()) }
    assertTrue(n.isFailure)
  }

  @Test def testMapSuccess(): Unit = {
    val t = Success(1)
    val n = t.map(x => x * 10)
    assertEquals(10, n.get)
  }

  @Test def testMapFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    t.map(x => fail(t.toString)): Unit
  }

  @Test def testFilterSuccessTrue(): Unit = {
    val t = Success(1)
    val n = t.filter(x => x > 0)
    assertEquals(1, n.get)
  }

  @Test def testFilterSuccessFalse(): Unit = {
    val t = Success(1)
    val n = t.filter(x => x < 0)
    n match {
      case Success(_) => fail(n.toString)
      case Failure(_: NoSuchElementException) => ()
      case _          => fail(n.toString)
    }
  }

  @Test def testFilterFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.filter { x => fail(t.toString) ; true }
    assertTrue(n.isFailure)
  }

  @Test def testRescueSuccess(): Unit = {
    val t = Success(1)
    t.recoverWith { case x => fail(t.toString) ; Try(-1) }
  }

  @Test def testRescueFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.recoverWith { case x => Try(1) }
    assertEquals(1, n.get)
  }

  @Test def testRecoverSuccess(): Unit = {
    val t = Success(1)
    val n = t.recover { case x => fail(t.toString); 99 }
    assertEquals(1, n.get)
  }

  @Test def testRecoverWithSuccess(): Unit = {
    val t = Success(1)
    val n = t recoverWith { case _ => fail(t.toString); Success(99) }
    assertEquals(1, n.get)
  }

  @Test def testRecoverFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.recover { case x => 1 }
    assertEquals(1, n.get)
  }

  @Test def testRecoverWithFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val recovered = Failure(new Exception("bar"))
    val n = t.recoverWith { case _ => Success(1) }
    assertEquals(1, n.get)
    val Failure(f) = t.recoverWith { case _ => recovered}: @unchecked
    assertSame(recovered.exception, f)
  }

  @Test def testFlattenSuccess(): Unit = {
    val f = Failure(new Exception("foo"))
    val t = Success(f)
    assertEquals(f, t.flatten)
  }

  @Test def testFailedSuccess(): Unit = {
    val t = Success(1)
    val n = t.failed
    n match {
      case Failure(_: UnsupportedOperationException) =>
      case _ => fail(n.toString)
    }
  }

  @Test def testFailedFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val n = t.failed
    n match {
      case Success(_: Exception) =>
      case _ => fail(n.toString)
    }
  }

  @Test def testSuccessTransform(): Unit = {
    val s = Success(1)
    val succ = (x: Int) => Success(x * 10)
    val fail = (x: Throwable) => Success(0)
    assertEquals(10, s.transform(succ, fail).get)
  }

  @Test def testFailureTransform(): Unit = {
    val f = Failure(new Exception("foo"))
    val succ = (x: Int) => Success(x * 10)
    val fail = (x: Throwable) => Success(0)
    assertEquals(0, f.transform(succ, fail).get)
  }

  @Test def testSuccessEither(): Unit = {
    val t = Success(1)
    assertTrue(t.toEither.isRight)
  }

  @Test def testFailureEither(): Unit = {
    val t = Failure(new Exception("foo"))
    assertTrue(t.toEither.isLeft)
  }

  @Test def testFoldSuccess(): Unit = {
    val t = Success(1)
    val res = t.fold("Throws " + _, "Returns " + _)
    assertEquals("Returns 1", res)
  }

  @Test def testFoldFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val res = t.fold("Throws " + _, "Returns " + _)
    assertEquals("Throws java.lang.Exception: foo", res)
  }

  @Test def testFoldSuccessFailure(): Unit = {
    val t = Success(1)
    val res = t.fold("Throws " + _, _ => throw new Exception("foo"))
    assertEquals("Throws java.lang.Exception: foo", res)
  }

  @Test def testFoldFailureFailure(): Unit = {
    val t = Failure(new Exception("foo"))
    val res = try {
      t.fold(_ => throw new Exception("bar"), "Returns " + _)
    } catch {
      case e: Throwable => "Throws " + e
    }
    assertEquals("Throws java.lang.Exception: bar", res)
  }
}
