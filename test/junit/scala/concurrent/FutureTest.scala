package scala.concurrent

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

import scala.concurrent.ExecutionContext.Implicits.global

/** Tests for Future */
@RunWith(classOf[JUnit4])
class FutureTest {

  case class IntThrowable(x: Int) extends Throwable(x.toString)

  def await[T](f: Future[T]): Either[Int,T] = {
    try {
      val x = Await.result(f, 5.seconds)
      Right(x)
    } catch {
      case IntThrowable(x) => Left(x)
    }
  }

  def assertResult[T](expected: Either[Int,T], f: Future[T]): Unit = {
    assertEquals(expected, await(f))
  }

  @Test
  def testMapTry {
    assertResult(Right(1), Future(1).mapTry(identity))
    assertResult(Right(2), Future(1).mapTry(_ => Success(2)))
    assertResult(Left(3),  Future(1).mapTry(_ => Failure(IntThrowable(3))))
    assertResult(Left(4),  Future(1).mapTry(_ => throw IntThrowable(4)))

    assertResult(Left(1),  Future(throw IntThrowable(1)).mapTry(identity))
    assertResult(Right(2), Future(throw IntThrowable(1)).mapTry(_ => Success(2)))
    assertResult(Left(3),  Future(throw IntThrowable(1)).mapTry(_ => Failure(IntThrowable(3))))
    assertResult(Left(4),  Future(throw IntThrowable(1)).mapTry(_ => throw IntThrowable(4)))

    assertResult(Left(2), Future(1).mapTry {
      case Success(x) => Failure(IntThrowable(x+1))
      case Failure(_) => Failure(IntThrowable(-1))
    })
    assertResult(Left(3), Future(throw IntThrowable(1)).mapTry {
      case Success(_) => Success(-1)
      case Failure(IntThrowable(x)) => Failure(IntThrowable(x+2))
      case Failure(_) => Success(-2)
    })
    assertResult(Right(2), Future(1).mapTry {
      case Success(x) => Success(x+1)
      case Failure(_) => Success(-1)
    })
    assertResult(Right(-1), Future[Int](throw new Exception("x")).mapTry {
      case Success(x) => Success(x+1)
      case Failure(_) => Success(-1)
    })
  }

}
