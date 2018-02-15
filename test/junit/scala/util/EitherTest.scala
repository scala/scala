package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class EitherTest {

  @Test
  def testLeft: Unit = {

    def rightSumOrLeftEmpty(l: List[Int]) =
      l.foldLeft(Either.left[String, Int]("empty")) {
        case (Left(_), i) => Right(i)
        case (Right(s), i) => Right(s + i)
      }

    assertEquals(rightSumOrLeftEmpty(List(1, 2, 3)), Right(6))
    assertEquals(rightSumOrLeftEmpty(Nil), Left("empty"))
  }

  @Test
  def testRight: Unit = {

    def leftSumOrRightEmpty(l: List[Int]) =
      l.foldLeft(Either.right[Int, String]("empty")) {
        case (Right(_), i) => Left(i)
        case (Left(s), i) => Left(s + i)
      }

    assertEquals(leftSumOrRightEmpty(List(1, 2, 3)), Left(6))
    assertEquals(leftSumOrRightEmpty(Nil), Right("empty"))
  }
}
