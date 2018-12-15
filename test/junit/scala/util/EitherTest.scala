package scala.util

import org.junit.Assert._
import org.junit.Test

class EitherTest {

  @Test def testFlatten: Unit = {
    val  l: Either[String, Either[String, Int]] = Either.Left("pancake")
    val rl: Either[String, Either[String, Int]] = Either.Right(Either.Left("flounder"))
    val rr: Either[String, Either[String, Int]] = Either.Right(Either.Right(7))

    val flatl : Either[String, Int] =  l.flatten
    val flatrl: Either[String, Int] = rl.flatten
    val flatrr: Either[String, Int] = rr.flatten

    assertEquals(Either.Left("pancake"), flatl)
    assertEquals(Either.Left("flounder"), flatrl)
    assertEquals(Either.Right(7), flatrr)
  }

  @Test
  def testWithRight: Unit = {

    def rightSumOrLeftEmpty(l: List[Int]) =
      l.foldLeft(Either.Left("empty").withRight[Int]) {
        case (Left(_), i) => Either.Right(i)
        case (Right(s), i) => Either.Right(s + i)
      }

    assertEquals(rightSumOrLeftEmpty(List(1, 2, 3)), Either.Right(6))
    assertEquals(rightSumOrLeftEmpty(Nil), Either.Left("empty"))
  }

  @Test
  def testWithLeft: Unit = {

    def leftSumOrRightEmpty(l: List[Int]) =
      l.foldLeft(Either.Right("empty").withLeft[Int]) {
        case (Right(_), i) => Either.Left(i)
        case (Left(s), i) => Either.Left(s + i)
      }

    assertEquals(leftSumOrRightEmpty(List(1, 2, 3)), Either.Left(6))
    assertEquals(leftSumOrRightEmpty(Nil), Either.Right("empty"))
  }
}
