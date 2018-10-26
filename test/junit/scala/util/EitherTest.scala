package scala.util

import org.junit.Assert._
import org.junit.Test

class EitherTest {

  @Test def testFlatten: Unit = {
    val  l: Either[String, Either[String, Int]] = Left("pancake")
    val rl: Either[String, Either[String, Int]] = Right(Left("flounder"))
    val rr: Either[String, Either[String, Int]] = Right(Right(7))

    val flatl : Either[String, Int] =  l.flatten
    val flatrl: Either[String, Int] = rl.flatten
    val flatrr: Either[String, Int] = rr.flatten

    assertEquals(Left("pancake"), flatl)
    assertEquals(Left("flounder"), flatrl)
    assertEquals(Right(7), flatrr)
  }

  @Test
  def testWithRight: Unit = {

    def rightSumOrLeftEmpty(l: List[Int]) =
      l.foldLeft(Left("empty").withRight[Int]) {
        case (Left(_), i) => Right(i)
        case (Right(s), i) => Right(s + i)
      }

    assertEquals(rightSumOrLeftEmpty(List(1, 2, 3)), Right(6))
    assertEquals(rightSumOrLeftEmpty(Nil), Left("empty"))
  }

  @Test
  def testWithLeft: Unit = {

    def leftSumOrRightEmpty(l: List[Int]) =
      l.foldLeft(Right("empty").withLeft[Int]) {
        case (Right(_), i) => Left(i)
        case (Left(s), i) => Left(s + i)
      }

    assertEquals(leftSumOrRightEmpty(List(1, 2, 3)), Left(6))
    assertEquals(leftSumOrRightEmpty(Nil), Right("empty"))
  }
}
