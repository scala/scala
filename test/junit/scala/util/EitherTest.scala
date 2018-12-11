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

  @Test
  def testCollectLeftsWithNoLeft: Unit = {
    val e1 = Right(1)
    val e2 = Right(2)
    val e3 = Right(3)

    assertEquals(Either.collectLefts(e1,e2,e3), Nil)
  }

  @Test
  def testCollectLeftsWithSomeLefts: Unit = {
    val e1 = Right(1)
    val e2 = Left("nan")
    val e3 = Left("nan2")

    assertEquals(Either.collectLefts(e1,e2,e3), Seq("nan", "nan2"))
  }

  @Test
  def testCollectLeftsWithSomeLeftsWithMixedRightTypes: Unit = {
    val e1 = Right(1)
    val e2 = Right("2")
    val e3 = Left("nan")

    assertEquals(Either.collectLefts(e1, e2, e3), Seq("nan"))
  }

  @Test
  def testCollectLeftsWithSomeLeftsWithErrorTrait: Unit = {
    sealed trait Error
    case class NumericError(message: String) extends Error
    case class OtherError(message: String) extends Error

    val e1 = Right(1)
    val e2 = Left(NumericError("nan"))
    val e3 = Left(OtherError("foo"))

    val lefts: Seq[Error] = Either.collectLefts(e1,e2,e3)
    assertEquals(lefts, Seq(NumericError("nan"), OtherError("foo")))
  }

  @Test
  def testCollectRightsWithNoRight: Unit = {
    val e1 = Left(1)
    val e2 = Left(2)
    val e3 = Left(3)

    assertEquals(Either.collectRights(e1,e2,e3), Nil)
  }

  @Test
  def testCollectRightsWithSomeRights: Unit = {
    val e0 = Left("nan")
    val e1 = Right(1)
    val e2 = Right(2)
    val e3 = Left("nan bis")
    val e4 = Right(3)

    assertEquals(Either.collectRights(e0, e1, e2, e3, e4), Seq(1, 2, 3))
  }  
}
