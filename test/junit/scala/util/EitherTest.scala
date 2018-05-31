package scala.util

import org.junit.{ Assert, Test }

class EitherTest {

  @Test def testFlatten: Unit = {
    val  l: Either[String, Either[String, Int]] = Left("pancake")
    val rl: Either[String, Either[String, Int]] = Right(Left("flounder"))
    val rr: Either[String, Either[String, Int]] = Right(Right(7))

    val flatl : Either[String, Int] =  l.flatten
    val flatrl: Either[String, Int] = rl.flatten
    val flatrr: Either[String, Int] = rr.flatten

    Assert.assertEquals(Left("pancake"), flatl)
    Assert.assertEquals(Left("flounder"), flatrl)
    Assert.assertEquals(Right(7), flatrr)
    
  }
}