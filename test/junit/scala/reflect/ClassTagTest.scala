package scala.reflect

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

class Misc

@RunWith(classOf[JUnit4])
class ClassTagTest {
  def checkNotString[A: ClassTag](a: Any)  = a match { case x: String  => false case x: A => true case _ => false }
  def checkNotInt[A: ClassTag](a: Any)  = a match { case x: Int  => false case x: A => true case _ => false }
  def checkNotLong[A: ClassTag](a: Any) = a match { case x: Long => false case x: A => true case _ => false }

  @Test def checkMisc    = assertTrue(checkNotString[Misc](new Misc))
  @Test def checkString  = assertTrue(checkNotInt[String] ("woele"))
  @Test def checkByte    = assertTrue(checkNotInt[Byte]   (0.toByte))
  @Test def checkShort   = assertTrue(checkNotInt[Short]  (0.toShort))
  @Test def checkChar    = assertTrue(checkNotInt[Char]   (0.toChar))
  @Test def checkInt     = assertTrue(checkNotLong[Int]   (0.toInt))
  @Test def checkLong    = assertTrue(checkNotInt[Long]   (0.toLong))
  @Test def checkFloat   = assertTrue(checkNotInt[Float]  (0.toFloat))
  @Test def checkDouble  = assertTrue(checkNotInt[Double] (0.toDouble))
  @Test def checkBoolean = assertTrue(checkNotInt[Boolean](false))
  @Test def checkUnit    = assertTrue(checkNotInt[Unit]   ({}))

  @Test def t9534: Unit = {
    val ct = implicitly[scala.reflect.ClassTag[Unit]]
    val a1 = ct.newArray(1)
    a1(0) = ()
    val a2 = ct.wrap.newArray(1)
    a2(0) = a1
    val a3 = ct.newArray2(1)
    a3(0) = a1
  }
}
