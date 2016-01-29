package scala.issues

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OptionsWithJavaValuesTests {
  import OptionsWithJavaValues._

  // See -> https://issues.scala-lang.org/browse/SI-9634

  @Test
  def t9634(): Unit = {
    // These compile but give NullPointerException in Scala 2.11.7 and 2.12.0-M3
    //
    // Note: SI-9634 does not try to fix this. Instead, it makes giving the type parameter explicitly
    //      unnecessary, since the conversion to a Scala AnyVal is done automatically.
    /*
    assert( Option[Boolean](null.asInstanceOf[java.lang.Boolean]) == None )
    assert( Option[Char](null.asInstanceOf[java.lang.Character]) == None )
    assert( Option[Byte](null.asInstanceOf[java.lang.Byte]) == None )
    assert( Option[Short](null.asInstanceOf[java.lang.Short]) == None )
    assert( Option[Int](null.asInstanceOf[java.lang.Integer]) == None )
    assert( Option[Long](null.asInstanceOf[java.lang.Long]) == None )
    assert( Option[Float](null.asInstanceOf[java.lang.Float]) == None )
    assert( Option[Double](null.asInstanceOf[java.lang.Double]) == None )
    */

    // Check that proper values get through, and they are converted to Scala AnyVals
    //
    def check[A](javaVal: A, scalaVal: AnyVal ): Unit = {
      val v = Option(javaVal).get
      assert( v == scalaVal )
      assert( v.getClass.getName == scalaVal.getClass.getName )
    }

    check(jBoolean, true)     // not "java.lang.Boolean"
    check(jCharacter, 'b')    // not "java.lang.Character"
    check(jByte, 1.toByte)    // not "java.lang.Byte"
    check(jShort, 1.toShort)  // not "java.lang.Short"
    check(jInteger, 1)        // not "java.lang.Integer"
    check(jLong, 1L)          // not "java.lang.Long"
    check(jFloat, 0.0f)       // not "java.lang.Float"
    check(jDouble, 0.0)       // not "java.lang.Double"

    // Using 'Some' on java.lang.* should convert to Scala AnyVals
    //
    assert( Some(jBoolean).get.getClass.getName == "boolean" )
    assert( Some(jCharacter).get.getClass.getName == "char" )
    assert( Some(jByte).get.getClass.getName == "byte" )
    assert( Some(jShort).get.getClass.getName == "short" )
    assert( Some(jInteger).get.getClass.getName == "int" )
    assert( Some(jLong).get.getClass.getName == "long" )
    assert( Some(jFloat).get.getClass.getName == "float" )
    assert( Some(jDouble).get.getClass.getName == "double" )

    // Creating Option with java.lang.* shouldn't be possible, even explicitly
    //
    // It still is, not sure how it could be blocked without burdening normal use of 'Option' (i.e.
    // adding to compile times, complexity etc.)
    /*
    Option[java.lang.Boolean](jBoolean)
    Option[java.lang.Character](jCharacter)
    Option[java.lang.Byte](jByte)
    Option[java.lang.Short](jShort)
    Option[java.lang.Integer](jInteger)
    Option[java.lang.Long](jLong)
    Option[java.lang.Float](jFloat)
    Option[java.lang.Double](jDouble)
    */
  }
}

object OptionsWithJavaValues {
  val jBoolean = new java.lang.Boolean(true)
  val jCharacter = new java.lang.Character('b')
  val jByte = (1.toByte).asInstanceOf[java.lang.Byte]
  val jShort = (1.toShort).asInstanceOf[java.lang.Short]
  val jInteger = new java.lang.Integer(1)
  val jLong = new java.lang.Long(1L)
  val jFloat = new java.lang.Float(0.0)
  val jDouble = new java.lang.Double(0.0)
}