import scala.language.reflectiveCalls

object Test extends App {
  val theClass = classOf[AnnotationTest]

  def annotation[T <: java.lang.annotation.Annotation](annotationClass: Class[T], methodName: String): T =
    theClass.getDeclaredMethod(methodName)
      .getAnnotation[T](annotationClass)

  def check[T, U <: java.lang.annotation.Annotation { def value(): T } ](annotationClass: Class[U], methodName: String, expected: T): Unit = {
    val a = annotation(annotationClass, methodName)
    assert(a != null, s"No annotation of type $annotationClass found on method $methodName")
    assert(a.value() == expected, s"Actual value of annotation $a on $methodName was not of expected value $expected")
  }

  check(classOf[BooleanAnnotation], "test1", Constants.BooleanTrue)
  check(classOf[ByteAnnotation], "test1", Constants.Byte)
  check(classOf[CharAnnotation], "test1", Constants.Char)
  check(classOf[ShortAnnotation], "test1", Constants.Short)
  check(classOf[IntAnnotation], "test1", Constants.Int)
  check(classOf[LongAnnotation], "test1", Constants.Long)
  check(classOf[FloatAnnotation], "test1", Constants.Float)
  check(classOf[DoubleAnnotation], "test1", Constants.Double)
  check(classOf[StringAnnotation], "test1", Constants.String)

  check(classOf[BooleanAnnotation], "test2", Constants.InvertedBoolean)
  check(classOf[ByteAnnotation], "test2", Constants.NegativeByte)
  // no negative char possible
  check(classOf[ShortAnnotation], "test2", Constants.NegativeShort)
  check(classOf[IntAnnotation], "test2", Constants.NegativeInt)
  check(classOf[LongAnnotation], "test2", Constants.NegativeLong)
  check(classOf[FloatAnnotation], "test2", Constants.NegativeFloat)
  check(classOf[DoubleAnnotation], "test2", Constants.NegativeDouble)
  check(classOf[StringAnnotation], "test2", Constants.NegativeString)

  check(classOf[BooleanAnnotation], "test3", Constants.BooleanFalse)
  check(classOf[ByteAnnotation], "test3", Constants.LiteralCharAsByte)
  check(classOf[CharAnnotation], "test3", Constants.LiteralChar)
  check(classOf[ShortAnnotation], "test3", Constants.LiteralCharAsShort)
  check(classOf[IntAnnotation], "test3", Constants.LiteralCharAsInt)
  check(classOf[LongAnnotation], "test3", Constants.LiteralCharAsLong)

  check(classOf[LongAnnotation], "test4", Constants.LiteralIntAsLong)
}
