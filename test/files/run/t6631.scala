import reflect.ClassTag

object Test extends App {
  def intercept[T <: Throwable : ClassTag](act: => Any) = try {
    act
  } catch {
    case x: Throwable =>
      val cls = implicitly[ClassTag[T]].runtimeClass
      assert(cls.isInstance(x), (x.getClass, x, cls).toString)
  }
  assert(s"""\f\r\n\t""" == "\f\r\n\t")

  import StringContext.InvalidEscapeException
  intercept[InvalidEscapeException](s"""\""")
  intercept[InvalidEscapeException](s"""\x""")
  intercept[InvalidEscapeException](s"\")

}
