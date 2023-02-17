package tastytest

object ApplySigPoly {

  class Foo {
    def foo(x: Int): Int = x
  }

  private val lookup = java.lang.invoke.MethodHandles.lookup()
  private val mt = java.lang.invoke.MethodType.methodType(classOf[Int], classOf[Int]);

  val mh = lookup.findVirtual(classOf[Foo], "foo", mt)

  val self = new Foo()

  class boxAnnot(val value: Int) extends scala.annotation.Annotation

  @boxAnnot(mh.invokeExact(self, 23): Int)
  def annotatedMethod: Int = 23
}
