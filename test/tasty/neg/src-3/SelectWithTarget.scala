package tastytest

import scala.annotation.{StaticAnnotation, targetName}

object SelectWithTarget {

  class defAnnot(arg: Any) extends StaticAnnotation

  class mctorAnnot(s: String)(i: Int) extends scala.annotation.Annotation

  @defAnnot(Overloads.foo("hi"))
  def selectFooString: Int = 23

  class MultiParamsAnnot {
    @mctorAnnot("a")(23)
    def methodWithAnnotMultipleParams = 47
  }

  object MultiParamsAnnotTpe {
    def foo: Int @mctorAnnot("a")(23) = 47
  }


  object Overloads {

    @targetName("fooString")
    def foo(t: => String): Int = t.length

    @targetName("fooInt")
    def foo(t: => Int): Int = t

  }

}
