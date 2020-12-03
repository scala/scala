package tastytest

import scala.annotation.{StaticAnnotation, targetName}

object SelectWithTarget {

  class defAnnot(arg: Any) extends StaticAnnotation

  @defAnnot(Overloads.foo("hi"))
  def selectFooString: Int = 23

  object Overloads {

    @targetName("fooString")
    def foo(t: => String): Int = t.length

    @targetName("fooInt")
    def foo(t: => Int): Int = t

  }

}
