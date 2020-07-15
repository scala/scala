package tastytest

import scala.annotation.StaticAnnotation

object DefAnnots {

  class argAnnot(arg: Any) extends StaticAnnotation
  class inferAnnot[T](arg: T) extends StaticAnnotation

  def withArgAnnot1(arg: Any @argAnnot(??? : Int)): Any = arg
  def withParamInferAnnot(@inferAnnot(Inner.Foo) arg: Any): Any = arg
  def withAnnotatedAnnot(@Wrapper.annotatedAnnot arg: Any): Any = arg

  object Inner {
    object Foo
  }

  object Wrapper {
    class annot extends inferAnnot(Final.msg)
    @annot class annotatedAnnot extends StaticAnnotation
  }

  object Final {
    final val msg = "msg"
  }

}
