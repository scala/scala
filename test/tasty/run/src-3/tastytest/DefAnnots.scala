package tastytest

import scala.annotation.StaticAnnotation

object DefAnnots {

  class argAnnot(arg: Any) extends StaticAnnotation

  def withArgAnnot1(arg: Any @argAnnot(??? : Int)): Any = arg

}
