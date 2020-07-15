package tastytest

import scala.annotation.StaticAnnotation

object DefAnnots {

  class argAnnot(arg: Any) extends StaticAnnotation

  var global = 3

  inline def foo = 3

  def withArgAnnotThrow(arg: Any @argAnnot(throw new AssertionError("?"))): Any = arg
  def withArgAnnotLoop(arg: Any @argAnnot(while (true) {})): Any = arg
  def withArgAnnotAssign(arg: Any @argAnnot(DefAnnots.global = 0)): Any = arg
  def withArgAnnotLambda(arg: Any @argAnnot((x: Int) => x + DefAnnots.global)): Any = arg // lambdas desugar to blocks
  def withArgAnnotInlined(arg: Any @argAnnot(DefAnnots.foo)): Any = arg

}
