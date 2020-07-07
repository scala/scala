package tastytest

@symbolicAnnot(new tastytest_>>>.Member)
trait Annotated

@rootAnnot(1)
trait RootAnnotated

trait OuterClassAnnotated extends OuterClass {
  @basicAnnot(xyz)
  def foo = 1
}

class ParameterizedAnnotated(@basicAnnot(ParameterizedAnnotated.value) x: Int) {
  def foo = x
}
object ParameterizedAnnotated {
  final val value = 23
}

trait OuterAnnotated extends OuterTrait {
  @innerAnnot(new Inner)
  def foo = 1
}
