package tastytest

object TraitParams:

  trait Foo(val s: String, val i: Int)

  private class Bar extends Foo("I am Bar", 23)

  val foo: Foo = new Bar

trait TopLevelTraitWithParams(val s: String, val i: Int)
final class ClassExtendingTopLevelTraitWithParams
  extends TopLevelTraitWithParams("I am ClassExtendingTopLevelTraitWithParams", 47)
