package tastytest

object TraitParams:
  trait Foo(a: String)
  trait Bar(x: Int, y: String)
  trait Baz(val baz: String)
  trait Qux(val qux: String)
  trait ForOverride(val myParam: String)
  trait ForOverrideFinal(final val myFinalParam: String)

trait TopLevelTraitParams1(a: String)
trait TopLevelTraitParams2(t: Int, u: String)
trait TopLevelTraitParams3(val b: Boolean)
trait TopLevelTraitParams4(val uninstantiated: String)
