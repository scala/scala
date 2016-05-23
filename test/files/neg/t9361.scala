abstract class Foo[Tc[_]] { def tc: Tc[_] }
object Foo {
  def foo[Tc[_]](): Foo[Tc] { type T = Nothing } =
    new Foo { def tc = null.asInstanceOf[Tc[_]] }
}
