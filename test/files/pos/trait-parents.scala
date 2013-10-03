trait Bip extends Any
trait Foo extends Any
trait Bar extends AnyRef
trait Quux

object Test {
  def f(x: Bip) = 1
  def g1(x: Foo with Bip) = f(x)

  def main(args: Array[String]): Unit = {
    f(new Bip with Foo { })
    f(new Foo with Bip { })
    g1(new Bip with Foo { })
    g1(new Foo with Bip { })
  }
}
