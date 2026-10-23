//> using options -Werror

// `A` is sealed and compiled in this run, so whether a type test involving `A` is fruitless is decided once typer has
// seen all children. Every test below warns, also when the test is typed in an argument position, where the typer
// first checks the application speculatively.

sealed trait A
final class A1 extends A
trait B

class C
object C { implicit class RichC(c: C) { def n(b: Boolean): Int = 0; def n(i: Int): Int = 0 } }

object T {
  def one(b: Boolean): Int = 0
  def ov(b: Boolean): Int = 0
  def ov(s: String): Int = 0
  def named(b: Boolean, i: Int = 0): Int = 0
  def oneI(i: Int): Int = 0
  def ovI(i: Int): Int = 0
  def ovI(s: String): Int = 0

  def f0(x: A) = x.isInstanceOf[B]
  def f1(x: A) = one(x.isInstanceOf[B])
  def f2(x: A) = ov(x.isInstanceOf[B])
  def f3(c: C, x: A) = c.n(x.isInstanceOf[B])
  def f4(x: A) = named(i = 1, b = x.isInstanceOf[B])
  def f5(x: A) = s"${x.isInstanceOf[B]}"

  def g0(x: List[A]) = x match { case _: Seq[B] => 0; case _ => 1 }
  def g1(x: List[A]) = oneI(x match { case _: Seq[B] => 0; case _ => 1 })
  def g2(x: List[A]) = ovI(x match { case _: Seq[B] => 0; case _ => 1 })
  def g3(c: C, x: List[A]) = c.n(x match { case _: Seq[B] => 0; case _ => 1 })
}
