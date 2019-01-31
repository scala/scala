// scalac: -Xsource:2.14

case class A private (i: Int)
object A
object ATest {
  def a1: A = A(1) // error: apply is private
  def a2: A = a1.copy(2) // error: copy is private
}

case class B private (i: Int) // no user-defined companion object, should compile
object BTest {
  def b1: B = B(1) // error: apply is private
  def b2: B = b1.copy(2) // error: copy is private
}

object qualified_private {
  case class C private[qualified_private] (i: Int)
  object C

  case class D private[qualified_private] (i: Int) // no user-defined companion object, should compile
}
object QPrivTest {
  import qualified_private._
  def c1: C = C(1) // error: apply is private
  def c2: C = c1.copy(2) // error: copy is private

  def d1: D = D(1) // error: apply is private
  def d2: D = d1.copy(2) // error: copy is private
}

case class E protected (i: Int)
object ETest {
  def e1: E = E(1)
  def e2: E = e2.copy(2) // error: copy is protected
}

object qualified_protected {
  case class F protected[qualified_protected] (i: Int)
}
object QProtTest {
  import qualified_protected._
  def f1: F = F(1)
  def f2: F = f2.copy(2) // error: copy is protected
}


case class OverrideApply private (i: Int)
object OverrideApply {
  def apply(i: Int): OverrideApply = new OverrideApply(i)
}

case class OverrideCopy private (i: Int) {
  def copy(i: Int = i): OverrideCopy = OverrideCopy(i)
}

object OverrideTest {
  def oa = OverrideApply(42).copy(24) // error: copy is still private
  def oc = OverrideCopy(42) // error: apply is still private
}
