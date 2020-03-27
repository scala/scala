// scalac: -Xsource:3

case class A private (i: Int)
object A {
  def a = A(1).copy(2) // apply and copy are accessible in companion
}

case class B private (i: Int) { // no user-defined companion object, should compile
  def b = B(1).copy(2) // apply and copy are accessible
}

object qualified_private {
  case class A private[qualified_private] (i: Int)
  object A {
    def a = A(1).copy(2) // apply and copy are accessible in companion
  }

  def a = A(1).copy(2) // apply and copy are accessible in qualified_private object

  case class B private[qualified_private] (i: Int) { // no user-defined companion object, should compile
    def b = B(1).copy(2) // apply and copy are accessible
  }

  def b = B(1).copy(2) // apply and copy are accessible in qualified_private object
}

case class C protected (i: Int)
class CSub extends C(1) {
  def c = copy(2) // copy is accessible in subclass
}
object CTest {
  def c = C(1) // apply is public
}

object qualified_protected {
  case class C protected[qualified_protected] (i: Int)
  class CSub extends C(1) {
    def c = copy(2) // copy is accessible in subclass
  }
  object CTest {
    def c = C(1) // apply is public
    def checkExtendsFunction: Int => C = C // companion extends (Int  => C)
  }

  def c = C(1).copy(2)
}
object CQualifiedTest {
  def c = qualified_protected.C(1) // apply is public
}


case class OverrideApply private (i: Int)
object OverrideApply {
  def apply(i: Int): OverrideApply = new OverrideApply(i)
}

case class OverrideCopy private (i: Int) {
  def copy(i: Int = i): OverrideCopy = OverrideCopy(i)
}

object OverrideTest {
  def oa = OverrideApply(42) // overridden apply is public
  def oc(o: OverrideCopy) = o.copy(42) // overridden copy is public
}
