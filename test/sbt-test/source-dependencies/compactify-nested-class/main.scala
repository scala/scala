package p1.p2

abstract class B {
  def m(): L
  abstract class L {
    def i: I
    abstract class I
  }
}
class D extends B { override def m() = new L { override case object i extends I } }
class G extends B { override def m() = new L { override case object i extends I } }
