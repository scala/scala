import Impls._

trait T1 {
  final type T = macro impl0
  type T(x: Int) = macro impl1
  final type T(x: String) = macro impl2
}

trait T2 extends T1 {
  override type T(x: Int) = macro impl1
}

object Test extends App