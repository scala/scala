import Impls._

trait T1 {
  type T = macro impl
}

trait T2 extends T1 {
  abstract override type T = macro impl
}