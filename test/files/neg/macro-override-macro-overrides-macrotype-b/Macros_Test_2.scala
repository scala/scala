import Impls._

object Test1 {
  trait T1 { type T <: B }
  trait T2 extends T1 { type T = macro impl_c }
  object M extends T2
  type T = M.T
}

object Test2 {
  trait T1 { type T <: C }
  trait T2 extends T1 { type T = macro impl_b }
  object M extends T2
  type T = M.T
}

object Test3 {
  trait T1 { type T = macro impl_b }
  trait T2 extends T1 { override type T = macro impl_c }
  object M extends T2
  type T = M.T
}

object Test4 {
  trait T1 { type T = macro impl_c }
  trait T2 extends T1 { override type T = macro impl_b }
  object M extends T2
  type T = M.T
}
