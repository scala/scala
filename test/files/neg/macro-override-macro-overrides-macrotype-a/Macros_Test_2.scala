import Impls._

object Test1 {
  trait T1 { type T = macro impl_b }
  trait T2 extends T1 { type T = macro impl_c }
}

object Test2 {
  class C1 { type T = macro impl_b }
  class C2 extends C1 { type T = macro impl_c }
}

object Test3 {
  trait T1 { type T = macro impl_b }
  trait T2 extends T1 { override type T = macro impl_c }
}

object Test4 {
  class C1 { type T = macro impl_b }
  class C2 extends C1 { override type T = macro impl_c }
}

object Test5 {
  trait T1 { type T = macro impl_b }
  trait T2 extends T1 { override type T() = macro impl_c1 }
}

object Test6 {
  class C1 { type T = macro impl_b }
  class C2 extends C1 { override type T() = macro impl_c1 }
}

object Test7 {
  trait T1 { type T() = macro impl_c1 }
  trait T2 extends T1 { override type T = macro impl_b }
}

object Test8 {
  class C1 { type T() = macro impl_c1 }
  class C2 extends C1 { override type T = macro impl_b }
}

object Test9 {
  trait T1 { type T = macro impl_b; type T(x: String) = macro impl_c3 }
  trait T2 extends T1 { override type T(x: Int) = macro impl_c2 }
}

object Test10 {
  class C1 { type T = macro impl_b }
  class C2 extends C1 { override type T(x: Int) = macro impl_c2 }
}

object Test11 {
  trait T1 { final type T = macro impl_b }
  trait T2 extends T1 { type T = macro impl_c }
}

object Test12 {
  class C1 { final type T = macro impl_b }
  class C2 extends C1 { type T = macro impl_c }
}

object Test13 {
  trait T1 { final type T = macro impl_b }
  trait T2 extends T1 { override type T = macro impl_c }
}

object Test14 {
  class C1 { final type T = macro impl_b }
  class C2 extends C1 { override type T = macro impl_c }
}