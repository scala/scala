import Impls._

object Test11 {
  trait T1 { type T = macro impl }
  trait T2 extends T1 { type T = C }
}

object Test12 {
  class C1 { type T = macro impl }
  class C2 extends C1 { type T = C }
}

object Test21 {
  trait T1 { type T = macro impl }
  trait T2 extends T1 { override type T = C }
}

object Test22 {
  class C1 { type T = macro impl }
  class C2 extends C1 { override type T = C }
}