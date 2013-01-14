import Impls._

object Test11 {
  trait T1 { type T <: C }
  trait T2 extends T1 { type T = macro impl }
}

object Test12 {
  class C1 { type T <: C }
  class C2 extends C1 { type T = macro impl }
}

object Test21 {
  trait T1 { type T <: C }
  trait T2 extends T1 { override type T = macro impl }
}

object Test22 {
  class C1 { type T <: C }
  class C2 extends C1 { override type T = macro impl }
}

object Test31 {
  trait T1 { type T <: C }
  trait T2 extends T1 { override type T() = macro impl1 }
}

object Test32 {
  class C1 { type T <: C }
  class C2 extends C1 { override type T() = macro impl1 }
}

object Test41 {
  trait T1 { type T <: C }
  trait T2 extends T1 { override type T[U] = macro impl }
}

object Test42 {
  class C1 { type T <: C }
  class C2 extends C1 { override type T[U] = macro impl }
}

// object Test51 {
//   trait T1 { final type T <: C }
//   trait T2 extends T1 { type T = macro impl }
// }

// object Test52 {
//   class C1 { final type T <: C }
//   class C2 extends C1 { type T = macro impl }
// }

// object Test61 {
//   trait T1 { final type T <: C }
//   trait T2 extends T1 { override type T = macro impl }
// }

// object Test62 {
//   class C1 { final type T <: C }
//   class C2 extends C1 { override type T = macro impl }
// }
