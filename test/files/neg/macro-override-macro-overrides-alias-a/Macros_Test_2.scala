import Impls._

object Test11 {
  trait T11 { type T = C }
  trait T12 extends T11 { type T = macro impl }
}

object Test12 {
  class C11 { type T = C }
  class C12 extends C11 { type T = macro impl }
}

object Test21 {
  trait T11 { type T = C }
  trait T12 extends T11 { override type T = macro impl }
}

object Test22 {
  class C11 { type T = C }
  class C12 extends C11 { override type T = macro impl }
}

object Test31 {
  trait T11 { type T = C }
  trait T12 extends T11 { override type T() = macro impl1 }
}

object Test32 {
  class C11 { type T = C }
  class C12 extends C11 { override type T() = macro impl1 }
}

object Test41 {
  trait T11 { type T = C }
  trait T12 extends T11 { override type T[U] = macro impl }
}

object Test42 {
  class C11 { type T = C }
  class C12 extends C11 { override type T[U] = macro impl }
}

object Test51 {
  trait T11 { final type T = C }
  trait T12 extends T11 { type T = macro impl }
}

object Test52 {
  class C11 { final type T = C }
  class C12 extends C11 { type T = macro impl }
}

object Test61 {
  trait T11 { final type T = C }
  trait T12 extends T11 { override type T = macro impl }
}

object Test62 {
  class C11 { final type T = C }
  class C12 extends C11 { override type T = macro impl }
}
