import Impls._

object Test11 {
  trait T1 { type T = macro impl }
  trait T2 extends T1 { class T extends C }
}

object Test12 {
  class C1 { type T = macro impl }
  class C2 extends C1 { class T extends C }
}

// object Test21 {
//   trait T1 { type T = macro impl }
//   trait T2 extends T1 { override class T extends C }
// }

// object Test22 {
//   class C1 { type T = macro impl }
//   class C2 extends C1 { override class T extends C }
// }