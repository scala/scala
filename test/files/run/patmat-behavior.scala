package s {
  sealed trait C[+A]

  case class C00[+A]() extends C[A]
  case class C10[+A](x: A) extends C[A]
  case class C20[+A](x: A, y: A) extends C[A]
  case class C01[+A](xs: A*) extends C[A]
  case class C11[+A](x: A, ys: A*) extends C[A]
  case class C21[+A](x: A, y: A, zs: A*) extends C[A]

  object E00 { def unapply[A](x: Any): Boolean                   = ??? }
  object E10 { def unapply[A](x: Any): Option[A]                 = ??? }
  object E20 { def unapply[A](x: Any): Option[(A, A)]            = ??? }
  object E01 { def unapplySeq[A](x: Any): Option[Seq[A]]         = ??? }
  object E11 { def unapplySeq[A](x: Any): Option[(A, Seq[A])]    = ??? }
  object E21 { def unapplySeq[A](x: Any): Option[(A, A, Seq[A])] = ??? }

  object F00 { def unapply[A](x: C[A]): Boolean                   = ??? }
  object F10 { def unapply[A](x: C[A]): Option[A]                 = ??? }
  object F20 { def unapply[A](x: C[A]): Option[(A, A)]            = ??? }
  object F01 { def unapplySeq[A](x: C[A]): Option[Seq[A]]         = ??? }
  object F11 { def unapplySeq[A](x: C[A]): Option[(A, Seq[A])]    = ??? }
  object F21 { def unapplySeq[A](x: C[A]): Option[(A, A, Seq[A])] = ??? }

  object G00 { def unapply[A](x: C00[A]): Boolean                   = ??? }
  object G10 { def unapply[A](x: C10[A]): Option[A]                 = ??? }
  object G20 { def unapply[A](x: C20[A]): Option[(A, A)]            = ??? }
  object G01 { def unapplySeq[A](x: C01[A]): Option[Seq[A]]         = ??? }
  object G11 { def unapplySeq[A](x: C11[A]): Option[(A, Seq[A])]    = ??? }
  object G21 { def unapplySeq[A](x: C21[A]): Option[(A, A, Seq[A])] = ??? }
}
import s._

package pos {
  object Test {
    def ga1(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga2(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga3(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga4(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga5(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga6(x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }

    def gb1[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb2[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb3[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb4[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb5[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb6[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }

    def gc1[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc2[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc3[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc4[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc5[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc6[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }

    def gd1[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gd2[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gd3[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gd4[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gd5[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gd6[A, B <: C[A]](x: B) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
  }
}

package neg {
  object Fail {
    def gb1[A](x: C00[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb2[A](x: C10[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb3[A](x: C20[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb4[A](x: C01[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb5[A](x: C11[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }
    def gb6[A](x: C21[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys @ _*) => x ; case E21(x, y, zs @ _*) => x }

    def gc1[A](x: C00[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc2[A](x: C10[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc3[A](x: C20[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc4[A](x: C01[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc5[A](x: C11[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }
    def gc6[A](x: C21[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys @ _*) => x ; case F21(x, y, zs @ _*) => x }

    def gd1[A](x: C00[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
    def gd2[A](x: C10[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
    def gd3[A](x: C20[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
    def gd4[A](x: C01[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
    def gd5[A](x: C11[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
    def gd6[A](x: C21[A]) = x match { case G00() => ??? ; case G10(x) => x ; case G20(x, y) => x ; case G01(xs @ _*) => xs.head ; case G11(x, ys @ _*) => x ; case G21(x, y, zs @ _*) => x }
  }
}

object Test {
  def main(args: Array[String]): Unit = {

  }
}
