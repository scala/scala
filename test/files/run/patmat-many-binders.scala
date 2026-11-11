// One case binds more than 64 variables (`Substitution.IndexAbove`), so the substitution of its binders into the
// guard and body is an `IndexedSubstitution`, restricted to the bindings each tree can mention.
import scala.language.reflectiveCalls

sealed trait T
final case class Big(x0: String, x1: String, x2: String, x3: String, x4: String, x5: String, x6: String, x7: String, x8: String, x9: String, x10: String, x11: String, x12: String, x13: String, x14: String, x15: String, x16: String, x17: String, x18: String, x19: String, x20: String, x21: String, x22: String, x23: String, x24: String, x25: String, x26: String, x27: String, x28: String, x29: String, x30: String, x31: String, x32: String, x33: String, x34: String, x35: String, x36: String, x37: String, x38: String, x39: String, x40: String, x41: String, x42: String, x43: String, x44: String, x45: String, x46: String, x47: String, x48: String, x49: String, x50: String, x51: String, x52: String, x53: String, x54: String, x55: String, x56: String, x57: String, x58: String, x59: String, x60: String, x61: String, x62: String, x63: String, x64: String, x65: String, x66: String, x67: String, x68: String, x69: String) extends T
case object Small extends T
trait Holder { type Elem; def elem: Elem }
class StrHolder(val elem: String) extends Holder { type Elem = String }
object Test {
  def sum(t: T): Int = t match {
    case Big(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63, a64, a65, a66, a67, a68, a69) if a0.length + a69.length > 0 =>
      class L { def m: a1.type = a1; def n(y: a2.type): y.type = y }
      val z: a5.type = a5
      val r: AnyRef { def f: a3.type } = new { def f: a3.type = a3 }
      val h: Holder { type Elem = a4.type } = new Holder { type Elem = a4.type; def elem: a4.type = a4 }
      val lst: List[a6.type] = List(a6)
      val ex: Holder { type Elem <: a7.type } = h.asInstanceOf[Holder { type Elem <: a7.type }]
      def poly[Q <: a8.type](q: Q): Q = q
      val fn = (q: a9.type) => q.length
      val anns: String @unchecked = a10
      (a11, a69) match {
        case (p: a11.type, q) if p.length > 0 => p.length + q.length
        case (p, q) => new L().m.length + new L().n(a2).length + z.length + r.f.length + h.elem.length + lst.head.length + ex.elem.length + poly(a8).length + fn(a9) + anns.length + a68.length
      }
    case Big(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63, a64, a65, a66, a67, a68, a69) => a0.length + a1.length + a2.length + a3.length + a4.length + a5.length + a6.length + a7.length + a8.length + a9.length + a10.length + a11.length + a12.length + a13.length + a14.length + a15.length + a16.length + a17.length + a18.length + a19.length + a20.length + a21.length + a22.length + a23.length + a24.length + a25.length + a26.length + a27.length + a28.length + a29.length + a30.length + a31.length + a32.length + a33.length + a34.length + a35.length + a36.length + a37.length + a38.length + a39.length + a40.length + a41.length + a42.length + a43.length + a44.length + a45.length + a46.length + a47.length + a48.length + a49.length + a50.length + a51.length + a52.length + a53.length + a54.length + a55.length + a56.length + a57.length + a58.length + a59.length + a60.length + a61.length + a62.length + a63.length + a64.length + a65.length + a66.length + a67.length + a68.length + a69.length
    case Small => 0
  }
  def lists(xs: List[String]): Int = xs match {
    case a0 :: a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: a11 :: a12 :: a13 :: a14 :: a15 :: a16 :: a17 :: a18 :: a19 :: a20 :: a21 :: a22 :: a23 :: a24 :: a25 :: a26 :: a27 :: a28 :: a29 :: a30 :: a31 :: a32 :: a33 :: a34 :: a35 :: a36 :: a37 :: a38 :: a39 :: a40 :: a41 :: a42 :: a43 :: a44 :: a45 :: a46 :: a47 :: a48 :: a49 :: a50 :: a51 :: a52 :: a53 :: a54 :: a55 :: a56 :: a57 :: a58 :: a59 :: a60 :: a61 :: a62 :: a63 :: a64 :: a65 :: a66 :: a67 :: a68 :: a69 :: rest => a0.length + a1.length + a2.length + a3.length + a4.length + a5.length + a6.length + a7.length + a8.length + a9.length + a10.length + a11.length + a12.length + a13.length + a14.length + a15.length + a16.length + a17.length + a18.length + a19.length + a20.length + a21.length + a22.length + a23.length + a24.length + a25.length + a26.length + a27.length + a28.length + a29.length + a30.length + a31.length + a32.length + a33.length + a34.length + a35.length + a36.length + a37.length + a38.length + a39.length + a40.length + a41.length + a42.length + a43.length + a44.length + a45.length + a46.length + a47.length + a48.length + a49.length + a50.length + a51.length + a52.length + a53.length + a54.length + a55.length + a56.length + a57.length + a58.length + a59.length + a60.length + a61.length + a62.length + a63.length + a64.length + a65.length + a66.length + a67.length + a68.length + a69.length
    case _ => -1
  }
  def main(args: Array[String]): Unit = {
    val big = Big("s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s", "ss", "sss", "s")
    println(sum(big)); println(sum(Small)); println(sum(big.copy(x0 = "", x69 = "")))
    println(lists(List("t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt", "t", "tt"))); println(lists(Nil))
  }
}
