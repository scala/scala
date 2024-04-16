//> using options -Werror -Ystop-after:patmat -Ypatmat-exhaust-depth 30
sealed trait Phantom[A] {}

object Phantom {
  type TypeA
}

sealed abstract class ThingType(val v: Int)

private object ThingType {
  case object A extends ThingType(1)
  case object B extends ThingType(-10)
  case object C extends ThingType(-5)
  case object D extends ThingType(15)
  case object E extends ThingType(-16)
}

sealed abstract class Thing[A](val thingType: ThingType)

object Thing {
  sealed abstract class ThingA[A] extends Thing[A](ThingType.A)
  sealed abstract class ThingB[A] extends Thing[A](ThingType.B)
  sealed abstract class ThingC[A] extends Thing[A](ThingType.C)
  sealed abstract class ThingD[A] extends Thing[A](ThingType.D)
  sealed abstract class ThingE[A] extends Thing[A](ThingType.E)
}

object Stuff extends Phantom[Phantom.TypeA] {
  import Phantom.TypeA
  import Thing._

  sealed abstract class OM(val id: String) extends ThingA[TypeA] {
    def linkedA: ThingA[TypeA]
  }

  case object OM1L extends ThingA[TypeA]

  case object OM1 extends OM("1") {
    override def linkedA: ThingA[TypeA] = OM1L
  }

  case object OM2L extends ThingA[TypeA]

  case object OM2 extends OM("2") {
    override def linkedA: ThingA[TypeA] = OM2L
  }

  case object OM3L extends ThingA[TypeA]

  case object OM3 extends OM("3") {
    override def linkedA: ThingA[TypeA] = OM3L
  }

  case object OM4L extends ThingA[TypeA]

  case object OM4 extends OM("4") {
    override def linkedA: ThingA[TypeA] = OM4L
  }

  case object OM5L extends ThingA[TypeA]

  case object OM5 extends OM("5") {
    override def linkedA: ThingA[TypeA] = OM5L
  }

  case object OM6L extends ThingA[TypeA]

  case object OM6 extends OM("6") {
    override def linkedA: ThingA[TypeA] = OM6L
  }

  case object OM7L extends ThingA[TypeA]

  case object OM7 extends OM("7") {
    override def linkedA: ThingA[TypeA] = OM7L
  }

  case object A1 extends ThingA[TypeA]

  case object A2 extends ThingA[TypeA]

  case object A3 extends ThingA[TypeA]

  sealed trait AA extends ThingA[TypeA] {
    def linkedD: ThingD[TypeA]
  }

  case object A4 extends AA {
    override val linkedD: ThingD[TypeA] = A4L
  }

  case object A5 extends AA {
    override val linkedD: ThingD[TypeA] = A5L
  }

  case object A6 extends AA {
    override val linkedD: ThingD[TypeA] = A6L
  }

  case object A7 extends AA {
    override val linkedD: ThingD[TypeA] = A7L
  }

  case object A8 extends AA {
    override val linkedD: ThingD[TypeA] = A8L
  }

  case object A9 extends AA {
    override val linkedD: ThingD[TypeA] = A9L
  }

  case object A10 extends AA {
    override val linkedD: ThingD[TypeA] = A10L
  }

  case object A11 extends AA {
    override val linkedD: ThingD[TypeA] = A11L
  }

  case object A12 extends AA {
    override val linkedD: ThingD[TypeA] = A12L
  }

  case object A13 extends AA {
    override val linkedD: ThingD[TypeA] = A13L
  }

  case object A14 extends AA {
    override val linkedD: ThingD[TypeA] = A14L
  }

  case object A15 extends AA {
    override val linkedD: ThingD[TypeA] = A15L
  }

  sealed abstract class G(val id: String) extends ThingA[TypeA] {
    def linkedG: ThingA[TypeA]
  }

  case object G1L extends ThingA[TypeA]

  case object G1 extends G("1") {
    override def linkedG: ThingA[TypeA] = G1L
  }

  case object G2L extends ThingA[TypeA]

  case object G2 extends G("2") {
    override def linkedG: ThingA[TypeA] = G2L
  }

  case object G3L extends ThingA[TypeA]

  case object G3 extends G("3") {
    override def linkedG: ThingA[TypeA] = G3L
  }

  case object G4L extends ThingA[TypeA]

  case object G4 extends G("4") {
    override def linkedG: ThingA[TypeA] = G4L
  }

  case object G5L extends ThingA[TypeA]

  case object G5 extends G("%") {
    override def linkedG: ThingA[TypeA] = G5L
  }

  case object G6L extends ThingA[TypeA]

  case object G6 extends G("6") {
    override def linkedG: ThingA[TypeA] = G6L
  }

  case object G7L extends ThingA[TypeA]

  case object G7 extends G("7") {
    override def linkedG: ThingA[TypeA] = G7L
  }

  case object G8L extends ThingA[TypeA]

  case object G8 extends G("8") {
    override def linkedG: ThingA[TypeA] = G8L
  }

  case object G9L extends ThingA[TypeA]

  case object G9 extends G("9") {
    override def linkedG: ThingA[TypeA] = G9L
  }

  case object G10L extends ThingA[TypeA]

  case object G10 extends G("10") {
    override def linkedG: ThingA[TypeA] = G10L
  }

  case object G11L extends ThingA[TypeA]

  case object G11 extends G("11") {
    override def linkedG: ThingA[TypeA] = G11L
  }

  sealed abstract class CC(val id: String) extends ThingA[TypeA] {
    def c1: ThingA[TypeA]

    def c2: ThingA[TypeA]

    def c3: ThingA[TypeA]

    def c4: ThingD[TypeA]

    def c5: ThingD[TypeA]
  }

  case object C1 extends CC("1") {
    override def c1: ThingA[TypeA] = C11

    override def c2: ThingA[TypeA] = C12

    override def c3: ThingA[TypeA] = C13

    override def c4: ThingD[TypeA] = C14

    override def c5: ThingD[TypeA] = C15
  }

  case object C11 extends ThingA[TypeA]

  case object C12 extends ThingA[TypeA]

  case object C13 extends ThingA[TypeA]

  case object C2 extends CC("2") {
    override def c1: ThingA[TypeA] = C21

    override def c2: ThingA[TypeA] = C22

    override def c3: ThingA[TypeA] = C23

    override def c4: ThingD[TypeA] = C24

    override def c5: ThingD[TypeA] = C25
  }

  case object C21 extends ThingA[TypeA]

  case object C22 extends ThingA[TypeA]

  case object C23 extends ThingA[TypeA]

  case object SN extends ThingC[TypeA]

  case object CLC extends ThingE[TypeA]

  case object SW extends ThingE[TypeA]

  case object A4L extends ThingD[TypeA]

  case object A5L extends ThingD[TypeA]

  case object A6L extends ThingD[TypeA]

  case object A7L extends ThingD[TypeA]

  case object A8L extends ThingD[TypeA]

  case object A9L extends ThingD[TypeA]

  case object A10L extends ThingD[TypeA]

  case object A11L extends ThingD[TypeA]

  case object A12L extends ThingD[TypeA]

  case object A13L extends ThingD[TypeA]

  case object A14L extends ThingD[TypeA]

  case object A15L extends ThingD[TypeA]

  case object ABC1 extends ThingD[TypeA]

  case object ABC2 extends ThingD[TypeA]

  case object ABC3 extends ThingD[TypeA]

  case object ABC4 extends ThingD[TypeA]

  case object ABC5 extends ThingD[TypeA]

  case object ABC6 extends ThingD[TypeA]

  case object ABC7 extends ThingD[TypeA]

  case object ABC8 extends ThingD[TypeA]

  case object ABC9 extends ThingD[TypeA]

  case object ABC10 extends ThingD[TypeA]

  case object C14 extends ThingD[TypeA]

  case object C15 extends ThingD[TypeA]

  case object C24 extends ThingD[TypeA]

  case object C25 extends ThingD[TypeA]

  case object ASD1 extends ThingD[TypeA]

  case object ASD2 extends ThingD[TypeA]

  case object ASD3 extends ThingD[TypeA]

  case object ASD4 extends ThingD[TypeA]

  case object ASD5 extends ThingE[TypeA]

  case object ASD6 extends ThingE[TypeA]

  sealed trait IR extends ThingE[TypeA] {
    def linkedIR1: ThingD[TypeA]
    def linkedIR2: ThingD[TypeA]
  }

  case object IR11 extends ThingD[TypeA]

  case object IR12 extends ThingD[TypeA]

  case object IR1 extends IR {
    override def linkedIR1: ThingD[TypeA] = IR11

    override def linkedIR2: ThingD[TypeA] = IR12
  }

  case object IR21 extends ThingD[TypeA]

  case object IR22 extends ThingD[TypeA]

  case object IR2 extends IR {
    override def linkedIR1: ThingD[TypeA] = IR21
    override def linkedIR2: ThingD[TypeA] = IR22
  }

  case object QW1 extends ThingE[TypeA]

  case object QW2 extends ThingE[TypeA]

  case object QW3 extends ThingE[TypeA]

  case object QW4 extends ThingE[TypeA]

  case object QW5 extends ThingE[TypeA]

  case object QW6 extends ThingE[TypeA]

  sealed abstract class IE(val id: String) extends ThingA[TypeA] {
    def linkedIE1: ThingE[TypeA]
    def linkedIE2: ThingE[TypeA]
    def linkedIE3: Thing[TypeA]
    def linkedIE4: Thing[TypeA]
  }

  case object IE1 extends IE("1") {
    override val linkedIE1: ThingE[TypeA] = IE11
    override val linkedIE2: ThingE[TypeA] = IE12
    override val linkedIE3: ThingD[TypeA] = ABC3
    override val linkedIE4: ThingD[TypeA] = ABC4
  }

  case object IE11 extends ThingE[TypeA]

  case object IE12 extends ThingE[TypeA]

  case object IE2 extends IE("2") {
    override val linkedIE1: ThingE[TypeA] = IE21
    override val linkedIE2: ThingE[TypeA] = IE22
    override val linkedIE3: ThingE[TypeA] = IE23
    override val linkedIE4: ThingE[TypeA] = IE24
  }

  case object IE21 extends ThingE[TypeA]

  case object IE22 extends ThingE[TypeA]

  case object IE23 extends ThingE[TypeA]

  case object IE24 extends ThingE[TypeA]

  sealed abstract class LA extends ThingC[TypeA]

  case object LA1 extends LA

  case object LA2 extends LA

  case object LA3 extends LA

  case object LA4 extends LA

  case object LA5 extends ThingC[TypeA]

  sealed abstract class MAD(val id: String) extends ThingC[TypeA] {
    def otherId: String
    def linkedMAD1: ThingC[TypeA]
    def linkedMAD2: ThingC[TypeA]
    def linkedMAD3: ThingD[TypeA]
    def linkedMAD4: ThingC[TypeA]
    def linkedMAD5: ThingC[TypeA]
    def linkedMAD6: ThingC[TypeA]
  }

  case object MAD11 extends ThingC[TypeA]

  case object MAD12 extends ThingC[TypeA]

  case object MAD13 extends ThingD[TypeA]

  case object MAD14 extends ThingC[TypeA]

  case object MAD15 extends ThingC[TypeA]

  case object MAD16 extends ThingC[TypeA]

  case object MAD1 extends MAD("1") {
    override def otherId: String = "c1"
    override def linkedMAD1: ThingC[TypeA] = MAD11
    override def linkedMAD2: ThingC[TypeA] = MAD12
    override def linkedMAD3: ThingD[TypeA] = MAD13
    override def linkedMAD4: ThingC[TypeA] = MAD14
    override def linkedMAD5: ThingC[TypeA] = MAD15
    override def linkedMAD6: ThingC[TypeA] = MAD16
  }

  case object MAD21 extends ThingC[TypeA]
  case object MAD22 extends ThingC[TypeA]
  case object MAD23 extends ThingD[TypeA]
  case object MAD24 extends ThingC[TypeA]
  case object MAD25 extends ThingC[TypeA]
  case object MAD26 extends ThingC[TypeA]
  case object MAD2 extends MAD("2") {
    override def otherId: String = "c2"
    override def linkedMAD1: ThingC[TypeA] = MAD21
    override def linkedMAD2: ThingC[TypeA] = MAD22
    override def linkedMAD3: ThingD[TypeA] = MAD23
    override def linkedMAD4: ThingC[TypeA] = MAD24
    override def linkedMAD5: ThingC[TypeA] = MAD25
    override def linkedMAD6: ThingC[TypeA] = MAD26
  }
}

object Matcher {
  implicit val converter: Thing[Phantom.TypeA] => String = {
    case Stuff.OM1 => "OM1"
    case Stuff.OM1L => "OM1L"
    case Stuff.OM2 => "OM2"
    case Stuff.OM2L => "OM2L"
    case Stuff.OM3 => "OM3"
    case Stuff.OM3L => "OM3L"
    case Stuff.OM4 => "OM4"
    case Stuff.OM4L => "OM4L"
    case Stuff.OM5 => "OM5"
    case Stuff.OM5L => "OM5L"
    case Stuff.OM6 => "OM6"
    case Stuff.OM6L => "OM6L"
    case Stuff.OM7 => "OM7"
    case Stuff.OM7L => "OM7L"
    case Stuff.A4 => "A4"
    case Stuff.A5 => "A5"
    case Stuff.A6 => "A6"
    case Stuff.A7 => "A7"
    case Stuff.A8 => "A8"
    case Stuff.A9 => "A9"
    case Stuff.A10 => "A10"
    case Stuff.A11 => "A11"
    case Stuff.A12 => "A12"
    case Stuff.A13 => "A13"
    case Stuff.A14 => "A14"
    case Stuff.A15 => "A15"
    case Stuff.A4L => "A4L"
    case Stuff.A5L => "A5L"
    case Stuff.A6L => "A6L"
    case Stuff.A7L => "A7L"
    case Stuff.A8L => "A8L"
    case Stuff.A9L => "A9L"
    case Stuff.A10L => "A10L"
    case Stuff.A11L => "A11L"
    case Stuff.A12L => "A12L"
    case Stuff.A13L => "A13L"
    case Stuff.A14L => "A14L"
    case Stuff.A15L => "A15L"
    case Stuff.ABC1 => "ABC1"
    case Stuff.ABC2 => "ABC2"
    case Stuff.ABC3 => "ABC3"
    case Stuff.ABC4 => "ABC4"
    case Stuff.QW1 => "QW1"
    case Stuff.QW2 => "QW2"
    case Stuff.IR1 => "IR1"
    case Stuff.QW3 => "QW3"
    case Stuff.QW4 => "QW4"
    case Stuff.QW5 => "QW5"
    case Stuff.QW6 => "QW6"
    case Stuff.IE1 => "IE1"
    case Stuff.IE11 => "IE11"
    case Stuff.IE12 => "IE12"
    case Stuff.IE2 => "IE2"
    case Stuff.IE21 => "IE21"
    case Stuff.IE22 => "IE22"
    case Stuff.IE23 => "IE23"
    case Stuff.IE24 => "IE24"
    case Stuff.LA1 => "LA1"
    case Stuff.LA2 => "LA2"
    case Stuff.LA3 => "LA3"
    case Stuff.LA5 => "LA5"
    case Stuff.A3 => "A3"
    case Stuff.ASD1 => "ASD1"
    case Stuff.ASD5 => "ASD5"
    case Stuff.ASD6 => "ASD6"
    case Stuff.IR11 => "IR11"
    case Stuff.IR12 => "IR12"
    case Stuff.ASD2 => "ASD2"
    case Stuff.ASD3 => "ASD3"
    case Stuff.A1 => "A1"
    case Stuff.A2 => "A2"
    case Stuff.G1 => "G1"
    case Stuff.G2 => "G2"
    case Stuff.G3 => "G3"
    case Stuff.G4 => "G4"
    case Stuff.G5 => "G5"
    case Stuff.G6 => "G6"
    case Stuff.G1L => "G1L"
    case Stuff.G2L => "G2L"
    case Stuff.G3L => "G3L"
    case Stuff.G4L => "G4L"
    case Stuff.G5L => "G5L"
    case Stuff.G6L => "G6L"
    case Stuff.ABC5 => "ABC5"
    case Stuff.ABC6 => "ABC6"
    case Stuff.ABC7 => "ABC7"
    case Stuff.ABC8 => "ABC8"
    case Stuff.ABC9 => "ABC9"
    case Stuff.ABC10 => "ABC10"
    case Stuff.ASD4 => "ASD4"
    case Stuff.SW => "SW"
    case Stuff.C1 => "C1"
    case Stuff.C11 => "C11"
    case Stuff.IR2 => "IR2"
    case Stuff.IR21 => "IR21"
    case Stuff.IR22 => "IR22"
    case Stuff.MAD14 => "MAD14"
    case Stuff.MAD15 => "MAD15"
    case Stuff.MAD11 => "MAD11"
    case Stuff.MAD1 => "MAD1"
    case Stuff.SN => "SN"
    case Stuff.C12 => "C12"
    case Stuff.C13 => "C13"
    case Stuff.MAD12 => "MAD12"
    case Stuff.C14 => "C14"
    case Stuff.C15 => "C15"
    case Stuff.G7 => "G7"
    case Stuff.G7L => "G7L"
    case Stuff.G8 => "G8"
    case Stuff.G8L => "G8L"
    case Stuff.C2 => "C2"
    case Stuff.C21 => "C21"
    case Stuff.C22 => "C22"
    case Stuff.C23 => "C23"
    case Stuff.C24 => "C24"
    case Stuff.C25 => "C25"
    case Stuff.MAD21 => "MAD21"
    case Stuff.MAD22 => "MAD22"
    case Stuff.MAD24 => "MAD24"
    case Stuff.MAD25 => "MAD25"
    case Stuff.MAD2 => "MAD2"
    case Stuff.CLC => "CLC"
    case Stuff.MAD13 => "MAD13"
    case Stuff.MAD16 => "MAD16"
    case Stuff.MAD23 => "MAD23"
    case Stuff.MAD26 => "MAD26"
    case Stuff.G9 => "G9"
    case Stuff.G9L => "G9L"
    case Stuff.LA4 => "LA4"
    case Stuff.G10 => "G10"
    case Stuff.G10L => "G10L"
    case Stuff.G11 => "G11"
    case Stuff.G11L => "G11L"
    case _ => "unknown"
  }
}
