// No unreachable or exhaustiveness warnings, please.

//
// The reported bug
//

trait AxisCompanion {
   sealed trait Format
   object Format {
      case object Decimal extends Format
      case object Integer extends Format
      // Gives an unrelated warning:  The outer reference in this type test cannot be checked at run time.
      //final case class Time( hours: Boolean = false, millis: Boolean = true ) extends Format
   }
}
object Axis extends AxisCompanion
class Axis {
   import Axis._
   def test( f: Format ) = f match {
      case Format.Integer => "Int"
      // case Format.Time( hours, millis ) => "Time"
      case Format.Decimal => "Dec"
   }
}


//
// Some tricksier variations
//

trait T1[X] {
  trait T2[Y] {
    sealed trait Format
    object Format {
      case object Decimal extends Format
      case object Integer extends Format
    }
  }
}

object O1 extends T1[Any] {
  object O2 extends T2[Any] {

  }
}

case object Shorty extends O1.O2.Format

class Test1 {
   import O1.O2._
   val FI: Format.Integer.type = Format.Integer
   def test( f: Format ) = {
     val ff: f.type = f
     ff match {
        case FI => "Int"
        case Format.Decimal => "Dec"
        case Shorty => "Sho"
     }
   }
}
