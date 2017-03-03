
/* This took me literally all day.
 */
trait RejectedLiterals {

  def missingHex: Int    = { 0x }        // line 4: was: not reported, taken as zero

  def leadingZeros: Int  = { 01 }        // line 6: no leading zero

  def tooManyZeros: Int  = { 00 }        // line 8: no leading zero

  def zeroOfNine: Int    = { 09 }        // line 10: no leading zero

  def orphanDot: Int     = { 9. }        // line 12: ident expected

  def zeroOfNineDot: Int = { 09. }       // line 14: malformed integer, ident expected

  def noHexFloat: Double = { 0x1.2 }     // line 16: ';' expected but double literal found.
}

trait Braceless {

  def missingHex: Int    = 0x            // line 22: was: not reported, taken as zero

  def leadingZeros: Int  = 01            // line 24: no leading zero

  def tooManyZeros: Int  = 00            // line 26: no leading zero

  def zeroOfNine: Int    = 09            // line 28: no leading zero

  def orphanDot: Int     = 9.            // line 30: ident expected

  def zeroOfNineDot: Int = 09.           // line 32: malformed integer, ident expected

  def noHexFloat: Double = 0x1.2         // line 34: ';' expected but double literal found.
}

trait MoreSadness {

  def tooTiny: Float     = { 0.7e-45f }      // floating point number too small

  def twoTiny: Double    = { 2.0e-324 }      // double precision floating point number too small

  def tooHuge: Float     = { 3.4028236E38f } // floating point number too large

  def twoHuge: Double    = { 1.7976931348623159e308 } // double precision floating point number too large
}
