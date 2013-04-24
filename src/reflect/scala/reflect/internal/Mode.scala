/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.language.implicitConversions

object Mode {
  private implicit def liftIntBitsToMode(bits: Int): Mode = apply(bits)
  def apply(bits: Int): Mode = new Mode(bits)

  /** NOmode, EXPRmode and PATTERNmode are mutually exclusive.
   */
  final val NOmode: Mode        = 0x000
  final val EXPRmode: Mode      = 0x001
  final val PATTERNmode: Mode   = 0x002

  /** TYPEmode needs a comment. <-- XXX.
   */
  final val TYPEmode: Mode      = 0x004

  /** SCCmode is orthogonal to above. When set we are
   *  in the this or super constructor call of a constructor.
   */
  final val SCCmode: Mode       = 0x008

  /** FUNmode is orthogonal to above.
   *  When set we are looking for a method or constructor.
   */
  final val FUNmode: Mode       = 0x010

  /** POLYmode is orthogonal to above.
   *  When set expression types can be polymorphic.
   */
  final val POLYmode: Mode      = 0x020

  /** QUALmode is orthogonal to above. When set
   *  expressions may be packages and Java statics modules.
   */
  final val QUALmode: Mode      = 0x040

  /** TAPPmode is set for the function/type constructor
   *  part of a type application. When set we do not decompose PolyTypes.
   */
  final val TAPPmode: Mode      = 0x080

  /** SUPERCONSTRmode is set for the super
   *  in a superclass constructor call super.<init>.
   */
  final val SUPERCONSTRmode: Mode = 0x100

  /** SNDTRYmode indicates that an application is typed for the 2nd time.
   *  In that case functions may no longer be coerced with implicit views.
   */
  final val SNDTRYmode: Mode    = 0x200

  /** LHSmode is set for the left-hand side of an assignment.
   */
  final val LHSmode: Mode       = 0x400

  /** STARmode is set when star patterns are allowed.
   *  (This was formerly called REGPATmode.)
   */
  final val STARmode: Mode      = 0x1000

  /** ALTmode is set when we are under a pattern alternative.
   */
  final val ALTmode: Mode       = 0x2000

  /** HKmode is set when we are typing a higher-kinded type.
   *  adapt should then check kind-arity based on the prototypical type's
   *  kind arity.  Type arguments should not be inferred.
   */
  final val HKmode: Mode        = 0x4000 // @M: could also use POLYmode | TAPPmode

  /** BYVALmode is set when we are typing an expression
   *  that occurs in a by-value position. An expression e1 is in by-value
   *  position within expression e2 iff it will be reduced to a value at that
   *  position during the evaluation of e2.  Examples are by-value function
   *  arguments or the conditional of an if-then-else clause.
   *  This mode has been added to support continuations.
   */
  final val BYVALmode: Mode     = 0x8000

  /** TYPEPATmode is set when we are typing a type in a pattern.
   */
  final val TYPEPATmode: Mode   = 0x10000

  /** RETmode is set when we are typing a return expression.
   */
  final val RETmode: Mode       = 0x20000

  final private val StickyModes: Mode = EXPRmode | PATTERNmode | TYPEmode | ALTmode

  /** Translates a mask of mode flags into something readable.
   */
  private val modeNameMap = Map[Int, String]( // TODO why duplicate the bitmasks here, rather than just referring to this.EXPRmode etc?
    (1 << 0)  -> "EXPRmode",
    (1 << 1)  -> "PATTERNmode",
    (1 << 2)  -> "TYPEmode",
    (1 << 3)  -> "SCCmode",
    (1 << 4)  -> "FUNmode",
    (1 << 5)  -> "POLYmode",
    (1 << 6)  -> "QUALmode",
    (1 << 7)  -> "TAPPmode",
    (1 << 8)  -> "SUPERCONSTRmode",
    (1 << 9)  -> "SNDTRYmode",
    (1 << 10) -> "LHSmode",
    (1 << 11) -> "<DOES NOT EXIST mode>",
    (1 << 12) -> "STARmode",
    (1 << 13) -> "ALTmode",
    (1 << 14) -> "HKmode",
    (1 << 15) -> "BYVALmode",
    (1 << 16) -> "TYPEPATmode"
  ).map({ case (k, v) => Mode(k) -> v })
}
import Mode._

final class Mode private (val bits: Int) extends AnyVal {
  def &(other: Mode): Mode = new Mode(bits & other.bits)
  def |(other: Mode): Mode = new Mode(bits | other.bits)
  def &~(other: Mode): Mode = new Mode(bits & ~(other.bits))

  def onlySticky = this & Mode.StickyModes
  def forFunMode = this & (Mode.StickyModes | SCCmode) | FUNmode | POLYmode | BYVALmode
  def forTypeMode =
    if (inAny(PATTERNmode | TYPEPATmode)) TYPEmode | TYPEPATmode
    else TYPEmode

  def inAll(required: Mode)              = (this & required) == required
  def inAny(required: Mode)              = (this & required) !=NOmode
  def inNone(prohibited: Mode)           = (this & prohibited) == NOmode
  def inHKMode                           = inAll(HKmode)
  def inFunMode                          = inAll(FUNmode)
  def inPolyMode                         = inAll(POLYmode)
  def inPatternMode                      = inAll(PATTERNmode)
  def inExprMode                         = inAll(EXPRmode)
  def inByValMode                        = inAll(BYVALmode)
  def inRetMode                          = inAll(RETmode)

  def inPatternNotFunMode                = inPatternMode && !inFunMode
  def inExprModeOr(others: Mode)         = inAny(EXPRmode | others)
  def inExprModeButNot(prohibited: Mode) = inAll(EXPRmode) && inNone(prohibited)

  override def toString =
    if (bits == 0) "NOmode"
    else (modeNameMap filterKeys inAll).values.toList.sorted mkString " "
}
