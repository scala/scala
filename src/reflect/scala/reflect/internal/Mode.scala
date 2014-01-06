/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
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

  /** LHSmode is set for the left-hand side of an assignment.
   */
  final val LHSmode: Mode       = 0x400

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

  private val StickyModes: Mode       = EXPRmode | PATTERNmode | TYPEmode
  private val StickyModesForFun: Mode = StickyModes | SCCmode
  final val MonoQualifierModes: Mode  = EXPRmode | QUALmode
  final val PolyQualifierModes: Mode  = EXPRmode | QUALmode | POLYmode
  final val OperatorModes: Mode       = EXPRmode |            POLYmode | TAPPmode | FUNmode

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
    (1 << 8)  -> "<>",      // formerly SUPERCONSTRmode
    (1 << 9)  -> "<>",      // formerly SNDTRYmode
    (1 << 10) -> "LHSmode",
    (1 << 11) -> "<>",
    (1 << 12) -> "<>",      // formerly STARmode
    (1 << 13) -> "<>",      // formerly ALTmode
    (1 << 14) -> "<>",      // formerly HKmode
    (1 << 15) -> "BYVALmode",
    (1 << 16) -> "TYPEPATmode"
  ).map({ case (k, v) => Mode(k) -> v })
}
import Mode._

final class Mode private (val bits: Int) extends AnyVal {
  def &(other: Mode): Mode  = new Mode(bits & other.bits)
  def |(other: Mode): Mode  = new Mode(bits | other.bits)
  def &~(other: Mode): Mode = new Mode(bits & ~(other.bits))

  def onlyTypePat = this & TYPEPATmode
  def onlySticky  = this & Mode.StickyModes
  def forFunMode  = this & Mode.StickyModesForFun | FUNmode | POLYmode | BYVALmode
  def forTypeMode = if (typingPatternOrTypePat) TYPEmode | TYPEPATmode else TYPEmode

  def inAll(required: Mode)    = (this & required) == required
  def inAny(required: Mode)    = (this & required) != NOmode
  def inNone(prohibited: Mode) = (this & prohibited) == NOmode

  /** True if this mode matches every mode in the 'all' Mode,
   *  and no modes in the 'none' Mode.
   */
  def in(all: Mode = NOmode, none: Mode = NOmode) = inAll(all) && inNone(none)

  def inByValMode   = inAll(BYVALmode)
  def inExprMode    = inAll(EXPRmode)
  def inFunMode     = inAll(FUNmode)
  def inPatternMode = inAll(PATTERNmode)
  def inPolyMode    = inAll(POLYmode)
  def inQualMode    = inAll(QUALmode)
  def inSccMode     = inAll(SCCmode)
  def inTappMode    = inAll(TAPPmode)
  def inTypeMode    = inAll(TYPEmode)

  def typingExprByValue           = inAll(EXPRmode | BYVALmode)
  def typingExprFun               = inAll(EXPRmode | FUNmode)
  def typingExprNotFun            = in(all = EXPRmode, none = FUNmode)
  def typingExprNotFunNotLhs      = in(all = EXPRmode, none = FUNmode | LHSmode)
  def typingExprNotLhs            = in(all = EXPRmode, none = LHSmode)
  def typingExprNotValue          = in(all = EXPRmode, none = BYVALmode)
  def typingMonoExprByValue       = in(all = EXPRmode | BYVALmode, none = POLYmode)
  def typingConstructorPattern    = inAll(PATTERNmode | FUNmode)
  def typingPatternNotConstructor = in(all = PATTERNmode, none = FUNmode)
  def typingPatternOrTypePat      = inAny(PATTERNmode | TYPEPATmode)

  override def toString =
    if (this == NOmode) "NOmode"
    else (modeNameMap filterKeys inAll).values.toList.sorted mkString "-"
}
