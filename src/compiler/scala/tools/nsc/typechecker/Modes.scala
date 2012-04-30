/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/** Mode constants.
 */
trait Modes {
  /** NOmode, EXPRmode and PATTERNmode are mutually exclusive.
   */
  final val NOmode        = 0x000
  final val EXPRmode      = 0x001
  final val PATTERNmode   = 0x002

  /** TYPEmode needs a comment. <-- XXX.
   */
  final val TYPEmode      = 0x004

  /** SCCmode is orthogonal to above. When set we are
   *  in the this or super constructor call of a constructor.
   */
  final val SCCmode       = 0x008

  /** FUNmode is orthogonal to above.
   *  When set we are looking for a method or constructor.
   */
  final val FUNmode       = 0x010

  /** POLYmode is orthogonal to above.
   *  When set expression types can be polymorphic.
   */
  final val POLYmode      = 0x020

  /** QUALmode is orthogonal to above. When set
   *  expressions may be packages and Java statics modules.
   */
  final val QUALmode      = 0x040

  /** TAPPmode is set for the function/type constructor
   *  part of a type application. When set we do not decompose PolyTypes.
   */
  final val TAPPmode      = 0x080

  /** SUPERCONSTRmode is set for the super
   *  in a superclass constructor call super.<init>.
   */
  final val SUPERCONSTRmode = 0x100

  /** SNDTRYmode indicates that an application is typed for the 2nd time.
   *  In that case functions may no longer be coerced with implicit views.
   */
  final val SNDTRYmode    = 0x200

  /** LHSmode is set for the left-hand side of an assignment.
   */
  final val LHSmode       = 0x400

  /** STARmode is set when star patterns are allowed.
   *  (This was formerly called REGPATmode.)
   */
  final val STARmode      = 0x1000

  /** ALTmode is set when we are under a pattern alternative.
   */
  final val ALTmode       = 0x2000

  /** HKmode is set when we are typing a higher-kinded type.
   *  adapt should then check kind-arity based on the prototypical type's
   *  kind arity.  Type arguments should not be inferred.
   */
  final val HKmode        = 0x4000 // @M: could also use POLYmode | TAPPmode

  /** BYVALmode is set when we are typing an expression
   *  that occurs in a by-value position. An expression e1 is in by-value
   *  position within expression e2 iff it will be reduced to a value at that
   *  position during the evaluation of e2.  Examples are by-value function
   *  arguments or the conditional of an if-then-else clause.
   *  This mode has been added to support continuations.
   */
  final val BYVALmode     = 0x8000

  /** TYPEPATmode is set when we are typing a type in a pattern.
   */
  final val TYPEPATmode   = 0x10000

  final private val StickyModes   = EXPRmode | PATTERNmode | TYPEmode | ALTmode

  final def onlyStickyModes(mode: Int) =
    mode & StickyModes

  final def forFunMode(mode: Int) =
    mode & (StickyModes | SCCmode) | FUNmode | POLYmode | BYVALmode

  final def forTypeMode(mode: Int) =
    if (inAnyMode(mode, PATTERNmode | TYPEPATmode)) TYPEmode | TYPEPATmode
    else TYPEmode

  final def inAllModes(mode: Int, required: Int)  = (mode & required) == required
  final def inAnyMode(mode: Int, required: Int)   = (mode & required) != 0
  final def inNoModes(mode: Int, prohibited: Int) = (mode & prohibited) == 0
  final def inHKMode(mode: Int)                   = (mode & HKmode) != 0
  final def inFunMode(mode: Int)                  = (mode & FUNmode) != 0
  final def inPolyMode(mode: Int)                 = (mode & POLYmode) != 0
  final def inPatternMode(mode: Int)              = (mode & PATTERNmode) != 0
  final def inExprModeOr(mode: Int, others: Int)  = (mode & (EXPRmode | others)) != 0
  final def inExprModeButNot(mode: Int, prohibited: Int) =
    (mode & (EXPRmode | prohibited)) == EXPRmode

  /** Translates a mask of mode flags into something readable.
   */
  private val modeNameMap = Map[Int, String](
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
  )
  def modeString(mode: Int): String =
    if (mode == 0) "NOmode"
    else (modeNameMap filterKeys (bit => inAllModes(mode, bit))).values mkString " "
}