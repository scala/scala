/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

object Mode {
  def apply(bits: Int): Mode = new Mode(bits)

  /** NOmode, EXPRmode and PATTERNmode are mutually exclusive.
   */
  final val NOmode: Mode        = Mode(0x000)
  final val EXPRmode: Mode      = Mode(0x001)
  final val PATTERNmode: Mode   = Mode(0x002)

  final val TYPEmode: Mode      = Mode(0x004)

  /** SCCmode is orthogonal to above. When set we are
   *  in the this or super constructor call of a constructor.
   */
  final val SCCmode: Mode       = Mode(0x008)

  /** FUNmode is orthogonal to above.
   *  When set we are looking for a method or constructor.
   */
  final val FUNmode: Mode       = Mode(0x010)

  /** POLYmode is orthogonal to above.
   *  When set expression types can be polymorphic.
   */
  final val POLYmode: Mode      = Mode(0x020)

  /** QUALmode is orthogonal to above. When set
   *  expressions may be packages and Java statics modules.
   */
  final val QUALmode: Mode      = Mode(0x040)

  /** TAPPmode is set for the function/type constructor
   *  part of a type application. When set we do not decompose PolyTypes.
   */
  final val TAPPmode: Mode      = Mode(0x080)

  /** LHSmode is set for the left-hand side of an assignment.
   */
  final val LHSmode: Mode       = Mode(0x400)

  /** BYVALmode is set when we are typing an expression
   *  that occurs in a by-value position. An expression e1 is in by-value
   *  position within expression e2 iff it will be reduced to a value at that
   *  position during the evaluation of e2.  Examples are by-value function
   *  arguments or the conditional of an if-then-else clause.
   *  This mode has been added to support continuations.
   */
  final val BYVALmode: Mode     = Mode(0x8000)

  /** TYPEPATmode is set when we are typing a type in a pattern.
   */
  final val TYPEPATmode: Mode   = Mode(0x10000)

  /** This mode is set when starting to type check a `Select`, `Apply` or `TypeApply`, e.g., `x.y`
    * or `a.b.foo[T](x, y).bar(z)`. Stabilizers (a feature added in PR scala/scala#5999) created
    * when typing the expression are emitted in a new enclosing block, e.g.
    *   {
    *     val $stabilizer$1 = a.b
    *     val $stabilizer$2 = $stabilizer1.foo[T](x, y)
    *     $stabilizer$2.bar(z)
    *   }
    *
    * The flag is sticky for typing the function of an Apply (`forFunMode`) and qualifiers of
    * nested selections (`MonoQualifierModes`), but cleared for argument expressions
    * (`onlySticky`). So `a.b.foo(a.b.bar)` becomes
    *   {
    *     val $stabilizer$1 = a.b
    *     $stabilizer$1.foo({
    *       val $stabilizer$2 = a.b
    *       $stabilizer$2.bar
    *     })
    *   }
    */
  final val APPSELmode: Mode    = Mode(0x20000)

  private val StickyModes: Mode       = EXPRmode | PATTERNmode | TYPEmode
  private val StickyModesForFun: Mode = StickyModes | SCCmode
  final val MonoQualifierModes: Mode  = EXPRmode | QUALmode | APPSELmode
  final val PolyQualifierModes: Mode  = MonoQualifierModes | POLYmode
  final val OperatorModes: Mode       = EXPRmode | POLYmode | TAPPmode | FUNmode

  /** Translates a mask of mode flags into something readable. */
  private val modeNameMap = Map[Mode, String](
    EXPRmode     -> "EXPRmode",
    PATTERNmode  -> "PATTERNmode",
    TYPEmode     -> "TYPEmode",
    SCCmode      -> "SCCmode",
    FUNmode      -> "FUNmode",
    POLYmode     -> "POLYmode",
    QUALmode     -> "QUALmode",
    TAPPmode     -> "TAPPmode",
    LHSmode      -> "LHSmode",
    BYVALmode    -> "BYVALmode",
    TYPEPATmode  -> "TYPEPATmode",
    APPSELmode   -> "APPSELmode"
  )

  // Former modes and their values:
  // SUPERCONSTRmode (0x100), SNDTRYmode (0x200), CONSTmode (0x800)
  // STARmode (0x1000), ALTmode (0x2000), HKmode (0x4000)
  // RETmode (0x20000) - now APPSELmode
}
import Mode._

final class Mode private (val bits: Int) extends AnyVal {
  def &(other: Mode): Mode  = new Mode(bits & other.bits)
  def |(other: Mode): Mode  = new Mode(bits | other.bits)
  def &~(other: Mode): Mode = new Mode(bits & ~(other.bits))

  def onlyTypePat = this & TYPEPATmode
  def onlySticky  = this & Mode.StickyModes
  def forFunMode  = this & Mode.StickyModesForFun | FUNmode | POLYmode | BYVALmode | APPSELmode
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
    else modeNameMap.view.filterKeys(inAll).values.toList.sorted.mkString("-")
}
