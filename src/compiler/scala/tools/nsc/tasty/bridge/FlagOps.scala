package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyFlags
import TastyFlags._

trait FlagOps extends TastyKernel {
  object FlagSets {
    import scala.reflect.internal.{Flags, ModifierFlags}

    val Private: FlagSet = Flag.PRIVATE
    val Protected: FlagSet = Flag.PROTECTED
    val AbsOverride: FlagSet = Flag.ABSOVERRIDE
    val Abstract: FlagSet = Flag.ABSTRACT
    val Final: FlagSet = Flag.FINAL

    val Interface: FlagSet = Flag.INTERFACE
    val Sealed: FlagSet = Flag.SEALED
    val Case: FlagSet = Flag.CASE
    val Implicit: FlagSet = ModifierFlags.IMPLICIT
    val Lazy: FlagSet = Flag.LAZY
    val Override: FlagSet = Flag.OVERRIDE
    val Macro: FlagSet = Flag.MACRO
    val JavaStatic: FlagSet = ModifierFlags.STATIC
    val Module: FlagSet = Flags.MODULE
    val Trait: FlagSet = Flag.TRAIT
    val Enum: FlagSet = Flag.ENUM
    val Local: FlagSet = Flag.LOCAL
    val Synthetic: FlagSet = Flag.SYNTHETIC
    val Artifact: FlagSet = Flag.ARTIFACT
    val Mutable: FlagSet = Flag.MUTABLE
    val Accessor: FlagSet = Flags.ACCESSOR
    val CaseAccessor: FlagSet = Flag.CASEACCESSOR
    val Covariant: FlagSet = Flag.COVARIANT
    val Contravariant: FlagSet = Flag.CONTRAVARIANT
    val DefaultParameterized: FlagSet = Flag.DEFAULTPARAM
    val Stable: FlagSet = Flag.STABLE
    val ParamAccessor: FlagSet = Flag.PARAMACCESSOR
    val Param: FlagSet = Flag.PARAM
    val Deferred: FlagSet = Flag.DEFERRED
    val Method: FlagSet = Flags.METHOD

    val NoInitsInterface: (FlagSet, TastyFlagSet) = (Interface, NoInits)
    val TermParamOrAccessor: FlagSet = Param | ParamAccessor
    val ModuleCreationFlags: FlagSet = Module | Lazy | Final | Stable
    val ModuleClassCreationFlags: FlagSet = Module | Final
    val DeferredOrLazyOrMethod: FlagSet = Deferred | Lazy | Method

    implicit class FlagSetOps(private val flagSet: FlagSet) {
      private def flags: FlagSet = {
        val fs = flagSet & phase.flagMask
        (fs | ((fs & Flags.LateFlags) >>> Flags.LateShift)) & ~((fs & Flags.AntiFlags) >>> Flags.AntiShift)
      }
      private def getFlag(mask: FlagSet): FlagSet = {
        mask & (if ((mask & Flags.PhaseIndependentFlags) == mask) flagSet else flags)
      }
      def not(mask: FlagSet): Boolean = !isOneOf(mask)
      def is(mask: FlagSet): Boolean = getFlag(mask) == mask
      def ensuring(is: FlagSet, when: FlagSet): FlagSet = if (flagSet.is(when)) (flagSet | is) else flagSet
      def is(mask: FlagSet, butNot: FlagSet): Boolean = is(mask) && not(butNot)
      def isOneOf(mask: FlagSet): Boolean = getFlag(mask) != 0
    }
  }

  def show(flags: FlagSet): String = symbolTable.show(flags)

  def show(flags: TastyFlagSet): String =
    if (!flags) "EmptyFlags"
    else flags.toSingletonSets.map { f =>
      (f: @unchecked) match {
        case Erased      => "Erased"
        case Internal    => "Internal"
        case Inline      => "Inline"
        case InlineProxy => "InlineProxy"
        case Opaque      => "Opaque"
        case Scala2x     => "Scala2x"
        case Extension   => "Extension"
        case Given       => "Given"
        case Exported    => "Exported"
        case NoInits     => "NoInits"
        case Open        => "Open"
      }
    } mkString(" | ")
}
