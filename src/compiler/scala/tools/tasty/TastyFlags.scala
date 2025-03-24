/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tasty

/**A static type representing a bitset of flags that are encoded in a TASTy file, along with some other flags
 * inferred from context, such as `Method` and `Deferred`.
 */
object TastyFlags {

  final val EmptyTastyFlags = TastyFlagSet(0)

  final val Private               = EmptyTastyFlags.next
  final val Protected             = Private.next
  final val AbsOverride           = Protected.next
  final val Abstract              = AbsOverride.next
  final val Final                 = Abstract.next
  final val Sealed                = Final.next
  final val Case                  = Sealed.next
  final val Implicit              = Case.next
  final val Lazy                  = Implicit.next
  final val Override              = Lazy.next
  final val Static                = Override.next
  final val Object                = Static.next
  final val Trait                 = Object.next
  final val Local                 = Trait.next
  final val Synthetic             = Local.next
  final val Artifact              = Synthetic.next
  final val Mutable               = Artifact.next
  final val FieldAccessor         = Mutable.next
  final val CaseAccessor          = FieldAccessor.next
  final val Covariant             = CaseAccessor.next
  final val Contravariant         = Covariant.next
  final val HasDefault            = Contravariant.next
  final val Stable                = HasDefault.next
  final val ParamSetter           = Stable.next
  final val Param                 = ParamSetter.next
  final val Deferred              = Param.next
  final val Method                = Deferred.next
  final val Erased                = Method.next
  final val Inline                = Erased.next
  final val InlineProxy           = Inline.next
  final val Opaque                = InlineProxy.next
  final val Extension             = Opaque.next
  final val Given                 = Extension.next
  final val Exported              = Given.next
  final val Macro                 = Exported.next
  final val Transparent           = Macro.next
  final val Enum                  = Transparent.next
  final val Open                  = Enum.next
  final val ParamAlias            = Open.next
  final val Infix                 = ParamAlias.next
  final val Invisible             = Infix.next
  final val Tracked               = Invisible.next

  def optFlag(cond: Boolean)(flag: TastyFlagSet): TastyFlagSet = if (cond) flag else EmptyTastyFlags

  case class TastyFlagSet(val toLong: Long) extends AnyVal {

    private[TastyFlags] def next: TastyFlagSet =
      TastyFlagSet(if (toLong == 0) 1 else toLong << 1)

    def |(other: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(toLong | other.toLong)
    def &(mask: TastyFlagSet): TastyFlagSet                   = TastyFlagSet(toLong & mask.toLong)
    def &~(mask: TastyFlagSet): TastyFlagSet                  = TastyFlagSet(toLong & ~mask.toLong)
    def unary_! : Boolean                                     = this.toLong == 0
    def is(mask: TastyFlagSet): Boolean                       = (this & mask) == mask
    def isOneOf(mask: TastyFlagSet): Boolean                  = (this & mask).hasFlags
    def is(mask: TastyFlagSet, butNot: TastyFlagSet): Boolean = if (!butNot) is(mask) else is(mask) && not(butNot)
    def not(mask: TastyFlagSet): Boolean                      = !isOneOf(mask)
    def hasFlags: Boolean                                     = this.toLong != 0

    def debug: String = {
      if (!this) {
        "EmptyTastyFlags"
      }
      else {
        val sb = collection.mutable.ArrayBuffer.empty[String]
        if (is(Private))       sb += "Private"
        if (is(Protected))     sb += "Protected"
        if (is(AbsOverride))   sb += "AbsOverride"
        if (is(Abstract))      sb += "Abstract"
        if (is(Final))         sb += "Final"
        if (is(Sealed))        sb += "Sealed"
        if (is(Case))          sb += "Case"
        if (is(Implicit))      sb += "Implicit"
        if (is(Lazy))          sb += "Lazy"
        if (is(Override))      sb += "Override"
        if (is(Static))        sb += "Static"
        if (is(Object))        sb += "Object"
        if (is(Trait))         sb += "Trait"
        if (is(Local))         sb += "Local"
        if (is(Synthetic))     sb += "Synthetic"
        if (is(Artifact))      sb += "Artifact"
        if (is(Mutable))       sb += "Mutable"
        if (is(FieldAccessor)) sb += "FieldAccessor"
        if (is(CaseAccessor))  sb += "CaseAccessor"
        if (is(Covariant))     sb += "Covariant"
        if (is(Contravariant)) sb += "Contravariant"
        if (is(HasDefault))    sb += "HasDefault"
        if (is(Stable))        sb += "Stable"
        if (is(ParamSetter))   sb += "ParamSetter"
        if (is(Param))         sb += "Param"
        if (is(Deferred))      sb += "Deferred"
        if (is(Method))        sb += "Method"
        if (is(Erased))        sb += "Erased"
        if (is(Inline))        sb += "Inline"
        if (is(InlineProxy))   sb += "InlineProxy"
        if (is(Opaque))        sb += "Opaque"
        if (is(Extension))     sb += "Extension"
        if (is(Given))         sb += "Given"
        if (is(Exported))      sb += "Exported"
        if (is(Macro))         sb += "Macro"
        if (is(Transparent))   sb += "Transparent"
        if (is(Enum))          sb += "Enum"
        if (is(Open))          sb += "Open"
        if (is(ParamAlias))    sb += "ParamAlias"
        if (is(Infix))         sb += "Infix"
        if (is(Invisible))     sb += "Invisible"
        if (is(Tracked))       sb += "Tracked"
        sb.mkString(" | ")
      }
    }
  }

}
