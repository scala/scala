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

package scala.tools.tasty

/**Flags as represented in TASTy
 */
object TastyFlags {

  final val EmptyTastyFlags = TastyFlagSet(0)

  final val Private               = EmptyTastyFlags.next
  final val Protected             = Private.next
  final val AbsOverride           = Protected.next
  final val Abstract              = AbsOverride.next
  final val Final                 = Abstract.next
  final val Interface             = Final.next
  final val Sealed                = Interface.next
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
  final val DefaultParameterized  = Contravariant.next
  final val Stable                = DefaultParameterized.next
  final val ParamSetter           = Stable.next
  final val Param                 = ParamSetter.next
  final val Deferred              = Param.next
  final val Method                = Deferred.next
  final val Erased                = Method.next
  final val Internal              = Erased.next
  final val Inline                = Internal.next
  final val InlineProxy           = Inline.next
  final val Opaque                = InlineProxy.next
  final val Scala2x               = Opaque.next
  final val Extension             = Scala2x.next
  final val Given                 = Extension.next
  final val Exported              = Given.next
  final val Macro                 = Exported.next
  final val Enum                  = Macro.next
  final val Open                  = Enum.next
  final val ParamAlias            = Open.next

  private[TastyFlags] final val maxFlag: Long = ParamAlias.shift

  case class TastyFlagSet(val toLong: Long) extends AnyVal {

    private[TastyFlags] def shift: Long = {
      var acc = 0L
      var curr = toLong
      while (curr != 0L) {
        acc += 1L
        curr = curr >> 1L
      }
      acc
    }

    private[TastyFlags] def next: TastyFlagSet = {
      TastyFlagSet(1L << shift)
    }

    def toSingletonSets: SingletonSets                        = SingletonSets(toLong)
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
        toSingletonSets.map { f =>
          (f: @unchecked) match {
            case Private => "Private"
            case Protected => "Protected"
            case AbsOverride => "AbsOverride"
            case Abstract => "Abstract"
            case Final => "Final"
            case Interface => "Interface"
            case Sealed => "Sealed"
            case Case => "Case"
            case Implicit => "Implicit"
            case Lazy => "Lazy"
            case Override => "Override"
            case Static => "Static"
            case Object => "Object"
            case Trait => "Trait"
            case Local => "Local"
            case Synthetic => "Synthetic"
            case Artifact => "Artifact"
            case Mutable => "Mutable"
            case FieldAccessor => "FieldAccessor"
            case CaseAccessor => "CaseAccessor"
            case Covariant => "Covariant"
            case Contravariant => "Contravariant"
            case DefaultParameterized => "DefaultParameterized"
            case Stable => "Stable"
            case ParamSetter => "ParamSetter"
            case Param => "Param"
            case Deferred => "Deferred"
            case Method => "Method"
            case Erased => "Erased"
            case Internal => "Internal"
            case Inline => "Inline"
            case InlineProxy => "InlineProxy"
            case Opaque => "Opaque"
            case Scala2x => "Scala2x"
            case Extension => "Extension"
            case Given => "Given"
            case Exported => "Exported"
            case Macro => "Macro"
            case Enum => "Enum"
            case Open => "Open"
            case ParamAlias => "ParamAlias"
          }
        } mkString(" | ")
      }
    }
  }

  case class SingletonSets(val toLong: Long) extends AnyVal {
    def map[A](f: TastyFlagSet => A): Iterable[A] = {
      val buf = Iterable.newBuilder[A]
      var i = 0
      while (i <= maxFlag) {
        val flag = 1 << i
        if ((flag & toLong) != 0) {
          buf += f(TastyFlagSet(flag))
        }
        i += 1
      }
      buf.result
    }
  }

}
