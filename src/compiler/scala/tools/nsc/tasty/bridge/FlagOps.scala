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

package scala.tools.nsc.tasty.bridge

import scala.tools.tasty.TastyFlags._
import scala.tools.nsc.tasty.TastyUniverse
import scala.reflect.internal.{Flags, ModifierFlags}

/**Handles encoding of [[TastyFlagSet]] to [[scala.reflect]] flags and witnessing which flags do not map directly
 * from TASTy.
 */
trait FlagOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  object FlagSets {
    val TastyOnlyFlags: TastyFlagSet = (
      Erased | Internal | Inline | InlineProxy | Opaque | Extension | Given | Exported | SuperTrait | Enum
      | Open | ParamAlias
    )
    val TermParamOrAccessor: TastyFlagSet = Param | ParamSetter
    val ObjectCreationFlags: TastyFlagSet = Object | Lazy | Final | Stable
    val ObjectClassCreationFlags: TastyFlagSet = Object | Final
    val SingletonEnumFlags: TastyFlagSet = Case | Static | Enum | Stable
    val FieldAccessorFlags: TastyFlagSet = FieldAccessor | Stable
    val LocalFieldFlags: TastyFlagSet = Private | Local
  }

  /**encodes a [[TastyFlagSet]] as [[scala.reflect]] flags and will ignore flags that can't be converted, such as
   * members of [[FlagSets.TastyOnlyFlags]]
   */
  private[bridge] def encodeFlagSet(tflags: TastyFlagSet): u.FlagSet = {
    import u.Flag
    var flags = u.NoFlags
    if (tflags.is(Private)) flags |= Flag.PRIVATE
    if (tflags.is(Protected)) flags |= Flag.PROTECTED
    if (tflags.is(AbsOverride)) flags |= Flag.ABSOVERRIDE
    if (tflags.is(Abstract)) flags |= Flag.ABSTRACT
    if (tflags.is(Final)) flags |= Flag.FINAL
    if (tflags.is(Sealed)) flags |= Flag.SEALED
    if (tflags.is(Case)) flags |= Flag.CASE
    if (tflags.is(Implicit)) flags |= ModifierFlags.IMPLICIT
    if (tflags.is(Lazy)) flags |= Flag.LAZY
    if (tflags.is(Macro)) flags |= Flag.MACRO
    if (tflags.is(Override)) flags |= Flag.OVERRIDE
    if (tflags.is(Static)) flags |= ModifierFlags.STATIC
    if (tflags.is(Object)) flags |= Flags.MODULE
    if (tflags.is(Trait)) flags |= Flag.TRAIT
    if (tflags.is(Local)) flags |= Flag.LOCAL
    if (tflags.is(Synthetic)) flags |= Flag.SYNTHETIC
    if (tflags.is(Artifact)) flags |= Flag.ARTIFACT
    if (tflags.is(Mutable)) flags |= Flag.MUTABLE
    if (tflags.is(FieldAccessor)) flags |= Flags.ACCESSOR
    if (tflags.is(CaseAccessor)) flags |= Flag.CASEACCESSOR
    if (tflags.is(Covariant)) flags |= Flag.COVARIANT
    if (tflags.is(Contravariant)) flags |= Flag.CONTRAVARIANT
    if (tflags.is(HasDefault)) flags |= Flag.DEFAULTPARAM
    if (tflags.is(Stable)) flags |= Flag.STABLE
    if (tflags.is(ParamSetter)) flags |= Flag.PARAMACCESSOR
    if (tflags.is(Param)) flags |= Flag.PARAM
    if (tflags.is(Deferred)) flags |= Flag.DEFERRED
    if (tflags.is(Method)) flags |= Flags.METHOD
    flags
  }

  def showTasty(flags: TastyFlagSet): String = { // keep up to date with with FlagSets.TastyOnlyFlags
    val tflags = flags & FlagSets.TastyOnlyFlags
    if (!tflags) "EmptyTastyFlags"
    else (tflags).toSingletonSets.map { f =>
      (f: @unchecked) match {
        case Erased      => "erased"
        case Internal    => "<internal>"
        case Inline      => "inline"
        case InlineProxy => "<inlineproxy>"
        case Opaque      => "opaque"
        case Extension   => "<extension>"
        case Given       => "given"
        case Exported    => "<exported>"
        case SuperTrait  => "<supertrait>"
        case Enum        => "enum"
        case Open        => "open"
        case ParamAlias  => "<paramalias>"
      }
    } mkString(" | ")
  }
}
