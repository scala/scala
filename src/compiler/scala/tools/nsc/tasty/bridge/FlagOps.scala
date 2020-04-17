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

import scala.tools.nsc.tasty.{TastyFlags, TastyUniverse}, TastyFlags._
import scala.reflect.internal.{Flags, ModifierFlags}

trait FlagOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  object FlagSets {
    val TastyOnlyFlags: TastyFlagSet = (
      Erased | Internal | Inline | InlineProxy | Opaque | Scala2x | Extension | Given | Exported | Macro | Enum
      | Open | ParamAlias
    )
    val TermParamOrAccessor: TastyFlagSet = Param | ParamSetter
    val ModuleCreationFlags: TastyFlagSet = Object | Lazy | Final | Stable
    val ModuleClassCreationFlags: TastyFlagSet = Object | Final
  }

  private[bridge] def encodeFlagSet(tflags: TastyFlagSet): u.FlagSet = {
    import u.Flag
    var flags = u.NoFlags
    if (tflags.is(Private)) flags |= Flag.PRIVATE
    if (tflags.is(Protected)) flags |= Flag.PROTECTED
    if (tflags.is(AbsOverride)) flags |= Flag.ABSOVERRIDE
    if (tflags.is(Abstract)) flags |= Flag.ABSTRACT
    if (tflags.is(Final)) flags |= Flag.FINAL
    if (tflags.is(Interface)) flags |= Flag.INTERFACE
    if (tflags.is(Sealed)) flags |= Flag.SEALED
    if (tflags.is(Case)) flags |= Flag.CASE
    if (tflags.is(Implicit)) flags |= ModifierFlags.IMPLICIT
    if (tflags.is(Lazy)) flags |= Flag.LAZY
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
    if (tflags.is(DefaultParameterized)) flags |= Flag.DEFAULTPARAM
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
        case Erased          => "erased"
        case Internal        => "<internal>"
        case Inline          => "inline"
        case InlineProxy     => "<inlineproxy>"
        case Opaque          => "opaque"
        case Scala2x         => "<scala2x>"
        case Extension       => "<extension>"
        case Given           => "given"
        case Exported        => "<exported>"
        case Macro           => "<tastymacro>"
        case Enum            => "enum"
        case Open            => "open"
        case ParamAlias      => "<paramalias>"
      }
    } mkString(" | ")
  }
}
