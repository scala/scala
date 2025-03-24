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

package scala
package reflect
package internal

import scala.language.implicitConversions

trait FlagSets extends api.FlagSets { self: SymbolTable =>

  type FlagSet = Long
  implicit val FlagSetTag: ClassTag[FlagSet] = ClassTag[FlagSet](classOf[FlagSet])

  implicit def addFlagOps(left: FlagSet): FlagOps =
    new FlagOpsImpl(left)

  private class FlagOpsImpl(left: Long) extends FlagOps {
    def | (right: Long): Long = left | right
  }

  val NoFlags: FlagSet = 0L

  object Flag extends FlagValues {
    val TRAIT         : FlagSet = Flags.TRAIT
    val INTERFACE     : FlagSet = Flags.INTERFACE
    val MUTABLE       : FlagSet = Flags.MUTABLE
    val MACRO         : FlagSet = Flags.MACRO
    val DEFERRED      : FlagSet = Flags.DEFERRED
    val ABSTRACT      : FlagSet = Flags.ABSTRACT
    val FINAL         : FlagSet = Flags.FINAL
    val SEALED        : FlagSet = Flags.SEALED
    val IMPLICIT      : FlagSet = Flags.IMPLICIT
    val LAZY          : FlagSet = Flags.LAZY
    val OVERRIDE      : FlagSet = Flags.OVERRIDE
    val PRIVATE       : FlagSet = Flags.PRIVATE
    val PROTECTED     : FlagSet = Flags.PROTECTED
    val LOCAL         : FlagSet = Flags.LOCAL
    val CASE          : FlagSet = Flags.CASE
    val ABSOVERRIDE   : FlagSet = Flags.ABSOVERRIDE
    val BYNAMEPARAM   : FlagSet = Flags.BYNAMEPARAM
    val PARAM         : FlagSet = Flags.PARAM
    val COVARIANT     : FlagSet = Flags.COVARIANT
    val CONTRAVARIANT : FlagSet = Flags.CONTRAVARIANT
    val DEFAULTPARAM  : FlagSet = Flags.DEFAULTPARAM
    val PRESUPER      : FlagSet = Flags.PRESUPER
    val DEFAULTINIT   : FlagSet = Flags.DEFAULTINIT
    val ENUM          : FlagSet = Flags.JAVA_ENUM
    val PARAMACCESSOR : FlagSet = Flags.PARAMACCESSOR
    val CASEACCESSOR  : FlagSet = Flags.CASEACCESSOR
    val SYNTHETIC     : FlagSet = Flags.SYNTHETIC
    val ARTIFACT      : FlagSet = Flags.ARTIFACT
    val STABLE        : FlagSet = Flags.STABLE
  }
}
