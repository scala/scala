package scala.reflect
package internal

import language.implicitConversions

trait FlagSets extends api.FlagSets { self: SymbolTable =>

  type FlagSet = Long
  implicit val FlagSetTag = ClassTag[FlagSet](classOf[FlagSet])

  implicit def addFlagOps(left: FlagSet): FlagOps =
    new FlagOpsImpl(left)

  private class FlagOpsImpl(left: Long) extends FlagOps {
    def | (right: Long): Long = left | right
    def & (right: Long): Long = left & right
    def containsAll (right: Long): Boolean = (right & ~left) == 0
  }

  val NoFlags: FlagSet = 0L

  trait FlagValues extends FlagValuesApi

  object Flag extends FlagValues {
    val TRAIT         : FlagSet = Flags.TRAIT
    val MODULE        : FlagSet = Flags.MODULE
    val MUTABLE       : FlagSet = Flags.MUTABLE
    val PACKAGE       : FlagSet = Flags.PACKAGE
    val METHOD        : FlagSet = Flags.METHOD
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
    val CASE          : FlagSet = Flags.CASE
    val ABSOVERRIDE   : FlagSet = Flags.ABSOVERRIDE
    val BYNAMEPARAM   : FlagSet = Flags.BYNAMEPARAM
    val PARAM         : FlagSet = Flags.PARAM
    val PARAMACCESSOR : FlagSet = Flags.PARAMACCESSOR
    val CASEACCESSOR  : FlagSet = Flags.CASEACCESSOR
    val COVARIANT     : FlagSet = Flags.COVARIANT
    val CONTRAVARIANT : FlagSet = Flags.CONTRAVARIANT
    val DEFAULTPARAM  : FlagSet = Flags.DEFAULTPARAM
    val INTERFACE     : FlagSet = Flags.INTERFACE

    def union(flags: FlagSet*): FlagSet = {
      var acc = 0L
      for (flag <- flags) acc |= flag
      acc
    }

    def intersection(flags: FlagSet*): FlagSet = {
      var acc = -1L
      for (flag <- flags) acc &= flag
      acc
    }

    def containsAll(superset: FlagSet, subset: FlagSet): Boolean =
      (subset & ~superset) == 0
  }
}
