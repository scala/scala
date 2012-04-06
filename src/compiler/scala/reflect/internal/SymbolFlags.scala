 /* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import util.Statistics._
import Flags._
import api.Modifier
import scala.tools.util.StringOps.{ ojoin }

trait SymbolFlags {
  self: SymbolTable =>

  import definitions._

  /** Not mixed in under normal conditions; a powerful debugging aid.
   */
  trait FlagVerifier extends Symbol {
    private def assert0(cond: Boolean, message: => Any) {
      if (!cond) {
        Console.err.println("[flag verification failure]\n%s\n%s\n".format(atPhaseStackMessage, message))
        (new Throwable).getStackTrace.take(13).drop(3).foreach(println)
        println("")
      }
    }

    protected def verifyChange(isAdd: Boolean, mask: Long, before: Long) {
      val after   = if (isAdd) before | mask else before & ~mask
      val added   = after & ~before
      val removed = before & ~after
      val ignored = mask & ~added & ~removed
      val error = (
           (added & OverloadedFlagsMask) != 0 || (removed & OverloadedFlagsMask) != 0
        // || (ignored != 0)
      )
      val addString    = if (added == 0) "" else "+(" + flagsToString(added) + ")"
      val removeString = if (removed == 0) "" else "-(" + flagsToString(removed) + ")"
      val changeString = if (added == 0 && removed == 0) "no change" else addString + " " + removeString

      if (error) {
        val templ = (
          """|  symbol: %s %s in %s
             |    call: %s(%s)
             |   flags: %s
             |  result: %s""".stripMargin
        )

        assert0(false, templ.format(
          shortSymbolClass,
          name.decode,
          owner,
          if (isAdd) "+" else "-",
          flagsToString(mask),
          flagsToString(before),
          changeString
        ))
      }
    }

    protected def verifyFlags(what: String) {
      assert0(this hasAllFlags alwaysHasFlags, symbolCreationString + "\n  always=%s, what=%s\n".format(flagsToString(alwaysHasFlags), what))
      if (this hasFlag neverHasFlags) {
        val hasRaw = (rawflags & neverHasFlags) != 0
        assert0(!hasRaw, symbolCreationString + "\n   never=%s, what=%s".format(flagsToString(neverHasFlags), what))
      }
    }
    abstract override def initFlags(mask: Long): this.type = {
      super.initFlags(mask)
      verifyFlags("initFlags(" + flagsToString(mask) + ")")
      this
    }
    abstract override def setFlag(mask: Long): this.type = {
      verifyChange(true, mask, rawflags)
      super.setFlag(mask)
      verifyFlags("setFlag(" + flagsToString(mask) + ")")
      this
    }
    abstract override def resetFlag(mask: Long): this.type = {
      verifyChange(false, mask, rawflags)
      super.resetFlag(mask)
      verifyFlags("resetFlag(" + flagsToString(mask) + ")")
      this
    }
    abstract override def flags_=(fs: Long) {
      if ((fs & ~rawflags) != 0)
        verifyChange(true, fs & ~rawflags, rawflags)
      if ((rawflags & ~fs) != 0)
        verifyChange(false, rawflags & ~fs, rawflags)
    
      super.flags_=(fs)
      verifyFlags("flags_=(" + flagsToString(fs) + ")")
    }
  }

  /** A distinguishing flag is one which the mixing class must always
   *  have, and which no other symbol class is allowed to have.
   */
  trait DistinguishingFlag extends SymbolFlagLogic {
    this: Symbol =>

    def distinguishingFlag: Long
    override protected def alwaysHasFlags = super.alwaysHasFlags | distinguishingFlag
    override protected def neverHasFlags  = super.neverHasFlags & ~distinguishingFlag
  }

  trait SymbolFlagLogic {
    this: Symbol =>

    // Forced for performance reasons to define all the flag manipulation
    // methods alongside the field being manipulated.
    def getFlag(mask: Long): Long
    def hasFlag(mask: Long): Boolean
    def hasAllFlags(mask: Long): Boolean
    def setFlag(mask: Long): this.type
    def resetFlag(mask: Long): this.type
    def initFlags(mask: Long): this.type
    def resetFlags(): Unit

    protected def resolveOverloadedFlag(flag: Long): String
    protected def calculateFlagString(basis: Long): String

    protected def alwaysHasFlags: Long = 0L
    protected def neverHasFlags: Long = METHOD | MODULE

    def rawFlagString(mask: Long): String = calculateFlagString(rawflags & mask)
    def rawFlagString: String             = rawFlagString(flagMask)
    def debugFlagString: String           = flagString(AllFlags)

    /** String representation of symbol's variance */
    def varianceString: String =
      if (variance == 1) "+"
      else if (variance == -1) "-"
      else ""

    override def flagMask =
      if (settings.debug.value && !isAbstractType) AllFlags
      else if (owner.isRefinementClass) ExplicitFlags & ~OVERRIDE
      else ExplicitFlags

    // make the error message more googlable
    def flagsExplanationString =
      if (isGADTSkolem) " (this is a GADT skolem)"
      else ""

    /** If the given flag is set on this symbol, also set the corresponding
     *  notFLAG.  For instance if flag is PRIVATE, the notPRIVATE flag will
     *  be set if PRIVATE is currently set.
     */
    final def setNotFlag(flag: Int) = if (hasFlag(flag)) setFlag((flag: @annotation.switch) match {
      case PRIVATE   => notPRIVATE
      case PROTECTED => notPROTECTED
      case OVERRIDE  => notOVERRIDE
      case _         => abort("setNotFlag on invalid flag: " + flag)
    })

    protected def shortSymbolClass = getClass.getName.split('.').last.stripPrefix("Symbols$")
    def symbolCreationString: String = (
      "%s%25s | %-40s | %s".format(
        if (settings.uniqid.value) "%06d | ".format(id) else "",
        shortSymbolClass,
        name.decode + " in " + owner,
        rawFlagString
      )
    )
  }
}
