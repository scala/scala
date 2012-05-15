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

trait SymbolFlags {
  self: SymbolTable =>

  import definitions._

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

    def shortSymbolClass = getClass.getName.split('.').last.stripPrefix("Symbols$")
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
