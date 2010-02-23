/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import ch.epfl.lamp.compiler.msil.{ Type => MSILType }
import util.MsilClassPath
import msil.GenMSIL

trait MSILPlatform extends Platform[MSILType] {
  import global._
  import definitions.{ ComparatorClass, BoxedNumberClass, getMember, getClass }

  if (settings.verbose.value)
    inform("[AssemRefs = " + settings.assemrefs.value + "]")

  // phaseName = "msil"
  object genMSIL extends {
    val global: MSILPlatform.this.global.type = MSILPlatform.this.global
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenMSIL

  lazy val classPath = MsilClassPath.fromSettings(settings)
  def rootLoader = new loaders.NamespaceLoader(classPath)

  def platformPhases = List(
    genMSIL   // generate .msil files
  )

  lazy val externalEquals = getMember(ComparatorClass.linkedModuleOfClass, nme.equals_)
  def isMaybeBoxed(sym: Symbol) = sym isNonBottomSubClass BoxedNumberClass
}
