/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import ch.epfl.lamp.compiler.msil.{ Type => MSILType }
import util.MsilClassPath
import util.MsilClassPath.MsilContext
import util.ClassPath.{ isTraitImplementation }
import msil.GenMSIL

trait MSILPlatform extends Platform[MSILType] {
  import global._

  if (settings.verbose.value)
    inform("[AssemRefs = " + settings.assemrefs.value + "]")

  // phaseName = "msil"
  object genMSIL extends {
    val global: MSILPlatform.this.global.type = MSILPlatform.this.global
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenMSIL

  lazy val classPath: MsilClassPath = {
    val context =
      if (isInlinerOn) new MsilContext
      else new MsilContext {
        override def isValidName(name: String) = !isTraitImplementation(name)
      }

    new MsilClassPath(settings.assemextdirs.value, settings.assemrefs.value, settings.sourcepath.value, context)
  }

  def rootLoader = new loaders.NamespaceLoader(classPath)

  def platformPhases = List(
    genMSIL   // generate .msil files
  )
}
