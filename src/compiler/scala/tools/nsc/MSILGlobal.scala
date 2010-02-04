/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc

import reporters.{ Reporter, ConsoleReporter }
import util.MsilClassPath
import backend.msil.GenMSIL

/** A subclass of Global which isolates the dependencies on msil.jar.
 */

class MSILGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  // alternate constructors ------------------------------------------
  def this(reporter: Reporter) = this(new Settings(err => reporter.error(null, err)), reporter)
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))

  // phaseName = "msil"
  object genMSIL extends {
    val global: MSILGlobal.this.type = MSILGlobal.this
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenMSIL

  override lazy val classPath: MsilClassPath = new MsilClassPath(
    settings.assemextdirs.value, settings.assemrefs.value,
    settings.sourcepath.value, validClassPathName
  )

  override def rootLoader: LazyType = new loaders.NamespaceLoader(classPath)

  override protected def computeInternalPhases() {
    super.computeInternalPhases()
    phasesSet += genMSIL			       // generate .msil files
  }

  if (settings.verbose.value)
    inform("[AssemRefs = " + settings.assemrefs.value + "]")
}

