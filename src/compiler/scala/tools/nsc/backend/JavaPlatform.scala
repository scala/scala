/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import util.JavaClassPath
import util.ClassPath.{ JavaContext, DefaultJavaContext }
import scala.tools.util.PathResolver

trait JavaPlatform extends Platform[AbstractFile] {
  import global._

  lazy val classPath: JavaClassPath = {
    val context =
      if (isInlinerOn) new JavaContext
      else DefaultJavaContext

    val cp = PathResolver.fromSettings(settings, context)
    if (settings.Ylogcp.value)
      Console.println("Created Global has classpath: " + cp.asClasspathString)

    cp
  }

  def rootLoader = new loaders.JavaPackageLoader(classPath)

  private def depAnalysisPhase = if (settings.make.value != "all") List(dependencyAnalysis) else Nil
  def platformPhases = List(
    flatten,    // get rid of inner classes
    liftcode,   // generate reified trees
    genJVM      // generate .class files
  ) ::: depAnalysisPhase
}
