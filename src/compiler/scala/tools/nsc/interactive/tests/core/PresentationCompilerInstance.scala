package scala.tools.nsc
package interactive
package tests.core

import reporters.{Reporter => CompilerReporter}
import scala.reflect.internal.util.Position

/** Trait encapsulating the creation of a presentation compiler's instance.*/
private[tests] trait PresentationCompilerInstance extends TestSettings {
  protected val settings = new Settings
  protected val withDocComments = false

  protected val compilerReporter: CompilerReporter = new InteractiveReporter {
    override def compiler = PresentationCompilerInstance.this.compiler
  }

  protected lazy val compiler: Global = {
    prepareSettings(settings)
    new Global(settings, compilerReporter) {
      override def forScaladoc = withDocComments
    }
  }

  /**
   * Called before instantiating the presentation compiler's instance.
   * You should provide an implementation of this method if you need
   * to customize the `settings` used to instantiate the presentation compiler.
   * */
  protected def prepareSettings(settings: Settings) {}

  protected def printClassPath(implicit reporter: Reporter) {
    reporter.println("\tbootClassPath: %s".format(settings.bootclasspath.value))
    reporter.println("\tverbose: %b".format(settings.verbose.value))
  }
}
