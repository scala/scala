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

package scala.tools.nsc
package interactive
package tests.core

import scala.tools.nsc.reporters.{Reporter => CompilerReporter}

/** Trait encapsulating the creation of a presentation compiler's instance.*/
private[tests] trait PresentationCompilerInstance extends TestSettings {
  protected val settings = new Settings

  protected val compilerReporter: CompilerReporter = new InteractiveReporter {
    override def compiler = PresentationCompilerInstance.this.compiler
  }

  protected def createGlobal: Global = new Global(settings, compilerReporter)

  protected lazy val compiler: Global = {
    prepareSettings(settings)
    createGlobal
  }

  /**
   * Called before instantiating the presentation compiler's instance.
   * You should provide an implementation of this method if you need
   * to customize the `settings` used to instantiate the presentation compiler.
   * */
  protected def prepareSettings(settings: Settings): Unit = ()

  protected def printClassPath(implicit reporter: Reporter): Unit = {
    reporter.println("\tbootClassPath: %s".format(settings.bootclasspath.value))
    reporter.println("\tverbose: %b".format(settings.verbose.value))
  }
}
