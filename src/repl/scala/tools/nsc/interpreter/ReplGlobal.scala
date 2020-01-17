/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import scala.tools.nsc.classpath.{AggregateClassPath, ClassPathFactory}
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.util.ClassPath
import typechecker.Analyzer

/** A layer on top of Global so I can guarantee some extra
 *  functionality for the repl.
 */
trait ReplGlobal extends Global {
  self =>
  // This exists mostly because using the reporter too early leads to deadlock.
  private def echo(msg: String) { Console println msg }

  override def abort(msg: String): Nothing = {
    echo("ReplGlobal.abort: " + msg)
    super.abort(msg)
  }

  override lazy val analyzer = new {
    val global: ReplGlobal.this.type = ReplGlobal.this
  } with Analyzer {
    override protected def findMacroClassLoader(): ClassLoader = {
      val loader = super.findMacroClassLoader
      macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(global.classPath.asURLs))
      val virtualDirectory = globalSettings.outputDirs.getSingleOutput.get
      new util.AbstractFileClassLoader(virtualDirectory, loader) {}
    }
  }

  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()
    addToPhasesSet(wrapperCleanup, "Remove unused values from import wrappers to avoid unwanted capture of non-serializable objects")
  }

  private object wrapperCleanup extends Transform {
    override val global: self.type = self
    override val phaseName: String = "wrapper-cleanup"
    /** Names of phases that must run before this phase. */
    override val runsAfter: List[String] = List("refchecks")
    /** Name of the phase that this phase must follow immediately. */
    override val runsRightAfter: Option[String] = None
    override protected def newTransformer(unit: CompilationUnit): Transformer = new WrapperCleanupTransformer(unit)
    class WrapperCleanupTransformer(unit: CompilationUnit) extends Transformer {
      val unusedPrivates = new analyzer.UnusedPrivates()
      unusedPrivates.traverse(unit.body)
      val unusedSet = unusedPrivates.unusedTerms.iterator.flatMap(tree => List(tree.symbol, tree.symbol.accessedOrSelf)).toSet
      override def transformTemplate(tree: Template): Template = {
        val (unused, used) = tree.body.partition (t => unusedSet(t.symbol))
        if (unused.isEmpty ) tree
        else {
          val decls = tree.symbol.info.decls
          unused.foreach(tree => decls.unlink(tree.symbol))
          deriveTemplate(tree)(_ => used)
        }
      }
    }
  }

  override def optimizerClassPath(base: ClassPath): ClassPath = {
    settings.outputDirs.getSingleOutput match {
      case None => base
      case Some(out) =>
        // Make bytecode of previous lines available to the inliner
        val replOutClasspath = ClassPathFactory.newClassPath(settings.outputDirs.getSingleOutput.get, settings, closeableRegistry)
        AggregateClassPath.createAggregate(platform.classPath, replOutClasspath)
    }
  }
}
