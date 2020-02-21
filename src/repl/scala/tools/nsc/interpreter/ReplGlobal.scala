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

import scala.reflect.internal.util.AbstractFileClassLoader
import scala.tools.nsc.classpath.{AggregateClassPath, ClassPathFactory}
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.util.ClassPath

trait ReplGlobal extends Global {
  self =>

  override def abort(msg: String): Nothing = {
    // Using the reporter too early leads to deadlock. TODO: is this still true?
    Console.println("ReplGlobal.abort: " + msg)
    super.abort(msg)
  }

  override def findMacroClassLoader(): ClassLoader = {
    val loader = super.findMacroClassLoader
    analyzer.macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(classPath.asURLs))
    val virtualDirectory = analyzer.globalSettings.outputDirs.getSingleOutput.get
    new AbstractFileClassLoader(virtualDirectory, loader) {}
  }

  override protected def computeInternalPhases(): Unit = {
    super.computeInternalPhases()
    addToPhasesSet(wrapperCleanup, "Remove unused values from import wrappers to avoid unwanted capture of non-serializable objects")
  }

  private val PositiveInt = """\d+""".r

  /** Is the given name of the form created by `lineReadValName`? */
  private def isLineReadVal(name: Global#Name) = {
    import Naming.sessionNames._
    name.startsWith(line) && name.endsWith(read) && (name.subSequence(line.length, name.length - read.length) match {
      case PositiveInt() => true
      case _ => false
    })
  }

  private object wrapperCleanup extends Transform {
    override val global: self.type = self
    override val phaseName: String = "wrapper-cleanup"
    /** Names of phases that must run before this phase. */
    override val runsAfter: List[String] = List("refchecks")
    /** Names of phases that must run after this phase. Default is `Nil`. */
    override val runsBefore: List[String] = List("uncurry")
    /** Name of the phase that this phase must follow immediately. */
    override val runsRightAfter: Option[String] = None
    override protected def newTransformer(unit: CompilationUnit): Transformer = new WrapperCleanupTransformer
    class WrapperCleanupTransformer extends Transformer {
      override def transformUnit(unit: CompilationUnit): Unit = {
        if (settings.Yreplclassbased.value) super.transformUnit(unit)
      }

      def newUnusedPrivates: analyzer.UnusedPrivates = new analyzer.UnusedPrivates() {
        override def isEffectivelyPrivate(sym: Symbol): Boolean = isLineReadVal(sym.name)
      }

      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case tree @ Template(parents, self, body) if nme.isReplWrapperName(tree.symbol.name) =>
          val unusedPrivates = newUnusedPrivates
          unusedPrivates.traverse(tree)
          val unusedSyms = unusedPrivates.unusedTerms.iterator.map(_.symbol)
          val unusedLineReadVals = unusedSyms.filter(sym => isLineReadVal(sym.name)).flatMap(sym => List(sym, sym.accessedOrSelf)).toSet
          val (removedStats, retainedStats) = tree.body.partition (t => unusedLineReadVals(t.symbol))
          if (removedStats.isEmpty) tree
          else {
            val decls = tree.symbol.info.decls
            removedStats.foreach(tree => decls.unlink(tree.symbol))
            deriveTemplate(tree)(_ => retainedStats)
          }
        case tree => tree
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

  override def toString = "<global>"
}
