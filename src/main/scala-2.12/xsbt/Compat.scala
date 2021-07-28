/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import java.io.PrintWriter
import xsbti.compile.Output
import scala.tools.nsc.Global
import scala.tools.nsc.Settings

abstract class Compat {
  val global: Global
  import global._

  /** If given tree contains object tree attachment calls func on tree from attachment. */
  protected def processOriginalTreeAttachment(in: Tree)(func: Tree => Unit): Unit = {
    Compat.OriginalTreeTraverser.Instance.traverseOriginal(in)(func)
  }
}
object Compat {
  // IR is renamed to Results
  val Results = scala.tools.nsc.interpreter.IR

  // IMain in 2.13 accepts ReplReporter
  def replReporter(settings: Settings, writer: PrintWriter) = writer

  sealed abstract class OriginalTreeTraverser private {
    def traverseOriginal[T <: Global#Tree](t: T)(f: T => Unit): Unit
  }

  object OriginalTreeTraverser {
    private[this] val cls = try {
      Class.forName("scala.tools.nsc.typechecker.StdAttachments$OriginalTreeAttachment")
    } catch { case _: Throwable => null }

    private object Reflective extends OriginalTreeTraverser {
      private[this] val ct = scala.reflect.ClassTag[AnyRef](cls)
      private[this] val meth = cls.getMethod("original")
      def traverseOriginal[T <: Global#Tree](t: T)(f: T => Unit): Unit =
        t.attachments.get(ct) match {
          case Some(attachment) => f(meth.invoke(attachment).asInstanceOf[T])
          case None             =>
        }
    }

    private object NoOp extends OriginalTreeTraverser {
      def traverseOriginal[T <: Global#Tree](t: T)(f: T => Unit): Unit = ()
    }

    val Instance = if (cls == null) NoOp else Reflective
  }
}

/** Defines compatibility utils for [[ZincCompiler]]. */
trait ZincGlobalCompat {
  protected def superDropRun(): Unit = ()
}

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler(settings: Settings, reporter: DelegatingReporter, output: Output): ZincCompiler =
    new ZincCompiler(settings, reporter, output)
}
