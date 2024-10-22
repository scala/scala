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
package interactive
package tests.core

import scala.annotation.unused
import scala.tools.nsc.interactive.Response
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.SourceFile

/**
 * A trait for defining commands that can be queried to the
 * presentation compiler.
 * */
trait AskCommand {

  /** presentation compiler's instance. */
  protected val compiler: Global

  /**
   * Presentation compiler's `askXXX` actions work by doing side-effects
   * on a `Response` instance passed as an argument during the `askXXX`
   * call.
   * The defined method `ask` is meant to encapsulate this behavior.
   * */
  protected def ask[T](op: Response[T] => Unit): Response[T] = {
    val r = new Response[T]
    op(r)
    r
  }
}

/** Ask the presentation compiler to shut-down. */
trait AskShutdown extends AskCommand {
  def askShutdown() = compiler.askShutdown()
}

/** Ask the presentation compiler to parse a sequence of `sources` */
trait AskParse extends AskCommand {
  import compiler.Tree

  /** `sources` need to be entirely parsed before running the test
   *  (else commands such as `AskTypeCompletionAt` may fail simply because
   *  the source's AST is not yet loaded).
   *
   *  Submit each parse job and get the response sequentially;
   *  otherwise, parser progress update will run the next job.
   */
  def askParse(sources: Seq[SourceFile]): Unit =
    for (source <- sources) {
      val _ = askParse(source).get
    }

  private def askParse(src: SourceFile, keepLoaded: Boolean = true): Response[Tree] = {
    ask {
      compiler.askParsedEntered(src, keepLoaded, _)
    }
  }
}

/** Ask the presentation compiler to reload a sequence of `sources` */
trait AskReload extends AskCommand {

  /** Reload the given source files and wait for them to be reloaded. */
  protected def askReload(sources: Seq[SourceFile])(implicit reporter: Reporter): Response[Unit] = {
    val sortedSources = (sources map (_.file.name)).sorted
    reporter.println("reload: " + sortedSources.mkString(", "))

    ask {
      compiler.askReload(sources.toList, _)
    }
  }
}

/** Ask the presentation compiler for completion at a given position. */
trait AskTypeCompletionAt extends AskCommand {
  import compiler.Member

  private[tests] def askTypeCompletionAt(pos: Position)(implicit reporter: Reporter): Response[List[Member]] = {
    reporter.println("\naskTypeCompletion at " + pos.source.file.name + ((pos.line, pos.column)))

    ask {
      compiler.askTypeCompletion(pos, _)
    }
  }
}

/** Ask the presentation compiler for scope completion at a given position. */
trait AskScopeCompletionAt extends AskCommand {
  import compiler.Member

  private[tests] def askScopeCompletionAt(pos: Position)(implicit reporter: Reporter): Response[List[Member]] = {
    reporter.println("\naskScopeCompletion at " + pos.source.file.name + ((pos.line, pos.column)))

    ask {
      compiler.askScopeCompletion(pos, _)
    }
  }
}

/** Ask the presentation compiler for type info at a given position. */
trait AskTypeAt extends AskCommand {
  import compiler.Tree

  private[tests] def askTypeAt(pos: Position)(implicit reporter: Reporter): Response[Tree] = {
    reporter.println("\naskType at " + pos.source.file.name + ((pos.line, pos.column)))

    ask {
      compiler.askTypeAt(pos, _)
    }
  }
}

trait AskLoadedTyped extends AskCommand {
  import compiler.Tree

  protected def askLoadedTyped(source: SourceFile, keepLoaded: Boolean = false)(implicit @unused reporter: Reporter): Response[Tree] = {
    ask {
      compiler.askLoadedTyped(source, keepLoaded, _)
    }
  }

}
