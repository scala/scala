/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.util.{ List => JList }

/** An implementation-agnostic completion interface which makes no
 *  reference to the jline classes.
 */
trait Completion {
  type ExecResult
  trait Instance {
    def complete(buffer: String, cursor: Int, candidates: JList[CharSequence]): Int
  }
  def resetVerbosity(): Unit
  def execute(line: String): Option[ExecResult]
  def completer(): Instance
}

object Completion {
  object Empty extends Completion {
    type ExecResult = Nothing
    object NullCompleter extends Instance {
      def complete(buffer: String, cursor: Int, candidates: JList[CharSequence]) = -1
    }
    def resetVerbosity() = ()
    def execute(line: String) = None
    def completer() = NullCompleter
  }
  def looksLikeInvocation(code: String) = (
        (code != null)
    &&  (code startsWith ".")
    && !(code == ".")
    && !(code startsWith "./")
    && !(code startsWith "..")
  )
  def looksLikePath(code: String) = (code != null) && (code.length >= 2) && (
    Set("/", "\\", "./", "../", "~/") exists (code startsWith _)
  )
  object Forwarder {
    def apply(forwardTo: () => Option[CompletionAware]): CompletionAware = new CompletionAware {
      def completions(verbosity: Int) = forwardTo() map (_ completions verbosity) getOrElse Nil
      override def follow(s: String) = forwardTo() flatMap (_ follow s)
    }
  }
}
