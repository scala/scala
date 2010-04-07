/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** Top level identifiers visible in the repl.  It immediately
 *  delegates to an InstanceCompletion.
 */
class IdentCompletion(repl: Interpreter) extends CompletionAware {
  val INTERPRETER_VAR_PREFIX = "res"

  def completions() = repl.unqualifiedIds ::: List("classOf")
  override def follow(id: String) =
    // XXX this will be nice but needs solidifying.
    // (repl completionAwareImplicit id) orElse
    if (completions contains id) {
      (repl completionAware id) orElse {
        repl clazzForIdent id map (x => new InstanceCompletion(x))
      }
    }
    else None
}
