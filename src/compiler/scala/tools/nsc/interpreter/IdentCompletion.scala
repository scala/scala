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
  def completions() = repl.unqualifiedIds
  override def follow(id: String) =
    if (completions contains id) {
      (repl completionAware id) orElse
      (repl completionAwareImplicit id) orElse
      Some(new InstanceCompletion(repl clazzForIdent id))
    }
    else None
}
