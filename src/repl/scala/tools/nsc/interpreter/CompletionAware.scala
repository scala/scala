/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** An interface for objects which are aware of tab completion and
 *  will supply their own candidates and resolve their own paths.
 */
trait CompletionAware {
  /** The complete list of unqualified Strings to which this
   *  object will complete.
   */
  def completions(verbosity: Int): List[String]

  /** The next completor in the chain.
   */
  def follow(id: String): Option[CompletionAware] = None

  /** A list of useful information regarding a specific uniquely
   *  identified completion.  This is specifically written for the
   *  following situation, but should be useful elsewhere too:
   *
   *    x.y.z.methodName<tab>
   *
   *  If "methodName" is among z's completions, and verbosity > 0
   *  indicating tab has been pressed twice consecutively, then we
   *  call alternativesFor and show a list of overloaded method
   *  signatures.
   */
  def alternativesFor(id: String): List[String] = Nil

  /** Given string 'buf', return a list of all the strings
   *  to which it can complete.  This may involve delegating
   *  to other CompletionAware objects.
   */
  def completionsFor(parsed: Parsed): List[String] = {
    import parsed.{ buffer, verbosity }
    val comps = completions(verbosity) filter (_ startsWith buffer)
    val exact = comps contains buffer

    val results =
      if (parsed.isEmpty) comps
      else if (parsed.isUnqualified && !parsed.isLastDelimiter)
        if (verbosity > 0 && exact) alternativesFor(buffer)
        else comps
      else follow(parsed.bufferHead) map (_ completionsFor parsed.bufferTail) getOrElse Nil

    results.sorted
  }
}
