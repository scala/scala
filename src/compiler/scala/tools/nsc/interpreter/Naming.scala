/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

trait Naming {
  val global: Global

  import global.{ Name, nme }
  import nme.{
    INTERPRETER_VAR_PREFIX, INTERPRETER_SYNTHVAR_PREFIX, INTERPRETER_LINE_PREFIX
  }

  /** Generates names pre0, pre1, etc. via calls to apply method */
  class NameCreator(pre: String) {
    private var x = -1
    var mostRecent: String = ""

    def apply(): String = {
      x += 1
      mostRecent = pre + x
      mostRecent
    }
    def reset(): Unit = x = -1
    def didGenerate(name: String) =
      (name startsWith pre) && ((name drop pre.length) forall (_.isDigit))
  }

  private lazy val line        = new NameCreator(INTERPRETER_LINE_PREFIX)     // line name, like line$0
  private lazy val userVar     = new NameCreator(INTERPRETER_VAR_PREFIX)      // var name, like res0
  private lazy val internalVar = new NameCreator(INTERPRETER_SYNTHVAR_PREFIX) // internal var name, like $synthvar0

  def isUserVarName(name: String)              = userVar didGenerate name
  def isInternalVarName(name: String): Boolean = internalVar didGenerate name
  def isInternalVarName(name: Name): Boolean   = internalVar didGenerate name.toString

  val freshLineId            = {
    var x = 0
    () => { x += 1 ; x }
  }
  def freshLineName()        = line()
  def freshUserVarName()     = userVar()
  def freshInternalVarName() = internalVar()

  def resetAllCreators() {
    line.reset()
    userVar.reset()
    internalVar.reset()
  }

  def mostRecentVar = userVar.mostRecent
}
