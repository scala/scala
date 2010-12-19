/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

package object interpreter {
  private[nsc] val DebugProperty = "scala.repl.debug"
  private[nsc] var _debug = false
  private[nsc] def isReplDebug = _debug || (system.props contains DebugProperty)

  /** Debug output */
  def repldbg(msg: String) = if (isReplDebug) Console println msg

  /** Tracing */
  def tracing[T](msg: String)(x: T): T = {
    if (isReplDebug)
      println("(" + msg + ") " + x)

    x
  }

  /** Frequency counter */
  def freq[T](seq: Seq[T]) = seq groupBy identity mapValues (_.length)

  /** Heuristically strip interpreter wrapper prefixes
   *  from an interpreter output string.
   */
  def stripWrapperGunk(str: String): String = {
    val wrapregex = """(line[0-9]+\$object[$.])?(\$iw[$.])*"""
    str.replaceAll(wrapregex, "")
  }

  /** Class objects */
  def classForName(name: String): Option[Class[_]] =
    try Some(Class forName name)
    catch { case _: ClassNotFoundException | _: SecurityException => None }
}
