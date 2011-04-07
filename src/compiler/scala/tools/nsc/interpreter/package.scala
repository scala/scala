/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

/** The main REPL related classes and values are as follows.
 *  In addition to standard compiler classes Global and Settings, there are:
 *
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  ILoop (formerly InterpreterLoop): The umbrella class for a session.
 *  IMain (formerly Interpreter): Handles the evolving state of the session
 *    and handles submitting code to the compiler and handling the output.
 *  InteractiveReader: how ILoop obtains input.
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  Power: a repository for more advanced/experimental features.
 *
 *  ILoop contains { in: InteractiveReader, intp: IMain, settings: Settings, power: Power }
 *  InteractiveReader contains { history: History, completion: Completion }
 *  IMain contains { global: Global }
 */
package object interpreter extends ReplConfig {
  type JFile          = java.io.File
  type JClass         = java.lang.Class[_]
  type JList[T]       = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type InputStream    = java.io.InputStream
  type OutputStream   = java.io.OutputStream

  private[nsc] implicit def enrichClass[T](clazz: Class[T]) = new RichClass[T](clazz)
  private[interpreter] implicit def javaCharSeqCollectionToScala(xs: JCollection[_ <: CharSequence]): List[String] = {
    import collection.JavaConverters._
    xs.asScala.toList map ("" + _)
  }

  // Longest common prefix
  def longestCommonPrefix(xs: List[String]): String = {
    if (xs.isEmpty || xs.contains("")) ""
    else xs.head.head match {
      case ch =>
        if (xs.tail forall (_.head == ch)) "" + ch + longestCommonPrefix(xs map (_.tail))
        else ""
    }
  }

  private[nsc] def words(s: String) = s.trim split "\\s+" toList
  private[nsc] def isQuoted(s: String) =
    (s.length >= 2) && (s.head == s.last) && ("\"'" contains s.head)

  /** Class objects */
  private[nsc] def classForName(name: String): Option[JClass] =
    try Some(Class forName name)
    catch { case _: ClassNotFoundException | _: SecurityException => None }
}
