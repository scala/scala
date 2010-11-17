/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Debug.scala 17777 2009-05-19 18:16:25Z michelou $

package scala.runtime.remoting

/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
object Debug extends Debug {
  override def info   (msg: String) { if (lib) super.info(msg) }
  override def verbose(msg: String) { if (lib) super.verbose(msg) }
  override def warning(msg: String) { if (lib) super.warning(msg) }
  override def error  (msg: String) { if (lib) super.error(msg) }
}

/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
class Debug(tag: String) {

  def this() = this("")

  object Level extends Enumeration {
    type Level = Value
    val SILENT, ERROR, WARNING, VERBOSE, INFO = Value
  }

  private val level0 =
    try {
      val prop = System.getProperty("scala.remoting.logLevel")
      if (prop ne null) prop.toLowerCase else ""
    }
    catch {
      case e =>
        Console.err.println(e.getMessage)
        ""
    }

  import Level._
  protected var (lev, lib) = {
    val p = java.util.regex.Pattern.compile("(error|warning|verbose|info)(\\,lib)?(.*)")
    val m = p matcher level0
    val (s, b) =
      if (m.matches) (m.group(1), m.group(2) ne null)
      else ("", false)
    s match {
      case "error"   => (ERROR  , b)
      case "warning" => (WARNING, b)
      case "verbose" => (VERBOSE, b)
      case "info"    => (INFO   , b)
      case _         => (SILENT , false)
    }
  }

  def level = lev
  def level_= (lev: Level) = { this.lev = lev }

  private val tag0: String =
    if (tag != null & tag.length > 0) tag+" " else ""

  def info(msg: String) {
    if (lev >= INFO) Console.println(tag0 + "(info): " + msg)
  }

  def verbose(msg: String) {
    if (lev >= VERBOSE) Console.println(tag0 + "(verb): " + msg)
  }

  def warning(msg: String) {
    if (lev >= WARNING) Console.err.println(tag0 + "(warn): " + msg)
  }

  def error(msg: String) {
    if (lev >= ERROR) Console.err.println(tag0 + "(erro): " + msg)
  }
}
