/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.PrintWriter

object NestUI {

  val NONE = 0
  val SOME = 1
  val MANY = 2

  private var _outline = ""
  private var _success = ""
  private var _failure = ""
  private var _warning = ""
  private var _default = ""

  def initialize(number: Int) = number match {
    case MANY =>
      _outline = Console.BOLD + Console.BLACK
      _success = Console.BOLD + Console.GREEN
      _failure = Console.BOLD + Console.RED
      _warning = Console.BOLD + Console.YELLOW
      _default = Console.RESET
    case SOME =>
      _outline = Console.BOLD + Console.BLACK
      _success = Console.RESET
      _failure = Console.BOLD + Console.BLACK
      _warning = Console.BOLD + Console.BLACK
      _default = Console.RESET
    case _ =>
  }

  def outline(msg: String) = print(_outline + msg + _default)
  def outline(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_outline + msg + _default)
  }

  def success(msg: String) = print(_success  + msg + _default)
  def success(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_success + msg + _default)
  }

  def failure(msg: String) = print(_failure  + msg + _default)
  def failure(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_failure + msg + _default)
  }

  def warning(msg: String) = print(_warning  + msg + _default)
  def warning(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_warning + msg + _default)
  }

  def normal(msg: String) = print(_default + msg)
  def normal(msg: String, wr: PrintWriter) = synchronized {
    wr.print(_default + msg)
  }

  def usage() {
    println("Usage: NestRunner [<options>] [<testfile> ..] [<resfile>]")
    println("version Mar13")
    println("    --all          run all tests")
    println("    --pos          next files test a compilation success")
    println("    --neg          next files test a compilation failure")
    println("    --run          next files test the interpreter and all backends")
    println("    --jvm          next files test the JVM backend")
    println("    --jvm5         next files test the JVM backend")
    println("    --res          next files test the resident compiler")
    println("    --shootout     next files are shootout tests")
    println("    --script       next files test the script runner")
    println("    --verbose      display progress information")
    //println("    --version      display version information")
    println
    println("Send bugs to <scala@listes.epfl.ch>")
    exit(1)
  }


  var _verbose = false

  def verbose(msg: String) {
    if (_verbose) {
      outline("debug: ")
      println(msg)
    }
  }

}
