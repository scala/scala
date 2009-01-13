/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import java.io.{OutputStream, PrintStream}

import Predef._


/** The <code>Console</code> object implements functionality for
 *  printing Scala values on the terminal. There are also functions
 *  for reading specific values. <code>Console</code> also defines
 *  constants for marking up text on ANSI terminals.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 */
object Console {

  // ANSI colors foreground
  final val BLACK      = "\033[30m"
  final val RED        = "\033[31m"
  final val GREEN      = "\033[32m"
  final val YELLOW     = "\033[33m"
  final val BLUE       = "\033[34m"
  final val MAGENTA    = "\033[35m"
  final val CYAN       = "\033[36m"
  final val WHITE      = "\033[37m"

  // ANSI colors background
  final val BLACK_B    = "\033[40m"
  final val RED_B      = "\033[41m"
  final val GREEN_B    = "\033[42m"
  final val YELLOW_B   = "\033[43m"
  final val BLUE_B     = "\033[44m"
  final val MAGENTA_B  = "\033[45m"
  final val CYAN_B     = "\033[46m"
  final val WHITE_B    = "\033[47m"

  // ANSI styles
  final val RESET      = "\033[0m"
  final val BOLD       = "\033[1m"
  final val UNDERLINED = "\033[4m"
  final val BLINK      = "\033[5m"
  final val REVERSED   = "\033[7m"
  final val INVISIBLE  = "\033[8m"

  var out: PrintStream = java.lang.System.out
  val err = java.lang.System.err

  /** Set the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: PrintStream): Unit = this.out = out

  /** Set the default output stream.
   *
   *  @param@ out the new output stream.
   */
  def setOut(out: OutputStream): Unit =
    setOut(new PrintStream(out))

  /** Print an object on the terminal.
   *
   *  @param obj the object to print.
   */
  def print(obj: Any): Unit =
    out.print(if (null == obj) "null" else obj.toString())

  /** Flush the output stream. This function is required when partial
   *  output (i.e. output not terminated by a new line character) has
   *  to be made visible on the terminal.
   */
  def flush(): Unit = out.flush()

  /** Print a new line character on the terminal.
   */
  def println(): Unit = out.println()

  /** Print out an object followed by a new line character.
   *
   *  @param x the object to print.
   */
  def println(x: Any): Unit = out.println(x)

}
