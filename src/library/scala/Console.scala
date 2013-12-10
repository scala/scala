/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import java.io.{ BufferedReader, InputStream, InputStreamReader, OutputStream, PrintStream, Reader }
import scala.io.{ AnsiColor, StdIn }
import scala.util.DynamicVariable

/** Implements functionality for
 *  printing Scala values on the terminal as well as reading specific values.
 *  Also defines constants for marking up text on ANSI terminals.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 */
object Console extends DeprecatedConsole with AnsiColor {
  private val outVar = new DynamicVariable[PrintStream](java.lang.System.out)
  private val errVar = new DynamicVariable[PrintStream](java.lang.System.err)
  private val inVar  = new DynamicVariable[BufferedReader](
    new BufferedReader(new InputStreamReader(java.lang.System.in)))

  protected def setOutDirect(out: PrintStream): Unit  = outVar.value = out
  protected def setErrDirect(err: PrintStream): Unit  = errVar.value = err
  protected def setInDirect(in: BufferedReader): Unit = inVar.value = in

  /** The default output, can be overridden by `setOut` */
  def out = outVar.value
  /** The default error, can be overridden by `setErr` */
  def err = errVar.value
  /** The default input, can be overridden by `setIn` */
  def in = inVar.value

  /** Sets the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @example {{{
   *  withOut(Console.err) { println("This goes to default _error_") }
   *  }}}
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return the results of `thunk`
   *  @see `withOut[T](out:OutputStream)(thunk: => T)`
   */
  def withOut[T](out: PrintStream)(thunk: =>T): T =
    outVar.withValue(out)(thunk)

  /** Sets the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return the results of `thunk`
   *  @see `withOut[T](out:PrintStream)(thunk: => T)`
   */
  def withOut[T](out: OutputStream)(thunk: =>T): T =
    withOut(new PrintStream(out))(thunk)

  /** Set the default error stream for the duration
   *  of execution of one thunk.
   *  @example {{{
   *  withErr(Console.out) { println("This goes to default _out_") }
   *  }}}
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return the results of `thunk`
   *  @see `withErr[T](err:OutputStream)(thunk: =>T)`
   */
  def withErr[T](err: PrintStream)(thunk: =>T): T =
    errVar.withValue(err)(thunk)

  /** Sets the default error stream for the duration
   *  of execution of one thunk.
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return the results of `thunk`
   *  @see `withErr[T](err:PrintStream)(thunk: =>T)`
   */
  def withErr[T](err: OutputStream)(thunk: =>T): T =
    withErr(new PrintStream(err))(thunk)

  /** Sets the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @example {{{
   *  val someFile:Reader = openFile("file.txt")
   *  withIn(someFile) {
   *    // Reads a line from file.txt instead of default input
   *    println(readLine)
   *  }
   *  }}}
   *
   *  @param thunk the code to execute with
   *               the new input stream active
   *
   * @return the results of `thunk`
   * @see `withIn[T](in:InputStream)(thunk: =>T)`
   */
  def withIn[T](reader: Reader)(thunk: =>T): T =
    inVar.withValue(new BufferedReader(reader))(thunk)

  /** Sets the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @param in the new input stream.
   *  @param thunk the code to execute with
   *               the new input stream active
   * @return the results of `thunk`
   * @see `withIn[T](reader:Reader)(thunk: =>T)`
   */
  def withIn[T](in: InputStream)(thunk: =>T): T =
    withIn(new InputStreamReader(in))(thunk)

  /** Prints an object to `out` using its `toString` method.
   *
   *  @param obj the object to print; may be null.
   */
  def print(obj: Any) {
    out.print(if (null == obj) "null" else obj.toString())
  }

  /** Flushes the output stream. This function is required when partial
   *  output (i.e. output not terminated by a newline character) has
   *  to be made visible on the terminal.
   */
  def flush() { out.flush() }

  /** Prints a newline character on the default output.
   */
  def println() { out.println() }

  /** Prints out an object to the default output, followed by a newline character.
   *
   *  @param x the object to print.
   */
  def println(x: Any) { out.println(x) }

  /** Prints its arguments as a formatted string to the default output,
   *  based on a string pattern (in a fashion similar to printf in C).
   *
   *  The interpretation of the formatting patterns is described in
   *  <a href="" target="contentFrame" class="java/util/Formatter">
   *  `java.util.Formatter`</a>.
   *
   *  @param text the pattern for formatting the arguments.
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException if there was a problem with the format string or arguments
   */
  def printf(text: String, args: Any*) { out.print(text format (args : _*)) }
}

private[scala] abstract class DeprecatedConsole {
  self: Console.type =>

  /** Internal usage only. */
  protected def setOutDirect(out: PrintStream): Unit
  protected def setErrDirect(err: PrintStream): Unit
  protected def setInDirect(in: BufferedReader): Unit

  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readBoolean(): Boolean                     = StdIn.readBoolean()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readByte(): Byte                           = StdIn.readByte()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readChar(): Char                           = StdIn.readChar()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readDouble(): Double                       = StdIn.readDouble()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readFloat(): Float                         = StdIn.readFloat()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readInt(): Int                             = StdIn.readInt()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readLine(): String                         = StdIn.readLine()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readLine(text: String, args: Any*): String = StdIn.readLine(text, args: _*)
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readLong(): Long                           = StdIn.readLong()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readShort(): Short                         = StdIn.readShort()
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readf(format: String): List[Any]           = StdIn.readf(format)
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readf1(format: String): Any                = StdIn.readf1(format)
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readf2(format: String): (Any, Any)         = StdIn.readf2(format)
  @deprecated("Use the method in scala.io.StdIn", "2.11.0") def readf3(format: String): (Any, Any, Any)    = StdIn.readf3(format)

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  @deprecated("Use withOut", "2.11.0") def setOut(out: PrintStream): Unit = setOutDirect(out)

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  @deprecated("Use withOut", "2.11.0") def setOut(out: OutputStream): Unit = setOutDirect(new PrintStream(out))

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  @deprecated("Use withErr", "2.11.0") def setErr(err: PrintStream): Unit = setErrDirect(err)

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  @deprecated("Use withErr", "2.11.0") def setErr(err: OutputStream): Unit = setErrDirect(new PrintStream(err))

  /** Sets the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  @deprecated("Use withIn", "2.11.0") def setIn(reader: Reader): Unit = setInDirect(new BufferedReader(reader))

  /** Sets the default input stream.
   *
   *  @param in the new input stream.
   */
  @deprecated("Use withIn", "2.11.0") def setIn(in: InputStream): Unit = setInDirect(new BufferedReader(new InputStreamReader(in)))
}
