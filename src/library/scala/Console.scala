/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import java.io.{ BufferedReader, InputStream, InputStreamReader, OutputStream, PrintStream, Reader }
import scala.io.{ AnsiColor, StdIn }
import scala.util.DynamicVariable

/** Implements functionality for printing Scala values on the terminal. For reading values
 *  use [[scala.io.StdIn$ StdIn]].
 *  Also defines constants for marking up text on ANSI terminals.
 *
 *  == Console Output ==
 *
 *  Use the print methods to output text.
 *  {{{
 *   scala> Console.printf(
 *     "Today the outside temperature is a balmy %.1f°C. %<.1f°C beats the previous record of %.1f°C.\n",
 *     -137.0,
 *     -135.05)
 *   Today the outside temperature is a balmy -137.0°C. -137.0°C beats the previous record of -135.1°C.
 *  }}}
 *
 *  == ANSI escape codes ==
 *  Use the ANSI escape codes for colorizing console output either to STDOUT or STDERR.
 *  {{{
 *    import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}
 *
 *    object PrimeTest {
 *
 *      def isPrime(): Unit = {
 *
 *        val candidate = io.StdIn.readInt().ensuring(_ > 1)
 *
 *        val prime = (2 to candidate - 1).forall(candidate % _ != 0)
 *
 *        if (prime)
 *          Console.println(s"${RESET}${GREEN}yes${RESET}")
 *        else
 *          Console.err.println(s"${RESET}${YELLOW_B}${RED}${UNDERLINED}NO!${RESET}")
 *      }
 *
 *      def main(args: Array[String]): Unit = isPrime()
 *
 *    }
 *  }}}
 *
 *  <table style="border: 10px solid #000;width:100%">
 *    <tr><td style="background-color:#000;color:#fff">$ scala PrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">1234567891</td></tr>
 *    <tr><td style="background-color:#000;color:#0f0">yes</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">$ scala PrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">56474</td></tr>
 *    <tr><td style="background-color:#000;color:#fff"><span style="background-color:#ff0;color:#f00;text-decoration:underline">NO!</span></td></tr>
 *  </table>
 *
 *  == IO redefinition ==
 *
 *  Use IO redefinition to temporarily swap in a different set of input and/or output streams. In this example the stream based
 *  method above is wrapped into a function.
 *
 *  {{{
 *    import java.io.{ByteArrayOutputStream, StringReader}
 *
 *    object FunctionalPrimeTest {
 *
 *      def isPrime(candidate: Int): Boolean = {
 *
 *        val input = new StringReader(s"$candidate\n")
 *        val outCapture = new ByteArrayOutputStream
 *        val errCapture = new ByteArrayOutputStream
 *
 *        Console.withIn(input) {
 *          Console.withOut(outCapture) {
 *            Console.withErr(errCapture) {
 *              PrimeTest.isPrime()
 *            }
 *          }
 *        }
 *
 *        if (outCapture.toByteArray.nonEmpty) // "yes"
 *          true
 *        else if (errCapture.toByteArray.nonEmpty) // "NO!"
 *          false
 *        else throw new IllegalArgumentException(candidate.toString)
 *      }
 *
 *      def main(args: Array[String]): Unit = {
 *        val primes = (2 to 50) filter (isPrime)
 *        println(s"First primes: $primes")
 *      }
 *
 *    }
 *  }}}
 *
 *
 *  <table style="border: 10px solid #000;width:100%">
 *    <tr><td style="background-color:#000;color:#fff">$ scala FunctionalPrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">First primes: Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)</td></tr>
 *  </table>
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 *
 *  @groupname console-output Console Output
 *  @groupprio console-output 30
 *  @groupdesc console-output These methods provide output via the console.
 *
 *  @groupname io-default IO Defaults
 *  @groupprio io-default 50
 *  @groupdesc io-default These values provide direct access to the standard IO channels
 *
 *  @groupname io-redefinition IO Redefinition
 *  @groupprio io-redefinition 60
 *  @groupdesc io-redefinition These methods allow substituting alternative streams for the duration of
 *             a body of code. Threadsafe by virtue of [[scala.util.DynamicVariable]].
 *
 */
object Console extends DeprecatedConsole with AnsiColor {
  private val outVar = new DynamicVariable[PrintStream](java.lang.System.out)
  private val errVar = new DynamicVariable[PrintStream](java.lang.System.err)
  private val inVar  = new DynamicVariable[BufferedReader](
    new BufferedReader(new InputStreamReader(java.lang.System.in)))

  protected def setOutDirect(out: PrintStream): Unit  = outVar.value = out
  protected def setErrDirect(err: PrintStream): Unit  = errVar.value = err
  protected def setInDirect(in: BufferedReader): Unit = inVar.value = in

  /** The default output, can be overridden by `withOut`
   *  @group io-default
   */
  def out = outVar.value
  /** The default error, can be overridden by `withErr`
   *  @group io-default
   */
  def err = errVar.value
  /** The default input, can be overridden by `withIn`
   *  @group io-default
   */
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
   *  @group io-redefinition
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
   *  @group io-redefinition
   */
  def withOut[T](out: OutputStream)(thunk: =>T): T =
    withOut(new PrintStream(out))(thunk)

  /** Set the default error stream for the duration
   *  of execution of one thunk.
   *  @example {{{
   *  withErr(Console.out) { err.println("This goes to default _out_") }
   *  }}}
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return the results of `thunk`
   *  @see `withErr[T](err:OutputStream)(thunk: =>T)`
   *  @group io-redefinition
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
   *  @group io-redefinition
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
   *  @return the results of `thunk`
   *  @see `withIn[T](in:InputStream)(thunk: =>T)`
   *  @group io-redefinition
   */
  def withIn[T](reader: Reader)(thunk: =>T): T =
    inVar.withValue(new BufferedReader(reader))(thunk)

  /** Sets the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @param in the new input stream.
   *  @param thunk the code to execute with
   *               the new input stream active
   *  @return the results of `thunk`
   *  @see `withIn[T](reader:Reader)(thunk: =>T)`
   *  @group io-redefinition
   */
  def withIn[T](in: InputStream)(thunk: =>T): T =
    withIn(new InputStreamReader(in))(thunk)

  /** Prints an object to `out` using its `toString` method.
   *
   *  @param obj the object to print; may be null.
   *  @group console-output
   */
  def print(obj: Any) {
    out.print(if (null == obj) "null" else obj.toString())
  }

  /** Flushes the output stream. This function is required when partial
   *  output (i.e. output not terminated by a newline character) has
   *  to be made visible on the terminal.
    * @group console-output
   */
  def flush() { out.flush() }

  /** Prints a newline character on the default output.
    * @group console-output
   */
  def println() { out.println() }

  /** Prints out an object to the default output, followed by a newline character.
   *
   *  @param x the object to print.
   *  @group console-output
   */
  def println(x: Any) { out.println(x) }

  /** Prints its arguments as a formatted string to the default output,
   *  based on a string pattern (in a fashion similar to printf in C).
   *
   *  The interpretation of the formatting patterns is described in [[java.util.Formatter]].
   *
   *  @param text the pattern for formatting the arguments.
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException if there was a problem with the format string or arguments
   *  @group console-output
   */
  def printf(text: String, args: Any*) { out.print(text format (args : _*)) }
}

private[scala] abstract class DeprecatedConsole {
  self: Console.type =>

  /** Internal usage only. */
  protected def setOutDirect(out: PrintStream): Unit
  protected def setErrDirect(err: PrintStream): Unit
  protected def setInDirect(in: BufferedReader): Unit

  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readBoolean(): Boolean                     = StdIn.readBoolean()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readByte(): Byte                           = StdIn.readByte()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readChar(): Char                           = StdIn.readChar()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readDouble(): Double                       = StdIn.readDouble()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readFloat(): Float                         = StdIn.readFloat()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readInt(): Int                             = StdIn.readInt()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readLine(): String                         = StdIn.readLine()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readLine(text: String, args: Any*): String = StdIn.readLine(text, args: _*)
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readLong(): Long                           = StdIn.readLong()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readShort(): Short                         = StdIn.readShort()
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readf(format: String): List[Any]           = StdIn.readf(format)
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readf1(format: String): Any                = StdIn.readf1(format)
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readf2(format: String): (Any, Any)         = StdIn.readf2(format)
  @deprecated("use the method in scala.io.StdIn", "2.11.0") def readf3(format: String): (Any, Any, Any)    = StdIn.readf3(format)

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  @deprecated("use withOut", "2.11.0") def setOut(out: PrintStream): Unit = setOutDirect(out)

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  @deprecated("use withOut", "2.11.0") def setOut(out: OutputStream): Unit = setOutDirect(new PrintStream(out))

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  @deprecated("use withErr", "2.11.0") def setErr(err: PrintStream): Unit = setErrDirect(err)

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  @deprecated("use withErr", "2.11.0") def setErr(err: OutputStream): Unit = setErrDirect(new PrintStream(err))

  /** Sets the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  @deprecated("use withIn", "2.11.0") def setIn(reader: Reader): Unit = setInDirect(new BufferedReader(reader))

  /** Sets the default input stream.
   *
   *  @param in the new input stream.
   */
  @deprecated("use withIn", "2.11.0") def setIn(in: InputStream): Unit = setInDirect(new BufferedReader(new InputStreamReader(in)))
}
