/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import java.io.{BufferedReader, InputStream, InputStreamReader,
                IOException, OutputStream, PrintStream, Reader}
import java.text.MessageFormat
import scala.util.DynamicVariable


/** Implements functionality for
 *  printing Scala values on the terminal as well as reading specific values.
 *  Also defines constants for marking up text on ANSI terminals.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 */
object Console {

  /** Foreground color for ANSI black */
  final val BLACK      = "\033[30m"
  /** Foreground color for ANSI red */
  final val RED        = "\033[31m"
  /** Foreground color for ANSI green */
  final val GREEN      = "\033[32m"
  /** Foreground color for ANSI yellow */
  final val YELLOW     = "\033[33m"
  /** Foreground color for ANSI blue */
  final val BLUE       = "\033[34m"
  /** Foreground color for ANSI magenta */
  final val MAGENTA    = "\033[35m"
  /** Foreground color for ANSI cyan */
  final val CYAN       = "\033[36m"
  /** Foreground color for ANSI white */
  final val WHITE      = "\033[37m"

  /** Background color for ANSI black */
  final val BLACK_B    = "\033[40m"
  /** Background color for ANSI red */
  final val RED_B      = "\033[41m"
  /** Background color for ANSI green */
  final val GREEN_B    = "\033[42m"
  /** Background color for ANSI yellow */
  final val YELLOW_B   = "\033[43m"
  /** Background color for ANSI blue */
  final val BLUE_B     = "\033[44m"
  /** Background color for ANSI magenta */
  final val MAGENTA_B  = "\033[45m"
  /** Background color for ANSI cyan */
  final val CYAN_B     = "\033[46m"
  /** Background color for ANSI white */
  final val WHITE_B    = "\033[47m"

  /** Reset ANSI styles */
  final val RESET      = "\033[0m"
  /** ANSI bold */
  final val BOLD       = "\033[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\033[4m"
  /** ANSI blink */
  final val BLINK      = "\033[5m"
  /** ANSI reversed */
  final val REVERSED   = "\033[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\033[8m"

  private val outVar = new DynamicVariable[PrintStream](java.lang.System.out)
  private val errVar = new DynamicVariable[PrintStream](java.lang.System.err)
  private val inVar = new DynamicVariable[BufferedReader](
    new BufferedReader(new InputStreamReader(java.lang.System.in)))

  /** The default output, can be overridden by `setOut` */
  def out = outVar.value
  /** The default error, can be overridden by `setErr` */
  def err = errVar.value
  /** The default input, can be overridden by `setIn` */
  def in = inVar.value

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: PrintStream) { outVar.value = out }

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

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: OutputStream): Unit =
    setOut(new PrintStream(out))

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


  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  def setErr(err: PrintStream) { errVar.value = err }

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

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  def setErr(err: OutputStream): Unit =
    setErr(new PrintStream(err))

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


  /** Sets the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  def setIn(reader: Reader) {
    inVar.value = new BufferedReader(reader)
  }

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

  /** Sets the default input stream.
   *
   *  @param in the new input stream.
   */
  def setIn(in: InputStream) {
    setIn(new InputStreamReader(in))
  }

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

  /** Read a full line from the default input.  Returns `null` if the end of the
   * input stream has been reached.
   *
   * @return the string read from the terminal or null if the end of stream was reached.
   */
  def readLine(): String = in.readLine()

  /** Print formatted text to the default output and read a full line from the default input.
   *  Returns `null` if the end of the input stream has been reached.
   *
   *  @param text the format of the text to print out, as in `printf`.
   *  @param args the parameters used to instantiate the format, as in `printf`.
   *  @return the string read from the default input
   */
  def readLine(text: String, args: Any*): String = {
    printf(text, args: _*)
    readLine()
  }

  /** Reads a boolean value from an entire line of the default input.
   *  Has a fairly liberal interpretation of the input.
   *
   *  @return the boolean value read, or false if it couldn't be converted to a boolean
   *  @throws java.io.EOFException if the end of the input stream has been reached.
   */
  def readBoolean(): Boolean = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toLowerCase() match {
        case "true" => true
        case "t" => true
        case "yes" => true
        case "y" => true
        case _ => false
      }
  }

  /** Reads a byte value from an entire line of the default input.
   *
   *  @return the Byte that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Byte
   */
  def readByte(): Byte = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toByte
  }

  /** Reads a short value from an entire line of the default input.
   *
   *  @return the short that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Short
   */
  def readShort(): Short = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toShort
  }

  /** Reads a char value from an entire line of the default input.
   *
   *  @return the Char that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.StringIndexOutOfBoundsException if the line read from default input was empty
   */
  def readChar(): Char = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s charAt 0
  }

  /** Reads an int value from an entire line of the default input.
   *
   *  @return the Int that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to an Int
   */
  def readInt(): Int = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toInt
  }

  /** Reads an long value from an entire line of the default input.
   *
   *  @return the Long that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Long
   */
  def readLong(): Long = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toLong
  }

  /** Reads a float value from an entire line of the default input.
   *  @return the Float that was read.
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Float
   *
   */
  def readFloat(): Float = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toFloat
  }

  /** Reads a double value from an entire line of the default input.
   *
   *  @return the Double that was read.
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Float
   */
  def readDouble(): Double = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toDouble
  }

  /** Reads in some structured input (from the default input), specified by
   *  a format specifier. See class `java.text.MessageFormat` for details of
   *  the format specification.
   *
   *  @param format the format of the input.
   *  @return a list of all extracted values.
   *  @throws java.io.EOFException if the end of the input stream has been
   *          reached.
   */
  def readf(format: String): List[Any] = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      textComponents(new MessageFormat(format).parse(s))
  }

  /** Reads in some structured input (from the default input), specified by
   *  a format specifier, returning only the first value extracted, according
   *  to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return The first value that was extracted from the input
   */
  def readf1(format: String): Any = readf(format).head

  /** Reads in some structured input (from the default input), specified
   *  by a format specifier, returning only the first two values extracted,
   *  according to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return A [[scala.Tuple2]] containing the first two values extracted
   */
  def readf2(format: String): (Any, Any) = {
    val res = readf(format)
    (res.head, res.tail.head)
  }

  /** Reads in some structured input (from the default input), specified
   *  by a format specifier, returning only the first three values extracted,
   *  according to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return A [[scala.Tuple3]] containing the first three values extracted
   */
  def readf3(format: String): (Any, Any, Any) = {
    val res = readf(format)
    (res.head, res.tail.head, res.tail.tail.head)
  }

  private def textComponents(a: Array[AnyRef]): List[Any] = {
    var i: Int = a.length - 1
    var res: List[Any] = Nil
    while (i >= 0) {
      res = (a(i) match {
        case x: java.lang.Boolean   => x.booleanValue()
        case x: java.lang.Byte      => x.byteValue()
        case x: java.lang.Short     => x.shortValue()
        case x: java.lang.Character => x.charValue()
        case x: java.lang.Integer   => x.intValue()
        case x: java.lang.Long      => x.longValue()
        case x: java.lang.Float     => x.floatValue()
        case x: java.lang.Double    => x.doubleValue()
        case x => x
      }) :: res;
      i -= 1
    }
    res
  }
}
