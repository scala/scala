/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import java.io.{BufferedReader, InputStream, InputStreamReader,
                IOException, OutputStream, PrintStream, Reader}
import java.text.MessageFormat

import scala.util.DynamicVariable
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

  private val outVar = new DynamicVariable[PrintStream](java.lang.System.out)
  private val errVar = new DynamicVariable[PrintStream](java.lang.System.err)
  private val inVar = new DynamicVariable[BufferedReader](
    new BufferedReader(new InputStreamReader(java.lang.System.in)))

  def out = outVar.value
  def err = errVar.value
  def in = inVar.value

  /** Set the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: PrintStream) { outVar.value = out }

  /** Set the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return ...
   */
  def withOut[T](out: PrintStream)(thunk: =>T): T =
    outVar.withValue(out)(thunk)

  /** Set the default output stream.
   *
   *  @param@ out the new output stream.
   */
  def setOut(out: OutputStream): Unit =
    setOut(new PrintStream(out))

  /** Set the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return ...
   */
  def withOut[T](out: OutputStream)(thunk: =>T): T =
    withOut(new PrintStream(out))(thunk)


  /** Set the default error stream.
   *
   *  @param err the new error stream.
   */
  def setErr(err: PrintStream) { errVar.value = err }

  /** Set the default error stream for the duration
   *  of execution of one thunk.
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return ...
   */
  def withErr[T](err: PrintStream)(thunk: =>T): T =
    errVar.withValue(err)(thunk)

  /** Set the default error stream.
   *
   *  @param err the new error stream.
   */
  def setErr(err: OutputStream): Unit =
    setErr(new PrintStream(err))

  /** Set the default error stream for the duration
   *  of execution of one thunk.
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return ...
   */
  def withErr[T](err: OutputStream)(thunk: =>T): T =
    withErr(new PrintStream(err))(thunk)


  /** Set the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  def setIn(reader: Reader) {
    inVar.value = new BufferedReader(reader)
  }

  /** Set the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @param in the new input stream.
   *  @param thunk the code to execute with
   *               the new input stream active
   */
  def withIn[T](reader: Reader)(thunk: =>T): T =
    inVar.withValue(new BufferedReader(reader))(thunk)


  /** Set the default input stream.
   *
   *  @param in the new input stream.
   */
  def setIn(in: InputStream) {
    setIn(new InputStreamReader(in))
  }

  /** Set the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @param in the new input stream.
   *  @param thunk the code to execute with
   *               the new input stream active
   */
  def withIn[T](in: InputStream)(thunk: =>T): T =
    withIn(new InputStreamReader(in))(thunk)

  /** Print an object on the terminal.
   *
   *  @param obj the object to print.
   */
  def print(obj: Any) {
    out.print(if (null == obj) "null" else obj.toString())
  }

  /** Flush the output stream. This function is required when partial
   *  output (i.e. output not terminated by a new line character) has
   *  to be made visible on the terminal.
   */
  def flush() { out.flush() }

  /** Print a new line character on the terminal.
   */
  def println() { out.println() }

  /** Print out an object followed by a new line character.
   *
   *  @param x the object to print.
   */
  def println(x: Any) { out.println(x) }

  /** <p>
   *    Prints its arguments as a formatted string, based on a string
   *    pattern (in a fashion similar to printf in C).
   *  </p>
   *  <p>
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    <code>java.util.Formatter</code></a>.
   *  </p>
   *
   *  @param text the pattern for formatting the arguments.
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def printf(text: String, args: Any*) { out.print(text format (args : _*)) }

  /**
   *  @see <a href="#printf(java.lang.String,scala.Any*)"
   *       target="contentFrame">Console.printf</a>.
   */
  @deprecated("For console output, use <code>Console.printf</code>. For <code>String</code>\n"+
              "formatting, <code>StringOps</code>'s <code>format</code> method.")
  def format(text: String, args: Any*) {
    if (text eq null) out.printf("null")
    else out.print(text format (args : _*))
  }

  /** Read a full line from the terminal.  Returns <code>null</code> if the end of the
   * input stream has been reached.
   *
   * @return the string read from the terminal.
   */
  def readLine(): String = in.readLine()

  /** Print a formatted text and read a full line from the terminal.
   * Returns null if the end of the input stream has been reached.
   *
   *  @param text the format of the text to print out.
   *  @param args the parameters used to instantiate the format.
   *  @return the string read from the terminal.
   */
  def readLine(text: String, args: Any*): String = {
    printf(text, args: _*)
    readLine()
  }

  /** Read a boolean value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @return the boolean value read from the terminal.
   *  @throws java.io.EOFException
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

  /** Read a byte value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readByte(): Byte = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toByte
  }

  /** Read a short value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readShort(): Short = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toShort
  }

  /** Read a char value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readChar(): Char = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s charAt 0
  }

  /** Read an int value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readInt(): Int = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toInt
  }

  /** Read an int value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readLong(): Long = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toLong
  }

  /** Read a float value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readFloat(): Float = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toFloat
  }

  /** Read a double value from the terminal.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @throws java.io.EOFException
   */
  def readDouble(): Double = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toDouble
  }

  /** Read in some structured input, specified by a format specifier.
   *  See class <code>java.text.MessageFormat</code> for details of
   *  the format specification.
   *  Throws <code>EOFException</code> if the end of the
   *  input stream has been reached.
   *
   *  @param format the format of the input.
   *  @return a list of all extracted values.
   *  @throws java.io.EOFException
   */
  def readf(format: String): List[Any] = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      textComponents(new MessageFormat(format).parse(s))
  }

  /** Read in some structured input, specified by a format specifier.
   *  Opposed to <code>readf</code>, this function only returns the
   *  first value extracted from the input according to the format
   *  specification.
   *
   *  @param format ...
   *  @return ...
   */
  def readf1(format: String): Any = readf(format).head

  /** Read in some structured input, specified by a format specifier.
   *  Opposed to <code>readf</code>, this function only returns the
   *  first two values extracted from the input according to the format
   *  specification.
   *
   *  @param format ...
   *  @return ...
   */
  def readf2(format: String): (Any, Any) = {
    val res = readf(format)
    (res.head, res.tail.head)
  }

  /** Read in some structured input, specified by a format specifier.
   *  Opposed to <code>readf</code>, this function only returns the
   *  first three values extracted from the input according to the format
   *  specification.
   *
   *  @param format ...
   *  @return ...
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

  private def textParams(s: Seq[Any]): Array[AnyRef] = {
    val res = new Array[AnyRef](s.length)
    var i: Int = 0
    val iter = s.iterator
    while (iter.hasNext) {
      res(i) = iter.next match {
        case x: Boolean => java.lang.Boolean.valueOf(x)
        case x: Byte    => java.lang.Byte.valueOf(x)
        case x: Short   => java.lang.Short.valueOf(x)
        case x: Char    => java.lang.Character.valueOf(x)
        case x: Int     => java.lang.Integer.valueOf(x)
        case x: Long    => java.lang.Long.valueOf(x)
        case x: Float   => java.lang.Float.valueOf(x)
        case x: Double  => java.lang.Double.valueOf(x)
        case x: Unit    => "()"
        case x: AnyRef  => x
      }
      i += 1
    }
    res
  }
}
