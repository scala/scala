/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import java.io.{BufferedReader, InputStream, InputStreamReader,
                OutputStream, PrintStream, Reader}
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
  private val inVar = new DynamicVariable[BufferedReader](
    new BufferedReader(new InputStreamReader(java.lang.System.in)))

  def out = outVar.value
  def in = inVar.value

  val err = java.lang.System.err

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


  /** Set the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  def setIn(reader: Reader): Unit = {
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
  def setIn(in: InputStream): Unit =
    setIn(new InputStreamReader(in))

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
  def print(obj: Any): Unit =
    out.print(if (null == obj) "null" else obj.toString())

  /** Flush the output stream. This function is required when partial
   *  output (i.e. output not terminated by a new line character) has
   *  to be made visible on the terminal.
   */
  def flush() { out.flush() }

  /** Print a new line character on the terminal.
   */
  def println(): Unit = out.println()

  /** Print out an object followed by a new line character.
   *
   *  @param x the object to print.
   */
  def println(x: Any): Unit = out.println(x)

  /** <p>
   *    Prints its arguments as a formatted string, based on a string
   *    pattern (in a fashion similar to printf in C).
   *  </p>
   *  <p>
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/text/MessageFormat">
   *    <code>java.text.MessageFormat</code></a>.
   *  </p>
   *
   *  @param text the pattern for formatting the arguments.
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def printf(text: String, args: Any*) { format(text, args: _*) }

  /**
   *  @see <a href="#printf(java.lang.String,scala.Any*)"
   *       target="contentFrame">Console.printf</a>.
   */
  def format(text: String, args: Any*): Unit =
    out.print(
      if (text eq null) "null"
      else MessageFormat.format(text, textParams(args))
    )

  /** Read a full line from the terminal.
   *
   *  @return the string read from the terminal.
   */
  def readLine(): String = in.readLine()

  /** Print a formatted text and read a full line from the terminal
   *
   *  @param text the format of the text to print out.
   *  @param args the parameters used to instantiate the format.
   *  @return the string read from the terminal.
   */
  def readLine(text: String, args: Any*): String = {
    format(text, args: _*)
    readLine()
  }


  /** Read a boolean value from the terminal.
   *
   *  @return the boolean value read from the terminal.
   */
  def readBoolean(): Boolean = readLine().toLowerCase() match {
    case "true" => true
    case "t" => true
    case "yes" => true
    case "y" => true
    case _ => false
  }

  /** Read a byte value from the terminal.
   */
  def readByte(): Byte = readLine().toByte

  /** Read a short value from the terminal.
   */
  def readShort(): Short = readLine().toShort

  /** Read a char value from the terminal.
   */
  def readChar(): Char = readLine() charAt 0

  /** Read an int value from the terminal.
   */
  def readInt(): Int = readLine().toInt

  /** Read an int value from the terminal.
   */
  def readLong(): Long = readLine().toLong

  /** Read a float value from the terminal.
   */
  def readFloat(): Float = readLine().toFloat

  /** Read a double value from the terminal.
   */
  def readDouble(): Double = readLine().toDouble

  /** Read in some structured input, specified by a format specifier.
   *  See class <code>java.text.MessageFormat</code> for details of
   *  the format specification.
   *
   *  @param format the format of the input.
   *  @return a list of all extracted values.
   */
  def readf(format: String): List[Any] =
    textComponents(new MessageFormat(format).parse(readLine()))

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
    val iter = s.elements
    while (iter.hasNext) {
      res(i) = iter.next match {
        case x: Boolean => java.lang.Boolean.valueOf(x)
        /** Should use java.lang.Byte.valueOf(Byte), but only available
         * in Java 1.5 and above. */
        case x: Byte    => new java.lang.Byte(x)
        /** Should use java.lang.Short.valueOf(Short), but only available
         * in Java 1.5 and above. */
        case x: Short   => new java.lang.Short(x)
        /** Should use java.lang.Character.valueOf(Char), but only available
         * in Java 1.5 and above. */
        case x: Char    => new java.lang.Character(x)
        /** Should use java.lang.Integer.valueOf(Int), but only available
         * in Java 1.5 and above. */
        case x: Int     => new java.lang.Integer(x)
        /** Should use java.lang.Long.valueOf(Long), but only available
         * in Java 1.5 and above. */
        case x: Long    => new java.lang.Long(x)
        case x: Float   => new java.lang.Float(x)
        case x: Double  => new java.lang.Double(x)
        case x: Unit    => "()"
        case x: AnyRef  => x
      }
      i += 1
    }
    res
  }
}
