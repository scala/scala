/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import scala.util.Fluid
import System.IO.{TextReader,TextWriter}
import compat.Platform
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

  private val outFluid = new Fluid[TextWriter](System.Console.Out)
  private val inFluid  = new Fluid[TextReader](System.Console.In)

  def out = outFluid.value
  def in  = inFluid.value

  /** Set the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: TextWriter): Unit = outFluid.value = out

  /** Set the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   */
  def withOut[T](out: TextWriter)(thunk: =>T): T =
    outFluid.withValue(out)(thunk)

  /** Set the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  def setIn(reader: TextReader): Unit = {
    inFluid.value = reader
  }

  /** Set the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @param in the new input stream.
   *  @param thunk the code to execute with
   *               the new input stream active
   */
  def withIn[T](reader: TextReader)(thunk: =>T): T =
    inFluid.withValue(reader)(thunk)


  /** Print an object on the terminal.
   *
   *  @param obj the object to print.
   */
  def print(obj: Any): Unit = {
    out.Write(if (null == obj) "null" else obj.toString());
  }

  /** Flush the output stream. This function is required when partial
   *  output (i.e. output not terminated by a new line character) has
   *  to be made visible on the terminal.
   */
  def flush: Unit = out.Flush()

  /** Print a new line character on the terminal.
   */
  def println: Unit = out.WriteLine()

  /** Print out an object followed by a new line character.
   *
   *  @param x the object to print.
   */
  def println(x: Any): Unit = out.WriteLine(x)

  /** <p>
   *    Format and print out some text (in a fashion similar to printf in C or
   *    <code>printf</code> in Java 6).
   *  </p>
   *  <p>
   *    The format of the text to print is specified by the parameter
   *    <code>text</code>. The arguments that are inserted into specific
   *    locations in <code>text</code> are provided with parameter
   *    <code>args</code>. See class <a href="" target="contentFrame"
   *    class="java_text_MessageFormat"><code>java.text.MessageFormat</code></a>
   *    for a full specification of the <a href="#syntax" target="contentFrame"
   *    class="java_util_Formatter">format syntax</a>.
   *  </p>
   *
   *  @param text the format of the text to print out.
   *  @param args the parameters used to instantiate the format.
   *  @throws java.lang.IllegalArgumentException
   */
  def printf(text: String, args: Any*): Unit = format(text, args: _*)

  /**
   *  @see <a href="#printf(java.lang.String,scala.Any*)"
   *       target="contentFrame">Console.printf</a>.
   */
  def format(text: String, args: Any*): Unit =
    if (text eq null) out.Write("null")
    else out.Write(text, args.toArray)

  /** Read a full line from the terminal.
   *
   *  @return the string read from the terminal.
   */
  def readLine: String = in.ReadLine();

  /** Print a formatted text and read a full line from the terminal
   *
   *  @param text the format of the text to print out.
   *  @param args the parameters used to instantiate the format.
   *  @return the string read from the terminal.
   */
  def readLine(text: String, args: Any*): String = {
    format(text, args: _*)
    readLine
  }


  /** Read a boolean value from the terminal.
   *
   *  @return the boolean value read from the terminal.
   */
  def readBoolean: Boolean = readLine.toLowerCase() match {
    case "true" => true
    case "t" => true
    case "yes" => true
    case "y" => true
    case _ => false
  }

  /** Read a byte value from the terminal.
   */
  def readByte: Byte = readLine.toByte

  /** Read a short value from the terminal.
   */
  def readShort: Short = readLine.toShort

  /** Read a char value from the terminal.
   */
  def readChar: Char = readLine charAt 0

  /** Read an int value from the terminal.
   */
  def readInt: Int = readLine.toInt

  /** Read a float value from the terminal.
   */
  def readFloat: Float = readLine.toFloat

  /** Read a double value from the terminal.
   */
  def readDouble: Double = readLine.toDouble

//   /** Read in some structured input, specified by a format specifier.
//    *  See class <code>java.text.MessageFormat</code> for details of
//    *  the format specification.
//    *
//    *  @param format the format of the input.
//    *  @return a list of all extracted values.
//    */
//   def readf(format: String): List[Any] =
//     textComponents(new MessageFormat(format).parse(in.readLine()))

//   /** Read in some structured input, specified by a format specifier.
//    *  Opposed to <code>readf</code>, this function only returns the
//    *  first value extracted from the input according to the format
//    *  specification.
//    *
//    *  @param format ...
//    *  @return ...
//    */
//   def readf1(format: String): Any = readf(format).head

//   /** Read in some structured input, specified by a format specifier.
//    *  Opposed to <code>readf</code>, this function only returns the
//    *  first two values extracted from the input according to the format
//    *  specification.
//    *
//    *  @param format ...
//    *  @return ...
//    */
//   def readf2(format: String): Pair[Any, Any] = {
//     val res = readf(format)
//     Pair(res.head, res.tail.head)
//   }

//   /** Read in some structured input, specified by a format specifier.
//    *  Opposed to <code>readf</code>, this function only returns the
//    *  first three values extracted from the input according to the format
//    *  specification.
//    *
//    *  @param format ...
//    *  @return ...
//    */
//   def readf3(format: String): Triple[Any, Any, Any] = {
//     val res = readf(format)
//     Triple(res.head, res.tail.head, res.tail.tail.head)
//   }

//   private def textComponents(a: Array[AnyRef]): List[Any] = {
//     var i: Int = a.length - 1
//     var res: List[Any] = Nil
//     while (i >= 0) {
//       res = (a(i) match {
//         case x: java.lang.Boolean => x.booleanValue()
//         case x: java.lang.Byte => x.byteValue()
//         case x: java.lang.Short => x.shortValue()
//         case x: java.lang.Character => x.charValue()
//         case x: java.lang.Integer => x.intValue()
//         case x: java.lang.Long => x.longValue()
//         case x: java.lang.Float => x.floatValue()
//         case x: java.lang.Double => x.doubleValue()
//         case x => x
//       }) :: res;
//       i = i - 1
//     }
//     res
//   }

//   private def textParams(s: Seq[Any]): Array[AnyRef] = {
//     val res = new Array[AnyRef](s.length);
//     var i: Int = 0;
//     val iter = s.elements;
//     while (iter.hasNext) {
//       res(i) = iter.next match {
//         case x: Boolean => new java.lang.Boolean(x)
//         case x: Byte => new java.lang.Byte(x)
//         case x: Short => new java.lang.Short(x)
//         case x: Char => new java.lang.Character(x)
//         case x: Int => new java.lang.Integer(x)
//         case x: Long => new java.lang.Long(x)
//         case x: Float => new java.lang.Float(x)
//         case x: Double => new java.lang.Double(x)
//         case x: Unit => "()"
//         case x: AnyRef => x
//       }
//       i = i + 1
//     }
//     res
//   }

}
