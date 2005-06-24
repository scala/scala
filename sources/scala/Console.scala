/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import Predef._;

/** The <code>Console</code> object implements functionality for
 *  printing Scala values on the terminal. There are also functions
 *  for reading specific values. <code>Console</code> also defines
 *  constants for marking up text on ANSI terminals.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 */
object Console {
    import java.io._;
    import java.text._;

    // ANSI colors foreground
    final val BLACK      = "\033[30m";
    final val RED        = "\033[31m";
    final val GREEN      = "\033[32m";
    final val YELLOW     = "\033[33m";
    final val BLUE       = "\033[34m";
    final val MAGENTA    = "\033[35m";
    final val CYAN       = "\033[36m";
    final val WHITE      = "\033[37m";

    // ANSI colors background
    final val BLACK_B    = "\033[40m";
    final val RED_B      = "\033[41m";
    final val GREEN_B    = "\033[42m";
    final val YELLOW_B   = "\033[43m";
    final val BLUE_B     = "\033[44m";
    final val MAGENTA_B  = "\033[45m";
    final val CYAN_B     = "\033[46m";
    final val WHITE_B    = "\033[47m";

    // ANSI styles
    final val RESET      = "\033[0m";
    final val BOLD       = "\033[1m";
    final val UNDERLINED = "\033[4m";
    final val BLINK      = "\033[5m";
    final val REVERSED   = "\033[7m";
    final val INVISIBLE  = "\033[8m";

    private var out: PrintStream = java.lang.System.out;
    private var in: BufferedReader =
        new BufferedReader(new InputStreamReader(java.lang.System.in));

    /** Set the default output stream.
     *
     *  @param out the new output stream.
     */
    def setOut(out: PrintStream): Unit = {
        this.out = out;
    }

    /** Set the default input stream.
     *
     *  @param in the new input stream.
     */
    def setIn(in: InputStream): Unit = {
        this.in = new BufferedReader(new InputStreamReader(in));
    }

    /** Set the default input stream.
     *
     *  @param reader specifies the new input stream.
     */
    def setIn(reader: Reader): Unit = {
        this.in = new BufferedReader(reader);
    }

    /** Print an object on the terminal.
     *
     *  @param obj the object to print.
     */
    def print(obj: Any): Unit = {
        if (obj == null)
            out.print("null");
        else
            out.print(obj.toString());
    }

    /** Flush the output stream. This function is required when partial
     *  output (i.e. output not terminated by a new line character) has
     *  to be made visible on the terminal.
     */
    def flush: Unit = out.flush();

    /** Print a new line character on the terminal.
     */
    def println: Unit = {
        out.println();
    }

    /** Print out an object followed by a new line character.
     *
     *  @param x the object to print.
     */
    def println(x: Any): Unit = {
        out.println(x);
    }

    /** Format and print out some text (in a fashion similar to printf in C).
     *  The format of the text to print is specified by the parameter
     *  <code>text</code>. The arguments that are inserted into specific
     *  locations in <code>text</code> are provided with parameter
     *  <code>args</code>. See class <code>java.text.MessageFormat</code>
     *  for a full specification of the format syntax.
     *
     *  @param text the format of the text to print out.
     *  @param args the parameters used to instantiate the format.
     */
    def printf(text: String)(args: Any*): Unit = {
      // todo: Uncurry
        if (text == null)
            out.print("null");
        else
            out.print(MessageFormat.format(text, textParams(args)));
    }

    /** Read a full line from the terminal.
     *
     *  @return the string read from the terminal.
     */
    def readLine: String = in.readLine();

    /** Read a boolean value from the terminal.
     *
     *  @return the boolean value read from the terminal.
     */
    def readBoolean: Boolean = in.readLine().toLowerCase() match {
        case "true" => true
        case "t" => true
        case "yes" => true
        case "y" => true
        case _ => false
    }

    /** Read a byte value from the terminal.
     */
    def readByte: Byte = java.lang.Byte.decode(in.readLine()).byteValue();

    /** Read a short value from the terminal.
     */
    def readShort: Short = java.lang.Short.decode(in.readLine()).shortValue();

    /** Read a char value from the terminal.
     */
    def readChar: Char = in.readLine().charAt(0);

    /** Read an int value from the terminal.
     */
    def readInt: Int = java.lang.Integer.decode(in.readLine()).intValue();

    /** Read a float value from the terminal.
     */
    def readFloat: Float =
      scala.runtime.compat.Platform.parseFloat(in.readLine());

    /** Read a double value from the terminal.
     */
    def readDouble: Double =
      scala.runtime.compat.Platform.parseDouble(in.readLine());

    /** Read in some structured input, specified by a format specifier.
     *  See class <code>java.text.MessageFormat</code> for details of
     *  the format specification.
     *
     *  @param format the format of the input.
     *  @return a list of all extracted values.
     */
    def readf(format: String): List[Any] =
        textComponents(new MessageFormat(format).parse(in.readLine()));

    /** Read in some structured input, specified by a format specifier.
     *  Opposed to <code>readf</code>, this function only returns the
     *  first value extracted from the input according to the format
     *  specification.
     */
    def readf1(format: String): Any = readf(format).head;

    /** Read in some structured input, specified by a format specifier.
     *  Opposed to <code>readf</code>, this function only returns the
     *  first two values extracted from the input according to the format
     *  specification.
     */
    def readf2(format: String): Pair[Any, Any] = {
        val res = readf(format);
        Pair(res.head, res.tail.head)
    }

    /** Read in some structured input, specified by a format specifier.
     *  Opposed to <code>readf</code>, this function only returns the
     *  first three values extracted from the input according to the format
     *  specification.
     */
    def readf3(format: String): Triple[Any, Any, Any] = {
        val res = readf(format);
        Triple(res.head, res.tail.head, res.tail.tail.head)
    }

    private def textComponents(a: Array[AnyRef]): List[Any] = {
        var i: Int = a.length - 1;
        var res: List[Any] = Nil;
        while (i >= 0) {
            res = (a(i) match {
                case x: java.lang.Boolean => x.booleanValue()
                case x: java.lang.Byte => x.byteValue()
                case x: java.lang.Short => x.shortValue()
                case x: java.lang.Character => x.charValue()
                case x: java.lang.Integer => x.intValue()
                case x: java.lang.Long => x.longValue()
                case x: java.lang.Float => x.floatValue()
                case x: java.lang.Double => x.doubleValue()
                case x => x
            }) :: res;
            i = i - 1;
        }
        res
    }

    private def textParams(s: Seq[Any]): Array[AnyRef] = {
        val res = new Array[AnyRef](s.length);
        var i: Int = 0;
        val iter = s.elements;
        while (iter.hasNext) {
            res(i) = iter.next match {
                case x: Boolean => new java.lang.Boolean(x);
                case x: Byte => new java.lang.Byte(x);
                case x: Short => new java.lang.Short(x);
                case x: Char => new java.lang.Character(x);
                case x: Int => new java.lang.Integer(x);
                case x: Long => new java.lang.Long(x);
                case x: Float => new java.lang.Float(x);
                case x: Double => new java.lang.Double(x);
                case x: Unit => "()";
                case x: AnyRef => x;
            }
            i = i + 1;
        }
        res
    }
}
