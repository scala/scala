/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.io.*;
import java.text.*;


class Console {

    // ANSI colors foreground
    public final static String BLACK      = "\033[30m";
    public final static String RED        = "\033[31m";
    public final static String GREEN      = "\033[32m";
    public final static String YELLOW     = "\033[33m";
    public final static String BLUE       = "\033[34m";
    public final static String MAGENTA    = "\033[35m";
    public final static String CYAN       = "\033[36m";
    public final static String WHITE      = "\033[37m";
    public final static String COLOR38    = "\033[38m";
    public final static String COLOR39    = "\033[39m";

    // ANSI colors background
    public final static String BLACK_B    = "\033[40m";
    public final static String RED_B      = "\033[41m";
    public final static String GREEN_B    = "\033[42m";
    public final static String YELLOW_B   = "\033[43m";
    public final static String BLUE_B     = "\033[44m";
    public final static String MAGENTA_B  = "\033[45m";
    public final static String CYAN_B     = "\033[46m";
    public final static String WHITE_B    = "\033[47m";

    // ANSI styles
    public final static String RESET      = "\033[0m";
    public final static String BOLD       = "\033[1m";
    public final static String UNDERLINED = "\033[4m";
    public final static String BLINK      = "\033[5m";
    public final static String REVERSED   = "\033[7m";
    public final static String INVISIBLE  = "\033[8m";

    private PrintStream out = System.out;
    private BufferedReader in =
    	new BufferedReader(new InputStreamReader(System.in));

    /** Set the default output stream.
     *
     *  @param out the new output stream.
     */
    public void setOut(PrintStream out) {
    	this.out = out;
    }

    /** Set the default input stream.
     *
     *  @param in the new input stream.
     */
    public void setIn(InputStream in) {
    	this.in = new BufferedReader(new InputStreamReader(in));
    }

    /** Set the default input stream.
     *
     *  @param reader specifies the new input stream.
     */
    public void setIn(Reader reader) {
    	this.in = new BufferedReader(reader);
    }

    /** Print an object on the terminal.
     *
     *  @param obj the object to print.
     */
    public void print(Object obj) {
        out.print((obj == null) ? "null" : obj.toString());
    }

    /** Flush the output stream. This function is required when partial
     *  output (i.e. output not terminated by a new line character) has
     *  to be made visible on the terminal.
     */
    public void flush() { out.flush(); }

    /** Print a new line character on the terminal.
     */
    public void println() {
        out.println();
    }

    /** Print out an object followed by a new line character.
     *
     *  @param x the object to print.
     */
    public void println(Object x) {
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
    public void printf(String text, Object[] args) {
        if (text == null)
            out.print("null");
        else
            out.print(MessageFormat.format(text, args));
    }

    /** Read a full line from the terminal.
     *
     *  @return the string read from the terminal.
     */
    public String readLine() throws IOException { return in.readLine(); }

    /** Read a boolean value from the terminal.
     *
     *  @return the boolean value read from the terminal.
     */
    public boolean readBoolean() throws IOException {
        String value = in.readLine().toLowerCase();
        return "true".equals(value) || "t".equals(value)
            || "yes".equals(value) || "y".equals(value);
    }

    /** Read a byte value from the terminal.
     */
    public byte readByte() throws IOException {
        return java.lang.Byte.decode(in.readLine()).byteValue();
    }

    /** Read a short value from the terminal.
     */
    public short readShort() throws IOException {
        return java.lang.Short.decode(in.readLine()).shortValue();
    }

    /** Read a char value from the terminal.
     */
    public char readChar() throws IOException {
        return in.readLine().charAt(0);
    }

    /** Read an int value from the terminal.
     */
    public int readInt() throws IOException {
        return java.lang.Integer.decode(in.readLine()).intValue();
    }

    /** Read a float value from the terminal.
     */
    public float readFloat() throws IOException { return java.lang.Float.parseFloat(in.readLine()); }

    /** Read a double value from the terminal.
     */
    public double readDouble() throws IOException {
        return java.lang.Double.parseDouble(in.readLine());
    }

}
