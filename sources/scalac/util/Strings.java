/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

public abstract class Strings {

    //########################################################################
    // Strings constants

    final public static String[] NONE = new String[0];

    //########################################################################
    // Strings interface

    /** The line separator */
    public static String EOL = System.getProperty("line.separator", "\n");

    /** Returns a string where all tabs have been replaced by white
     * spaces to make the corresponding fields the same width.
     */
    public static String format(List strings) {
        List[] lines = new List[strings.size()];
        List widths = new ArrayList();
        int height = 0;
        for (Iterator iterator = strings.iterator(); iterator.hasNext(); ) {
            String string = (String)iterator.next();
            List line = lines[height++] = new ArrayList();
            for (int last = 0; last < string.length(); ) {
                int next = string.indexOf('\t', last);
                if (next < 0) next = string.length();
                String substring = string.substring(last, next);
                int index = line.size();
                if (index == widths.size()) widths.add(new Integer(0));
                int width = ((Integer)widths.get(index)).intValue();
                widths.set(
                    index, new Integer(Math.max(width, substring.length())));
                line.add(substring);
                last = next + 1;
            }
        }
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < lines.length; i++) {
            List line = lines[i];
            for (int j = 0; true; j++) {
                String string = (String)line.get(j);
                buffer.append(string);
                if (j == line.size() - 1) break;
                int width = ((Integer)widths.get(j)).intValue();
                for (int k = string.length(); k<width; k++) buffer.append(' ');
            }
            buffer.append(EOL);
        }
        return buffer.toString();
    }

    /** Returns the first char of the string or -1 if the string is empty. */
    public static int firstChar(String string) {
        return string.length() == 0 ? -1 : string.charAt(0);
    }

    /** Returns the last char of the string or -1 if the string is empty. */
    public static int lastChar(String string) {
        return string.length() == 0 ? -1 : string.charAt(string.length() - 1);
    }

    /** Returns a copy of the string, with leading whitespace omitted. */
    public static String trimLeading(String string) {
        for (int i = 0; i < string.length(); i++)
            if (string.charAt(i) > ' ') return string.substring(i);
        return "";
    }

    /** Returns a copy of the string, with trailing whitespace omitted. */
    public static String trimTrailing(String string) {
        for (int i = string.length() - 1; i >= 0; i--)
            if (string.charAt(i) > ' ') return string.substring(0, i + 1);
        return "";
    }

    /** Returns the stack trace of the exception */
    public static String stackTrace(Throwable exception) {
        StringWriter buffer = new StringWriter();
        PrintWriter writer = new PrintWriter(buffer);
        exception.printStackTrace(writer);
        writer.close();
        return buffer.toString();
    }

    //########################################################################
}
