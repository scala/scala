/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

import java.io.*;
import java.util.Hashtable;
import scalac.util.AbstractFile;
import scalac.util.Name;
import scalac.util.Position;


/** This class represents a single scala source file. It provides
 *  functionality to read the file and to output error messages on
 *  a given line and column. Error messages are logged to
 *  decouple the time where an error message is issued from the
 *  time where the error messages are displayed.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class Sourcefile {

    /** the id management
     */
    public static int numIds = 1;
    public static String[] files = new String[]{"console", null, null, null};
    public static Sourcefile[] sources = new Sourcefile[]{null, null, null, null};
    public int id;

    /** the filename
     */
    public final boolean    console;
    protected String        filename;
    public String           shortname;
    public String           pathname;

    /** the encoding of the file
     */
    protected String        encoding;

    /** a log of all errors generated so far; used to avoid printing an
     *  error message more than once
     */
    protected Hashtable     recorded = new Hashtable();

    /** the buffer containing the file that is currently translated
     */
    protected byte[]        buf = null;

    /** the last error position
     */
    protected int    lastLine = 0;
    protected int    lastPos = 0;
    protected int    lineEnd = 0;
    protected int    newPos = 0;

    /** constants used for source parsing
     */
    final static byte       LF = 0xA;
    final static byte       FF = 0xC;
    final static byte       CR = 0xD;
    final static byte       SU = 0x1A;

    /** set col to NO_COLUMN, if the printLine method should not mark
     *  the column
     */
    final static int        NO_COLUMN = -1;

    /** number of lines and bytes (not used internally)
     */
    public int              lines;  // set externally
    public int              bytes;

    /** prompt after error?
     */
    public boolean prompt;

    /** constructors
     */
    public Sourcefile(String filename, boolean console) throws IOException, FileNotFoundException {
        this.console = console;
        if (filename == null) {
            this.filename = "(sourcefile not available)";
            this.shortname = "?";
            this.pathname = "?";
            buf = new byte[]{SU};
        } else {
            File f = new File(filename);
            this.filename = filename;
            this.shortname = f.getName();
            this.pathname = f.getAbsoluteFile().getParentFile().
                              getCanonicalPath();
            fillBuffer(new FileInputStream(f));
        }
        if (numIds == files.length) {
            String[] newfiles = new String[numIds * 2];
            System.arraycopy(files, 0, newfiles, 0, numIds);
            files = newfiles;
            Sourcefile[] newsources = new Sourcefile[numIds * 2];
            System.arraycopy(sources, 0, newsources, 0, numIds);
            sources = newsources;
        }
        sources[numIds] = this;
        files[id = numIds++] = shortname;
    }

    public Sourcefile(AbstractFile abs) throws IOException, FileNotFoundException {
        this.console = false;
        if (filename == null) {
            this.filename = "(sourcefile not available)";
            this.shortname = "?";
            this.pathname = "?";
            buf = new byte[]{SU};
        } else {
            this.filename = abs.getPath();
            this.shortname = abs.getName();
            this.pathname = abs.getPath();
            fillBuffer(abs.getInputStream());
        }
        if (numIds == files.length) {
            String[] newfiles = new String[numIds * 2];
            System.arraycopy(files, 0, newfiles, 0, numIds);
            files = newfiles;
            Sourcefile[] newsources = new Sourcefile[numIds * 2];
            System.arraycopy(sources, 0, newsources, 0, numIds);
            sources = newsources;
        }
        sources[numIds] = this;
        files[id = numIds++] = shortname;
    }

    public Sourcefile(byte[] input, boolean console) {
        this.console = console;
        if (input == null) {
            this.filename = "(sourcefile not available)";
            this.shortname = "?";
            this.pathname = "?";
            buf = new byte[]{SU};
        } else {
            this.filename = "console";
            this.shortname = "console";
            this.pathname = "console";
            buf = new byte[input.length + 2];
            System.arraycopy(input, 0, buf, 0, input.length);
            buf[input.length] = Scanner.LF;
            buf[input.length + 1] = SU;
        }
        sources[0] = this;
        id = 0;
    }

    /** fill the buffer using the InputStream
     */
    private void fillBuffer(InputStream in) throws IOException {
        try {
            buf = new byte[(bytes = in.available()) + 1];
            if (in.read(buf) != (buf.length - 1))
                throw new IOException();
            in.close();
            buf[buf.length - 1] = SU;
        } catch (IOException e) {
            throw new IOException("cannot read '" + filename + "'");
        }
    }

    /** return filename as a string
     */
    public String toString() {
        return filename;
    }

    /** return filename as a name
     */
    public Name getName() {
        return Name.fromString(filename);
    }

    /** return the shortname without the suffix
     */
    public String getShortnameWithoutSuffix() {
        int idx = shortname.lastIndexOf('.');
        if (idx < 0)
            return shortname;
        else
            return shortname.substring(0, idx);
    }

    /** return the source buffer of this file
     */
    public byte[] getBuffer() {
        return buf;
    }

    /** number of logged entries
     */
    public int logged() {
        return recorded.size();
    }

    /** is there already an entry at position 'pos'
     */
    public boolean isLogged(int pos) {
        return (recorded.get(new Integer(pos)) != null);
    }

    /** enter entry into log table
     */
    public void log(int pos, String message) {
        recorded.put(new Integer(pos), message);
    }

    /** set encoding of the file
     */
    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    /** return true if there is an entry for this position,
     *  otherwise return false and enter message into log
     */
    public boolean testAndSetLog(int pos, String message) {
        if (!isLogged(pos)) {
            log(pos, message);
            return false;
        }
        return true;
    }

    /** get error message with line from sourcefile
     */
    public String getMessage(int pos, String message) {
        if (pos == Position.NOPOS)
            return filename + ": " + message;
        else {
            int fileId = Position.file(pos);
            String filename = files[fileId];
            int line = Position.line(pos);
            int col = Position.column(pos);
            String main = filename + ":" + line + ": " + message;
            if ((fileId > 0) &&
                (fileId < numIds) &&
                (sources[fileId] != null))
                return main + '\n' + sources[fileId].getLine(line, col);
            else
                return main;
            //else
            //    System.out.println("(source file not available anymore)");
        }
    }

    /** get source line
     */
    public String getLine(int line, int col) {
        int pos = 0;
        if (lastLine > line)
            lastLine = 0;
        else
            pos = newPos;
        while ((pos < buf.length) && (lastLine < line))
        {
            lastPos = pos;
            while ((pos < buf.length) && (buf[pos] != CR) &&
                    (buf[pos] != LF) && (buf[pos] != FF))
                pos++;
            lineEnd = pos;
            if (pos < buf.length)
                pos++;
            if ((pos < buf.length) && (buf[pos-1] == CR) && (buf[pos] == LF))
                pos++;
            lastLine++;
        }
        newPos = pos;
        try
        {
            String  errline = (encoding != null) ?
                                new String(buf, lastPos, lineEnd - lastPos, encoding) :
                                new String(buf, lastPos, lineEnd - lastPos);
            if (col != NO_COLUMN)
            {
                byte[]  ptr = new byte[col];
                for (int i = col - 2; i >= 0; i--)
                    ptr[i] = (byte)' ';
                ptr[col - 1] = (byte)'^';
                return errline + '\n' + new String(ptr);
            } else
                return errline;
        } catch (UnsupportedEncodingException e) {
            throw new InternalError(e.getMessage());
        }
    }

    /** release all sourcefile objects
     */
    public static void flushSources() {
        for (int i = 0; i < sources.length; i++)
            sources[i] = null;
    }
}
