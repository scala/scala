/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package ch.epfl.lamp.util;

import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import java.nio.*;
import java.nio.channels.*;
import java.nio.charset.*;

/** This class represents a single source file.
 */
public class SourceFile2 implements AbstractSourceFile {

    //########################################################################
    // Public Constants

    /** Constants used for source parsing */
    public static final char LF = 0x0A;
    public static final char FF = 0x0C;
    public static final char CR = 0x0D;
    public static final char SU = 0x1A;

    //########################################################################
    // Private Fields

    /** The name of this source file */
    private final String name;

    /** The content of source this file */
    private final char[] chars;

    /** The encoding of this source file or null if unspecified */
    private String encoding = "ISO-8859-1";

    /** The position of the last line returned by getLine */
    private int lineNumber = 0;
    private int lineStart  = 0;
    private int lineLength = 0;
    private int nextIndex  = 0;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public SourceFile2(String name, String encoding)
        throws IOException {
        this(new File(name), encoding);
    }

    /** Initializes a new instance. */
    public SourceFile2(File name, String encoding)
        throws IOException {
        this(name.toString(), read(name, encoding));
    }

    /** Initializes a new instance. */
    public SourceFile2(String name, InputStream input, String encoding)
        throws IOException {
        this(name, read(name, input, encoding));
    }

    /** Initializes a new instance. */
    public SourceFile2(String name, char[] inp) {
        this.name = name;
        this.chars = normalize( inp );
    }

    //########################################################################
    // Public Methods

    /** Returns the name of this source file. */
    public String name() {
        return name;
    }

    /** Returns the content of this source file. */
    public char[] chars() {
        return chars;
    }

    /** Returns the short name (name without path) of this source file. */
    public String getShortName() {
        int start = name.lastIndexOf( File.separatorChar );
        return start < 0 ? name : name.substring( start + 1 );
    }

    /**
     * Returns an instance of Position representing the given line and
     * column of this source file.
     */
    public Position getPosition( int line, int column ) {
        return new Position( this, line, column );
    }

    /** Returns the content of the given line. */
    public String getLine(int line) {
        int index = lineNumber <= line ? nextIndex : (lineNumber = 0);
        for (; index < chars.length && lineNumber < line; lineNumber++) {
            lineStart = index;
            for (; index < chars.length; index++) {
                if (chars[ index ] == CR) break;
                if (chars[ index ] == LF) break;
                if (chars[ index ] == FF) break;
            }
            lineLength = index - lineStart;
            if (index < chars.length)
            	index++;
            if (index < chars.length)
                if (chars[index - 1] == CR && chars[index] == LF) index++;
        }
        nextIndex = index;
        return new String(chars, lineStart, lineLength);
    }

    /** Returns the name of this source file. */
    public String toString() {
        return name;
    }

    //########################################################################
    // Private Methods

    /** Reads the file and returns its content as a byte array.
     */
    private static char[] read(File file, String encoding) throws IOException {
        FileInputStream input = new FileInputStream( file );
        try {
            return read( input, encoding );
        } finally {
            input.close();
        }
    }

    private static char[] read(ByteBuffer bb, String encoding)
        throws IOException  {
        try {
            CharsetDecoder chd = Charset.forName( encoding ).newDecoder();
            CharBuffer cb = chd.decode( bb );
            if( cb.hasArray() )
                return cb.array();
            else {
                char[] cs = new char[ cb.length() ];
                for(int i = 0; cb.hasRemaining(); i++ ) {
                    cs[i] = cb.get();
                }
                return cs;
            }
        } catch (CharacterCodingException e) {
            throw new IOException("error in decoding:"+e.getMessage());
        }
    }

    private static char[] read(FileInputStream input, String encoding)
        throws IOException {
        return read( input
                     .getChannel()
                     .map( FileChannel.MapMode.READ_ONLY, 0, input.available()),
                     encoding );
    }

    /** Reads the InputStream and returns its content as a byte array.
     */
    private static char[] read(String name, InputStream input, String encoding) throws IOException {
        ByteBuffer bb = ByteBuffer.allocate( input.available() );
        Channels.newChannel( input ).read( bb );
        return read( bb, encoding );
    }

    /** Ensures that the last byte of the array is SU. */
    private static char[] normalize( char[] input ) {
        if ( input.length > 0 && input[ input.length - 1 ] == SU )
            return input;
        char[] cs = new char[ input.length + 1 ];
        System.arraycopy( input, 0, cs, 0, input.length );
        cs[ input.length ] = SU;
        return cs;
    }

    //########################################################################
}
