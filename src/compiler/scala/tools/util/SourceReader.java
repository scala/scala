/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;

/** This class implements methods to read and decode source files. */
public class SourceReader {

    //########################################################################
    // Private Fields

    /** The decoder used to transform bytes into characters */
    private final CharsetDecoder decoder;

    /** The input byte buffer (small enough to fit in cache) */
    private final ByteBuffer bytes;

    /** The output character buffer */
    private CharBuffer chars;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with the specified decoder. */
    public SourceReader(CharsetDecoder decoder) {
        this.decoder = decoder;
        this.bytes = ByteBuffer.allocate(0x4000);
        this.chars = CharBuffer.allocate(0x4000);
    }

    //########################################################################
    // Public Methods

    /** Reads the file with the specified name. */
    public char[] read(String filename) throws IOException {
        return read(new File(filename));
    }

    /** Reads the specified file. */
    public char[] read(File file) throws IOException {
        FileChannel channel = new FileInputStream(file).getChannel();
        try {
            return read(channel);
        } finally {
            channel.close();
        }
    }

    /** Reads the specified file. */
    public char[] read(AbstractFile file) throws IOException {
        CharsetDecoder decoder = this.decoder.reset();
        ByteBuffer bytes = ByteBuffer.wrap(file.read());
        CharBuffer chars = this.chars; chars.clear();
        return terminate(flush(decoder, decode(decoder, bytes, chars, true)));
    }

    /** Reads the specified byte channel. */
    public char[] read(ReadableByteChannel input) throws IOException {
        CharsetDecoder decoder = this.decoder.reset();
        ByteBuffer bytes = this.bytes; bytes.clear();
        CharBuffer chars = this.chars; chars.clear();
        for (boolean endOfInput = false; !endOfInput; ) {
            endOfInput = input.read(bytes) < 0;
            bytes.flip();
            chars = decode(decoder, bytes, chars, endOfInput);
        }
        return terminate(flush(decoder, chars));
    }

    //########################################################################
    // Private Methods

    /**
     * Sets the specified char buffer as the new output buffer and
     * reads and returns its content.
     */
    private char[] terminate(CharBuffer chars) {
        char[] result = new char[chars.length()];
        chars.get(result);
        this.chars = chars;
        return result;
    }

    //########################################################################
    // Private Functions

    /**
     * Decodes the content of the specified byte buffer with the
     * specified decoder into the specified char buffer, allocating
     * bigger ones if necessary, then compacts the byte buffer and
     * returns the last allocated char buffer. The "endOfInput"
     * argument indicates whether the byte buffer contains the last
     * chunk of the input file.
     */
    private static CharBuffer decode(CharsetDecoder decoder, ByteBuffer bytes,
        CharBuffer chars, boolean endOfInput) throws IOException
    {
        while (true) {
            CoderResult result = decoder.decode(bytes, chars, endOfInput);
            if (result.isUnderflow()) break;
            if (result.isError()) throw new IOException(result.toString());
            assert result.isOverflow();
            chars = increaseCapacity(chars);
        }
        bytes.compact();
        return chars;
    }

    /**
     * Flushes the specified decoder into the specified char buffer,
     * allocating bigger ones if necessary and then flips and returns
     * the last allocated char buffer.
     */
    private static CharBuffer flush(CharsetDecoder decoder, CharBuffer chars)
        throws IOException
    {
        while (true) {
            CoderResult result = decoder.flush(chars);
            if (result.isUnderflow()) break;
            if (result.isError()) throw new IOException(result.toString());
            assert result.isOverflow();
            chars = increaseCapacity(chars);
        }
        chars.flip();
        return chars;
    }

    /**
     * Flips the specified buffer and returns a new one with the same
     * content but with an increased capacity.
     */
    private static CharBuffer increaseCapacity(CharBuffer buffer) {
        buffer.flip();
        int capacity = 2 * buffer.capacity();
        return CharBuffer.allocate(capacity).put(buffer);
    }

    //########################################################################

}
