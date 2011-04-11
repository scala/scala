/*
 * System.Reflection-like API for acces to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil.util;

import ch.epfl.lamp.compiler.msil.PEFile;
import ch.epfl.lamp.compiler.msil.PEFile.Sig;

import java.io.PrintStream;
import java.io.IOException;

import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/**
 * Implements support for CLI streams within a PE file.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class PEStream implements Signature {

    //##########################################################################
    // Members

    /** The name of the stream. */
    public final String name;

    /** The offset of the stream from the beginning of the file. */
    public final int offset;

    /** The size of the stream in bytes; shall be multiple of 4. */
    public final int size;

    private final PEFile file;

    private final ByteBuffer buffer;

    //##########################################################################

    /** The PEStream class constructor.
     *  @param file - the PEFile to which this stream belongs
     */
    public PEStream(PEFile file) {
	this.file = file;
	offset = file.fromRVA(file.rvaMetadata + file.readInt());
	size = file.readInt();
	buffer = file.getBuffer(offset, size);

	int i = 0;
	byte [] _buf = new byte [16];
	do {
	    _buf[i] = (byte) file.readByte();
	    i++;
	} while(0 != _buf[i-1]);
	name = new String(_buf, 0, i - 1);

	file.align(PEFile.INT_SIZE, file.posMetadata);
	//assert size % 4 == 0;
    }

    /** Move to the specified position in the stream. */
    private  void seek(int pos) {
	try {
	    buffer.position(pos);
	} catch (IllegalArgumentException e) {
	    System.err.println("\nSeek failed in file " + file
			       + " for position " + pos
			       + " of stream " + name + " (" + buffer + ")");
	    throw e;
	}
    }

    /** Return a string from the specified position in the stream. */
    public String getString(int pos) {
	seek(pos);
	buffer.mark();
	int i;
	for (i = 0; getByte() != 0; i++);
	byte[] buf = new byte[i];
	buffer.reset(); // go back to the marked position
	buffer.get(buf);
	try {
	    return new String(buf, "UTF-8");
	} catch (java.io.UnsupportedEncodingException e) {
	    throw new RuntimeException(e);
	}
    }

    /** Read a byte from the stream. */
    public int getByte() {
	return (buffer.get() + 0x0100) & 0xff;
    }

    /** Return the GUID at the given position in the stream. */
    public byte[] getGUID(int pos) {
	seek(pos);
	byte[] buf = new byte[32]; // 128-bit GUID
	try {
	    buffer.get(buf);
	} catch (Exception e) {
	    System.err.println();
	    System.err.println("PEStream.getBlob(): Exception for pos = " +
			       pos + " and buf.length = " + buf.length);
	    System.err.println("\tbuffer = " + buffer);
	    e.printStackTrace();
	    throw new RuntimeException();
	}
	return buf;
    }

    public int readLength() {
	int length = getByte();
	if ((length & 0x80) != 0) {
	    length = ((length & 0x7f) << 8) | getByte();
	    if ((length & 0x4000) != 0)
		length = ((length & 0x3fff) << 16) | (getByte()<<8) | getByte();
	}
	return length;
    }

    /** Return a blob from the specified position in the stream. */
    public byte[] getBlob(int pos) {
	seek(pos);
	// the length indicates the number of bytes
	// AFTER the encoded size of the blob
	int length = readLength();
	byte[] buf = new byte[length];
	buffer.get(buf);
	return buf;
    }

    /***/
    public Sig getSignature(int pos) {
	seek(pos);
	return file.newSignature(buffer);
    }

    /**
     */
    public Object getConstant(int type, int pos) {
	Object val = null;
	seek(pos);
	int length = readLength(); // skip over the blob length field
	switch (type) {
	case ELEMENT_TYPE_BOOLEAN:
	    assert length == 1;
	    return buffer.get() == 0 ? Boolean.FALSE : Boolean.TRUE;
	case ELEMENT_TYPE_CHAR:
	    assert length == 2 : "length == " + length;
	    return new Character(buffer.getChar());
	case ELEMENT_TYPE_I1:
	case ELEMENT_TYPE_U1:       // TODO U1 not the same as I1
	    assert length == 1;
	    return new Byte(buffer.get());
	case ELEMENT_TYPE_I2:
	case ELEMENT_TYPE_U2:
	    assert length == 2;
	    return new Short(buffer.getShort());
	case ELEMENT_TYPE_I4:
	case ELEMENT_TYPE_U4:
	    assert length == 4;
	    return new Integer(buffer.getInt());
	case ELEMENT_TYPE_I8:
	case ELEMENT_TYPE_U8:
	    assert length == 8;
	    return new Long(buffer.getLong());
	case ELEMENT_TYPE_R4:
	    assert length == 4;
	    return new Float(buffer.getFloat());
	case ELEMENT_TYPE_R8:
	    assert length == 8;
	    return new Double(buffer.getDouble());
	case ELEMENT_TYPE_STRING:
// 	    length /= 2;
// 	    char[] chars = new char[length];
// 	    for (int i = 0; i < length; i++)
// 		chars[i] = buffer.getChar();
// 	    val = new String(chars);
	    try {
		return new String(getBlob(pos), "UTF-16LE");
	    } catch(java.io.UnsupportedEncodingException e) {
		throw new RuntimeException(e);
	    }
	default: throw new RuntimeException("Illegal constant type: " + type);
	}
    }

    public void dump(PrintStream out) {
	out.println("Stream name:   " + name + " (length " +
		   name.length() + " characters)");
	out.println("Stream offset: 0x" + PEFile.int2hex(offset));
	out.println("Stream size:   0x" + PEFile.int2hex(size));
    }

    //##########################################################################
}  // class PEStream
