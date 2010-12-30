/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Array of bytes.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class ByteArray {
    protected final static int BYTE_BLOCK_BITS = 8;
    protected final static int BYTE_BLOCK_SIZE = 1 << BYTE_BLOCK_BITS;
    protected final static int BYTE_BLOCK_MASK = BYTE_BLOCK_SIZE - 1;

    protected byte[][] data = new byte[][] { new byte[BYTE_BLOCK_SIZE] };
    protected int pos = 0;  // The next free position.

    protected boolean frozen = false;

    public ByteArray() { }

    public ByteArray(InputStream stream, int size) throws IOException {
        pos = size;
        for (int i = 0; size > 0; ++i) {
            int sizeToRead = Math.min(BYTE_BLOCK_SIZE, size);
            stream.read(data[i], 0, sizeToRead);

            size -= sizeToRead;
            if (size > 0) addNewBlock();
        }
    }

    public void freeze() { frozen = true; }

    public int nextBytePosition() {
        return pos;
    }

    public int getSize() {
        return pos;
    }

    protected void addNewBlock() {
        int nextBlockPos = pos >>> BYTE_BLOCK_BITS;
        if (nextBlockPos == data.length) {
            byte[][] newData = new byte[data.length * 2][];
            System.arraycopy(data, 0, newData, 0, data.length);
            data = newData;
        }
        assert data[nextBlockPos] == null : pos + " " + nextBlockPos;
        data[nextBlockPos] = new byte[BYTE_BLOCK_SIZE];
    }

    protected void addByte(int b) {
        assert !frozen;

        if ((pos & BYTE_BLOCK_MASK) == 0 && pos > 0)
            addNewBlock();
        int currPos = pos++;
        data[currPos >>> BYTE_BLOCK_BITS][currPos & BYTE_BLOCK_MASK] = (byte)b;
    }

    public void addU1(int i) {
        assert i <= 0xFF : i;
        addByte(i);
    }

    public void addU2(int i) {
        assert i <= 0xFFFF : i;

        addByte(i >>> 8);
        addByte(i & 0xFF);
    }

    public void addU4(int i) {
        addByte(i >>> 24);
        addByte((i >>> 16) & 0xFF);
        addByte((i >>>  8) & 0xFF);
        addByte(i & 0xFF);
    }

    public void putByte(int targetPos, int b) {
        assert !frozen;
        assert targetPos < pos : targetPos + " >= " + pos;

        data[targetPos >>> BYTE_BLOCK_BITS][targetPos & BYTE_BLOCK_MASK] = (byte)b;
    }

    public void putU2(int targetPos, int i) {
        assert i < 0xFFFF : i;
        putByte(targetPos, i >>> 8);
        putByte(targetPos + 1, i & 0xFF);
    }

    public void putU4(int targetPos, int i) {
        putByte(targetPos, i >>> 24);
        putByte(targetPos + 1, (i >>> 16) & 0xFF);
        putByte(targetPos + 2, (i >>>  8) & 0xFF);
        putByte(targetPos + 3, i & 0xFF);
    }

    public int getU1(int sourcePos) {
        assert sourcePos < pos : sourcePos + " >= " + pos;
        return data[sourcePos >>> BYTE_BLOCK_BITS][sourcePos & BYTE_BLOCK_MASK] & 0xFF;
    }

    public int getU2(int sourcePos) {
        return (getU1(sourcePos) << 8) | getU1(sourcePos + 1);
    }

    public int getU4(int sourcePos) {
        return (getU2(sourcePos) << 16) | getU2(sourcePos + 2);
    }

    public int getS1(int sourcePos) {
        assert sourcePos < pos : sourcePos + " >= " + pos;
        return data[sourcePos >>> BYTE_BLOCK_BITS][sourcePos & BYTE_BLOCK_MASK];
    }

    public int getS2(int sourcePos) {
        return (getS1(sourcePos) << 8) | getU1(sourcePos + 1);
    }

    public int getS4(int sourcePos) {
        return (getS2(sourcePos) << 16) | getU2(sourcePos + 2);
    }

    public void writeTo(OutputStream stream) throws IOException {
        if (!frozen) freeze();

        for (int i = 0; i < data.length && data[i] != null; ++i) {
            int len = Math.min(BYTE_BLOCK_SIZE, pos - (i << BYTE_BLOCK_BITS));
            stream.write(data[i], 0, len);
        }
    }
}
