/*
 * System.Reflection-like API for acces to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil.util;

import ch.epfl.lamp.compiler.msil.PEFile;

import java.io.PrintStream;

/** Describes a section from a PE/COFF file
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class PESection {

    private final PEFile file;
    private final long sectionStart;

    public final String name;
    public final int virtAddr;
    public final int virtSize;
    public final int realAddr;
    public final int realSize;
    public final int flags;

    private static final byte[] buf = new byte[8];

    public PESection(PEFile file) {
	this.file = file;
	sectionStart = file.pos();
	file.read(buf);
 	int i;
 	for(i = 7; (i >= 0) && (0 == buf[i]); i--);
 	name = new String(buf, 0, i + 1);
	virtSize = file.readInt();
	virtAddr = file.readInt();
	realSize = file.readInt();
	realAddr = file.readInt();
	file.skip(3 * PEFile.INT_SIZE);
	flags = file.readInt();
    }


    public void dump(PrintStream out) {
	out.println("Section name:    " + name +
		   " (name.length=" + name.length() + ")");
	out.println("Virtual Address: 0x" + PEFile.int2hex(virtAddr));
	out.println("Virtual Size:    0x" + PEFile.int2hex(virtSize));
	out.println("Real Address:    0x" + PEFile.int2hex(realAddr));
	out.println("Real Size:       0x" + PEFile.int2hex(realSize));
	out.println("Flags:           0x" + PEFile.int2hex(flags));
    }

} // class PESection
