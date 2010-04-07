/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.*;
import ch.epfl.lamp.compiler.msil.util.Table.*;

import ch.epfl.lamp.compiler.msil.Type;
import ch.epfl.lamp.compiler.msil.Module;

import java.io.File;
import java.io.RandomAccessFile;
import java.io.PrintStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.MappedByteBuffer;

import java.util.Date;

/**
 * A class that represents a .NET PE/COFF image.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 * @see <a href="http://www.ecma-international.org/publications/standards/Ecma-335.htm">Standard ECMA-335:  Common Language Infrastructure (CLI), 4th edition (June 2006)</a>
 */
public class PEFile {

    //##########################################################################

    public static final int INT_SIZE = 4;

    protected final int PE_SIGNATURE_OFFSET;
    protected final int COFF_HEADER_OFFSET;
    protected final int PE_HEADER_OFFSET;

    protected final int numOfSections;
    protected final int CLI_RVA;
    protected final int CLI_Length;
    public final int rvaMetadata;
    public final int posMetadata;
    protected final int numOfStreams;
    protected final int optHeaderSize;

    protected final File underlyingFile;
    protected final RandomAccessFile file;
    protected final MappedByteBuffer buf;

    protected final PESection [] sections;

    public PEStream Meta, Strings, US, Blob, GUID;

    private final Table [] tables = new Table[Table.MAX_NUMBER];

    public final boolean isDLL;

    protected final int heapSizes;
    public final boolean StringIsShort, BlobIsShort, GUIDIsShort;

    protected PEModule pemodule = null;

    //##########################################################################
    // PEFile constructor

    private static void fileFormatCheck(boolean cond, String s) {
	if (cond)
	    throw new RuntimeException(s);
    }

    /**
     */
    public PEFile(String filename) throws FileNotFoundException {
	this.underlyingFile = new File(filename);
	this.file = new RandomAccessFile(underlyingFile, "r");
	FileChannel fc = file.getChannel();
	MappedByteBuffer bb = null;
	try {
	    bb = fc.map(FileChannel.MapMode.READ_ONLY, 0L, fc.size());
 	} catch (IOException e) { throw new RuntimeException(e); }

    /** Ecma 335, 25 File format extensions to PE:
     *
     *  "Unless stated otherwise, all binary values are stored in little-endian format."
     */

	bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
	this.buf = bb;

    /** Ecma 335, 25.2.1 MS-DOS header:
     *
     *  "The PE format starts with an MS-DOS stub of exactly the following 128 bytes to
     *  be placed at the front of the module."
     *
     *  We are only checking for MZ (Mark Zbikowski)
     */

	seek(0);
	fileFormatCheck(readByte() != 0x4d, "Invalid PE file format: " + filename); // 'M'
	fileFormatCheck(readByte() != 0x5a, "Invalid PE file format: " + filename); // 'Z'

    /** Ecma 335, 25.2.1 MS-DOS header:
     *
     *  "At offset 0x3c in the DOS header is a 4-byte unsigned integer offset, lfanew,
     *  to the PE signature (shall be “PE\0\0”), immediately followed by the PE file header.
     */

	seek(0x3c);
	PE_SIGNATURE_OFFSET = readInt();
	seek(PE_SIGNATURE_OFFSET);

	fileFormatCheck(readByte() != 0x50, "Invalid PE file format: " + filename); // 'P'
	fileFormatCheck(readByte() != 0x45, "Invalid PE file format: " + filename); // 'E'
    fileFormatCheck(readByte() != 0x00, "Invalid PE file format: " + filename); //  0
    fileFormatCheck(readByte() != 0x00, "Invalid PE file format: " + filename); //  0

	//trace("PE signature offset = 0x" + Table.int2hex(PE_SIGNATURE_OFFSET));

	COFF_HEADER_OFFSET = PE_SIGNATURE_OFFSET + 4;
	PE_HEADER_OFFSET = COFF_HEADER_OFFSET + 20;

	seek(COFF_HEADER_OFFSET);
	skip(2);
    /** Ecma 335, 25.2.2: "Number of sections; indicates size of the Section Table" */
	numOfSections = readShort();
	//trace("Number of sections = " + numOfSections);

    /** Ecma 335, 25.2.2: "Time and date the file was created in seconds since
     *  January 1st 1970 00:00:00 or 0."
     */
	Date timeStamp = new Date(readInt() * 1000L);
	//trace("Time stamp = " + timeStamp);

	skip(2 * INT_SIZE);
	optHeaderSize = readShort();
	int characteristics = readShort();
	isDLL = (characteristics & 0x2000) != 0;
	//trace("Characteristics = " + Integer.toHexString(characteristics));

	seek(PE_HEADER_OFFSET + 208); // p.157, Partition II

 	CLI_RVA = readInt();
	CLI_Length = readInt();
	//trace("CLI_RVA = 0x" + Table.int2hex(CLI_RVA));
	//trace("CLI_Length = 0x" + Table.int2hex(CLI_Length));

	sections = new PESection[numOfSections];

	seek(PE_HEADER_OFFSET + optHeaderSize); // go to the sections descriptors

	for (int i = 0; i < numOfSections; i++) {
	    seek(PE_HEADER_OFFSET + optHeaderSize + i * 40);
	    sections[i] = new PESection(this);
	    //sections[i].dump(System.out);
	}

	seek(fromRVA(CLI_RVA));
	skip(8);
	rvaMetadata = readInt();
	posMetadata = fromRVA(rvaMetadata);
	//trace("rvaMetadata = 0x" + Table.int2hex(rvaMetadata));
	//trace("posMetadata = 0x" + Table.int2hex(posMetadata));

	seek(posMetadata);
	int magic = readInt();
	//trace("Magic metadata signature = 0x" + Table.int2hex(magic));
	fileFormatCheck(magic != 0x424a5342, "Invalid metadata signature!");
	skip(8);

	int strlength = readInt();
	//trace("version name string length = " + strlength);
	skip(strlength);
	align(INT_SIZE, posMetadata);
	//trace("position of flags = 0x" + Table.int2hex((int)pos()));
	skip(2); // ignore the flags
	numOfStreams = readShort();
	//trace("Number of metadata streams = " + numOfStreams);

	for (int i = 0; i < numOfStreams; i++) {
	    PEStream strm = new PEStream(this);
	    //strm.dump(System.out);
	    if (strm.name.equals("#~")
		|| strm.name.equals("#-"))    Meta = strm;
	    if (strm.name.equals("#Strings")) Strings = strm;
	    if (strm.name.equals("#US"))      US = strm;
	    if (strm.name.equals("#Blob"))    Blob = strm;
	    if (strm.name.equals("#GUID"))    GUID = strm;
	}

	seek(Meta.offset);
	skip(6);
	heapSizes = readByte();
	StringIsShort = (heapSizes & 0x01) == 0;
	GUIDIsShort   = (heapSizes & 0x02) == 0;
	BlobIsShort   = (heapSizes & 0x04) == 0;

	skip(1);
	long tablesMask = readLong();
	long nonStandardTables = tablesMask & ~Table.VALID_TABLES_MASK;
	skip(8); //go to the list of number of rows
	for (int i = 0; i < tables.length; i++) {
	    tables[i] = Table.newTable
		(this, i, ((tablesMask >> i) & 0x01) != 0 ? readInt() : 0);
	}

	initIndexSize();
	initTableRefs();
	// populate the tables from the CLI image file
	long start = pos();
	for (int i = 0; i < tables.length; i++)
	    start = tables[i].init(start);

    } // PEFile()


    public final int[] indexSize = new int[Table.TABLE_SET_LENGTH];

    private void initIndexSize() {
	for (int i = 0; i < Table.TABLE_SET_LENGTH; i++) {
	    indexSize[i] = 2;
	    int[] tableSet = Table.TableSet[i];
	    int treshold = (65536 >> Table.NoBits[i]);
	    for (int j = 0; j < tableSet.length; j++) {
		if (tableSet[j] >= 0) {
		    Table t = tables[tableSet[j]];
		    if (t.rows >= treshold) {
			indexSize[i] = 4;
			break;
		    }
		}
	    }
	}
    }

    protected void initModule(PEModule module) {
	if (pemodule != null)
	    throw new RuntimeException("File " + this
				       + " has already been assigned module "
				       + pemodule + "; new module is " + module);
	this.pemodule = module;
    }

    //##########################################################################

    public ModuleDef ModuleDef;
    public ModuleDef ModuleDef(int i) {
	ModuleDef.readRow(i);
	return ModuleDef;
    }

    public TypeRef TypeRef;

    public TypeDef TypeDef;
    public TypeDef TypeDef(int i) {
	TypeDef.readRow(i);
	return TypeDef;
    }

    public FieldTrans FieldTrans;
    public FieldTrans FieldTrans(int i) {
	FieldTrans.readRow(i);
	return FieldTrans;
    }

    public FieldDef FieldDef;
    public FieldDef FieldDef(int i) {
	FieldDef.readRow(i);
	return FieldDef;
    }

    public MethodTrans MethodTrans;
    public MethodTrans MethodTrans(int i) {
	MethodTrans.readRow(i);
	return MethodTrans;
    }

    public MethodDef MethodDef;
    public MethodDef MethodDef(int i) { MethodDef.readRow(i); return MethodDef; }


    public ParamDef ParamDef;
    public ParamDef ParamDef(int i) { ParamDef.readRow(i); return ParamDef; }

    public InterfaceImpl InterfaceImpl;
    public MemberRef MemberRef;
    public Constant Constant;
    public CustomAttribute CustomAttribute;
    public FieldMarshal FieldMarshal;
    public DeclSecurity DeclSecurity;
    public ClassLayout ClassLayout;
    public FieldLayout FieldLayout;
    public StandAloneSig StandAloneSig;
    public EventMap EventMap;
    public EventDef EventDef;
    public PropertyMap PropertyMap;
    public PropertyDef PropertyDef;
    public MethodSemantics MethodSemantics;
    public MethodImpl MethodImpl;
    public ModuleRef ModuleRef;
    public TypeSpec TypeSpec;
    public ImplMap ImplMap;
    public FieldRVA FieldRVA;
    public AssemblyDef AssemblyDef;
    public AssemblyRef AssemblyRef;
    public FileDef FileDef;
    public ExportedType ExportedType;
    public ManifestResource ManifestResource;
    public NestedClass NestedClass;


    private void initTableRefs() {
	ModuleDef = (ModuleDef) getTable(Table.ModuleDef.ID);
	TypeRef = (TypeRef) getTable(Table.TypeRef.ID);
	TypeDef = (TypeDef) getTable(Table.TypeDef.ID);
	FieldTrans = (FieldTrans) getTable(Table.FieldTrans.ID);
	FieldDef = (FieldDef) getTable(Table.FieldDef.ID);
	MethodTrans = (MethodTrans) getTable(Table.MethodTrans.ID);
	MethodDef = (MethodDef) getTable(Table.MethodDef.ID);
	ParamDef = (ParamDef) getTable(Table.ParamDef.ID);
	InterfaceImpl = (InterfaceImpl) getTable(Table.InterfaceImpl.ID);
	MemberRef = (MemberRef) getTable(Table.MemberRef.ID);
	Constant = (Constant) getTable(Table.Constant.ID);
	CustomAttribute = (CustomAttribute) getTable(Table.CustomAttribute.ID);
	FieldMarshal = (FieldMarshal) getTable(Table.FieldMarshal.ID);
	DeclSecurity = (DeclSecurity) getTable(Table.DeclSecurity.ID);
	ClassLayout = (ClassLayout) getTable(Table.ClassLayout.ID);
	FieldLayout = (FieldLayout) getTable(Table.FieldLayout.ID);
	StandAloneSig = (StandAloneSig) getTable(Table.StandAloneSig.ID);
	EventMap = (EventMap) getTable(Table.EventMap.ID);
	EventDef = (EventDef) getTable(Table.EventDef.ID);
	PropertyMap = (PropertyMap) getTable(Table.PropertyMap.ID);
	PropertyDef = (PropertyDef) getTable(Table.PropertyDef.ID);
	MethodSemantics = (MethodSemantics) getTable(Table.MethodSemantics.ID);
	MethodImpl = (MethodImpl) getTable(Table.MethodImpl.ID);
	ModuleRef = (ModuleRef) getTable(Table.ModuleRef.ID);
	TypeSpec = (TypeSpec) getTable(Table.TypeSpec.ID);
	ImplMap = (ImplMap) getTable(Table.ImplMap.ID);
	FieldRVA = (FieldRVA) getTable(Table.FieldRVA.ID);
	AssemblyDef = (AssemblyDef) getTable(Table.AssemblyDef.ID);
	AssemblyRef = (AssemblyRef) getTable(Table.AssemblyRef.ID);
	FileDef = (FileDef) getTable(Table.FileDef.ID);
	ExportedType = (ExportedType) getTable(Table.ExportedType.ID);
	NestedClass = (NestedClass) getTable(Table.NestedClass.ID);
	ManifestResource =
	    (ManifestResource) getTable(Table.ManifestResource.ID);
    }

    public static String long2hex(long a) {
	StringBuffer str = new StringBuffer("0000000000000000");
	str.append(Long.toHexString(a));
	int l = str.length();
	return str.substring(l - 16, l);
    }

    public static String int2hex(int a) {
	StringBuffer str = new StringBuffer("00000000");
	str.append(Integer.toHexString(a));
	int l = str.length();
	return str.substring(l - 8, l);
    }

    public static String short2hex(int a) {
	StringBuffer str = new StringBuffer("0000");
	str.append(Integer.toHexString(a));
	int l = str.length();
	return str.substring(l - 4, l);
    }

    public static String byte2hex(int a) {
	StringBuffer str = new StringBuffer("00");
	str.append(Integer.toHexString(a));
	int l = str.length();
	return str.substring(l - 2, l);
    }

    public static String bytes2hex(byte[] buf) {
	StringBuffer str = new StringBuffer();
	for (int i = 0; i < buf.length; i++) {
	    str.append(byte2hex(buf[i]));
	    if (i < buf.length - 1)
		str.append(" ");
	}
	return str.toString();
    }

    //##########################################################################
    // filename

    public File getUnderlyingFile() {
	return underlyingFile;
    }

    /**
     * @return the absolute path of the file
     */
    public String getAbsolutePath() {
	return underlyingFile.getAbsolutePath();
    }

    /**
     * @return the name of this file
     */
    public String getName() {
	return underlyingFile.getName();
    }

    /**
     * @return
     */
    public String getParent() {
	return underlyingFile.getParent();
    }

    /**
     * @return the file representing the directory the file belongs to
     */
    public File getParentFile() {
	return underlyingFile.getParentFile();
    }

    public String toString() {
	return getAbsolutePath();
    }

    //##########################################################################
    // file pointer manipulation methods

    /** Returns the current position in the file. */
    public int pos() {
	return buf.position();
    }

    /** Go to the specified position in the file. */
    public void seek(int pos) {
	buf.position(pos);
    }


    /** Align the current position in the file. */
    public void align(int base) { align(base, 0); }

    /** Align the current position in a section starting at offset. */
    public void align(int base, int offset) {
	int p = pos() - offset;
	seek( offset + ((p % base) == 0 ? p : (p/base + 1) * base));
    }

    /** Computes the position in the file that corresponds to the given RVA. */
    public int fromRVA(int rva) {
	int i;
	for(i = 0; i < numOfSections; i++)
	    if(sections[i].virtAddr <= rva &&
	       rva <= (sections[i].virtAddr + sections[i].virtSize))
		return rva - sections[i].virtAddr + sections[i].realAddr;
	throw new RuntimeException("RVA 0x" + Integer.toHexString(rva) +
				   " is not within this file's sections!");
    }

    /** Go to the specified RVA (Relative Virtual Address). */
    public void gotoRVA(int rva) {
	seek(fromRVA(rva));
    }

    /** Move the forward in the file by the specified number of bytes. */
    public void skip(int n) {
	buf.position(buf.position() + n);
    }

    /**
     * Returns a memory mapped little-endian buffer
     * for the specified region of the file.
     */
    public MappedByteBuffer mapBuffer(long offset, int size) {
 	try {
	    MappedByteBuffer b = file.getChannel()
		.map(FileChannel.MapMode.READ_ONLY, offset, size);
	    b.order(java.nio.ByteOrder.LITTLE_ENDIAN);
	    return b;
 	} catch (IOException e) { throw new RuntimeException(e); }
    }

    /** Returns a buffer from the given offset to the end of the file. */
    public ByteBuffer getBuffer(long offset, int size) {
	buf.mark();
	buf.position((int)offset);
	ByteBuffer bb = buf.slice();
	buf.reset();
	bb.limit(size);
	bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
	return bb;
    }

    //##########################################################################
    // file read methods

    /**
     * Read bs.length number of bytes
     */
    public void read(byte[] bs) {
	buf.get(bs);
    }

    /**
     * Read 1-byte integer from the current position in the file.
     */
    public int readByte() {
	return buf.get();
    }

    /**
     * Read 2-byte integer from the current position in the file.
     */
    public int readShort() {
	return buf.getShort();
    }

    /**
     * Read 4-byte integer from the current position in the file.
     */
    public int readInt() {
	return buf.getInt();
    }

    /**
     * Read 8-byte integer from the current position in the file.
     */
    public long readLong() {
	return buf.getLong();
    }

    /**
     * @return the size of string indeces for this file.
     */
    public int getStringIndexSize() {
	return StringIsShort ? 2 : 4;
    }

    /**
     * @return the size of GUID indeces for this file.
     */
    public int getGUIDIndexSize() {
	return GUIDIsShort ? 2 : 4;
    }

    /**
     * @return the size of Blob indeces for this file.
     */
    public int getBlobIndexSize() {
	return BlobIsShort ? 2 : 4;
    }

    /**
     * @return the size of the index to tableID for this file;
     * @param tableID the ID of the table
     */
    public int getTableIndexSize(int tableID) {
	return tables[tableID].isShort ? 2 : 4;
    }

    /**
     * @return the size of the index to a set of tables with the given @param TableSetID
     * @param tableSetID the ID of the table set
     */
    public int getTableSetIndexSize(int tableSetID) {
	return indexSize[tableSetID];
    }

    /**
     * Read a String index from the current position in the file.
     * @return an index into the String stream
     */
    public int readStringIndex() {
	return StringIsShort ? readShort() : readInt();
    }

    /**
     * Read a GUID index from the current position in the file.
     * @return an index in to the GUID stream
     */
    public int readGUIDIndex() {
	return GUIDIsShort ? readShort() : readInt();
    }

    /**
     * Read a Blob index from the current position in the file.
     * @return an index into the Blob stream
     */
    public int readBlobIndex() {
	return BlobIsShort ? readShort() : readInt();
    }

    /** Read an entry interpreted as index into table @param tableID. */
    public int readTableIndex(int tableId) {
	return tables[tableId].isShort ? readShort() : readInt();
    }

    /***/
    public int readTableSetIndex(int tableSetId) {
	return indexSize[tableSetId] == 2 ? readShort() : readInt();
    }

    /**
     * Read a string from the String stream
     * @return the string at the given position
     * @param pos the position of the string in the String stream
     */
    public String getString(int pos) {
	String s = Strings.getString(pos);
	return s;//.length() == 0 ? null : s;
    }

    /**
     * Read a string from the US (User Strings) stream
     * @return the string at the given position
     * @param pos the position of the string in the US stream
     */
    public String getUString(int pos) {
	return US.getString(pos);
    }

    /**
     * Read a blob from the Blob Stream
     * @return the blob at the given position
     * @param pos the position of the blob in the Blob stream
     */
    public byte[] getBlob(int pos) {
	return Blob.getBlob(pos);
    }

    /***/
    public Sig getSignature(int pos) {
	//return new Sig(getBlob(pos));
	return Blob.getSignature(pos);
    }

    /***/
    public byte[] getGUID(int pos) {
	return GUID.getGUID(pos);
    }

    /**
     * @return the table with the corresponding ID.
     */
    public final Table getTable(int tableID) {
	return tables[tableID];
    }

    //##########################################################################

    /***/
    void trace(String msg) {
	System.out.println("[trace] " + msg);
    }

    //##########################################################################

    public Sig newSignature(ByteBuffer buf) {
	return new Sig(buf);
    }

    /**
     */
    public class Sig implements Signature {

	//######################################################################
	// instance members

	protected final ByteBuffer buf;
	protected final int pos;
	protected final int length;

	public Sig(ByteBuffer buf) {
	    this.buf = buf;
	    //int tmpPos = buf.position();
	    length = decodeInt();
	    this.pos = buf.position();
	}

	public String toString() {
	    StringBuffer b = new StringBuffer("(");
	    reset();
	    for (int i = 0; i < length; i++) {
		b.append(byte2hex(readByte()));
		if (i < length - 1)
		    b.append(" ");
	    }
	    return b.append(")").toString();
	}

	public Sig reset() { buf.position(pos); return this; }

	public int pos() { return buf.position() - pos; }

	/** @return the byte at the current position in the signature Blob.
	 *  Stay at the same position
	 */
	public int getByte() {
	    return (buf.get(buf.position()) + 0x100) & 0xff;
	}

	/** @return the byte at the current position in the signature Blob.
	 *  Move to the next byte.
	 */
	public int readByte() { return (buf.get() + 0x100) & 0xff; }

	/** Skip the current byte if equal to the given value. */
	public void skipByte(int b) { if (b == getByte()) buf.get(); }

	/** Decodes an integer from the signature Blob.
	 *  @return the decoded integer
	 */
	public int decodeInt() {
	    int res = readByte();
	    if ((res & 0x80) != 0) {
		res = ((res & 0x7f) << 8) | readByte();
		if ((res & 0x4000) != 0)
		    res = ((res & 0x3fff)<<16) | (readByte()<<8) | readByte();
	    }
	    return res;
	}

	/** @return - the type encoded at the current position in the signature
	 *  according to 22.2.12
	 */
	public Type decodeType() {
	    try { return decodeType0(); }
	    catch (RuntimeException e) {
		System.out.println("" + pos() + "@" + this);
		throw e;
	    }
	}

	public Type decodeType0() {
	    Type type = null;
	    int desc = readByte();
	    switch (desc) {
	    case ELEMENT_TYPE_BOOLEAN:type = Type.GetType("System.Boolean"); break;
	    case ELEMENT_TYPE_CHAR:   type = Type.GetType("System.Char"); break;
	    case ELEMENT_TYPE_I1:     type = Type.GetType("System.SByte"); break;
	    case ELEMENT_TYPE_U1:     type = Type.GetType("System.Byte"); break;
	    case ELEMENT_TYPE_I2:     type = Type.GetType("System.Int16"); break;
	    case ELEMENT_TYPE_U2:     type = Type.GetType("System.UInt16"); break;
	    case ELEMENT_TYPE_I4:     type = Type.GetType("System.Int32"); break;
	    case ELEMENT_TYPE_U4:     type = Type.GetType("System.UInt32"); break;
	    case ELEMENT_TYPE_I8:     type = Type.GetType("System.Int64"); break;
	    case ELEMENT_TYPE_U8:     type = Type.GetType("System.UInt64"); break;
	    case ELEMENT_TYPE_R4:     type = Type.GetType("System.Single"); break;
	    case ELEMENT_TYPE_R8:     type = Type.GetType("System.Double"); break;
	    case ELEMENT_TYPE_OBJECT: type = Type.GetType("System.Object"); break;
	    case ELEMENT_TYPE_STRING: type = Type.GetType("System.String"); break;
	    case ELEMENT_TYPE_I:      type = Type.GetType("System.IntPtr"); break;
	    case ELEMENT_TYPE_U:      type = Type.GetType("System.UIntPtr"); break;
	    case ELEMENT_TYPE_PTR:        // Followed by <type> token.
		if (getByte() == ELEMENT_TYPE_VOID) {
		    readByte();
		    type = Type.mkPtr(Type.GetType("System.Void"));
		} else type = Type.mkPtr(decodeType());
		break;
	    case ELEMENT_TYPE_BYREF:      // Followed by <type> token.
	    case ELEMENT_TYPE_VALUETYPE:  // Followed by <type> token
		//System.out.println("Signature.getType(): valuetype");
		//type = pemodule.getTypeDefOrRef(decodeInt());
	    case ELEMENT_TYPE_CLASS:
		// Followed by <type> token
		type = pemodule.getTypeDefOrRef(decodeInt());
		if (type == null) throw new RuntimeException();
		break;

	    case ELEMENT_TYPE_SZARRAY:    // Single-dim array with 0 lower bound.
		skipCustomMods();
		type = Type.mkArray(decodeType(), 1);
		break;
	    case ELEMENT_TYPE_ARRAY:
		// <type> <rank> <boundsCount> <bound1> ... <loCount> <lo1> ...
		Type elem = decodeType();
		int rank = decodeInt();
		int numSizes = decodeInt();
		for (int i = 0; i < numSizes; i++)
		    decodeInt();
		int numLoBounds = decodeInt();
		for (int i = 0; i < numLoBounds; i++)
		    decodeInt();
		type = Type.mkArray(elem, rank);
		break;

	    case ELEMENT_TYPE_FNPTR:
		// Followed by full method signature.
	    case ELEMENT_TYPE_END:
		// Marks end of a list
	    case ELEMENT_TYPE_CMOD_REQD:
		// Required modifier : followed by a TypeDef or TypeRef token.
	    case ELEMENT_TYPE_CMOD_OPT:
		// Optional modifier : followed by a TypeDef or TypeRef token.
	    case ELEMENT_TYPE_INTERNAL:
		// Implemented within the CLI.
	    case ELEMENT_TYPE_MODIFIER:
		// Or'd with following element types.
	    case ELEMENT_TYPE_SENTINEL:
		// Sentinel for varargs method signature.
	    case ELEMENT_TYPE_PINNED:
		// Denotes a local variable that points at a pinned object.
	    default:
		throw new RuntimeException(byte2hex(desc) +
					   "@" + pos() + " in " + this);

	    }
	    if (type == null) throw new RuntimeException();
	    return type;
	} // getType()

	public Type decodeFieldType() {
	    skipByte(FIELD);
	    skipCustomMods();
	    return decodeType();
	}

	/** decodes the return type of a method signature (22.2.11). */
	public Type decodeRetType() {
	    skipCustomMods();
	    switch (getByte()) {
	    case ELEMENT_TYPE_VOID:
		readByte();
		return Type.GetType("System.Void");
	    case ELEMENT_TYPE_TYPEDBYREF:
		return Type.GetType("System.TypedReference");
	    case ELEMENT_TYPE_BYREF:
		skipByte(ELEMENT_TYPE_BYREF);
		return decodeType();
	    default:
		return decodeType();
	    }
	}

	public Type decodeParamType() {
	    skipCustomMods();
	    switch (getByte()) {
	    case ELEMENT_TYPE_BYREF:
		skipByte(ELEMENT_TYPE_BYREF);
		return decodeType();
	    case ELEMENT_TYPE_TYPEDBYREF:
		return Type.GetType("System.TypedReference");
	    default:
		return decodeType();
	    }
	}

	public void skipCustomMods() {
	    while (getByte() == ELEMENT_TYPE_CMOD_OPT
		   || getByte() == ELEMENT_TYPE_CMOD_REQD)
		{
                    // skip the tag 23.2.7
                    readByte();
                    // skip the TypeDefOrRefEncoded (23.2.8)
                    readByte();
                    readByte();

                    // @FIXME: could be 4 bytes, not always 2...

                    //Type t = decodeType();
		    //System.err.println("CMOD: " + t);
		    //if (getByte() == ELEMENT_TYPE_CMOD_REQD)
                      //throw new RuntimeException("Reqired CMOD: " + t);
		}
	}

	//######################################################################

    }  // class Sig

    //##########################################################################

}  // class PEFile
