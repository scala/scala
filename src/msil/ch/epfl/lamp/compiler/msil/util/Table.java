/*
 * System.Reflection-like API for acces to .NET Assemblies
 */


package ch.epfl.lamp.compiler.msil.util;

import ch.epfl.lamp.compiler.msil.PEFile;
import ch.epfl.lamp.compiler.msil.PEFile.Sig;

import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;

/**
 * Represents a table in a .NET assembly
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class Table {

    //##########################################################################

    public static final int MAX_NUMBER = 64;

    public static final long VALID_TABLES_MASK = 0x03ff3fb7ff57L;

    //##########################################################################
    // fields and methods for handling predefined sets of tables

    public static final int TABLE_SET_LENGTH = 13;

    public static final int _TypeDefOrRef = 0;
    public static final int _HasConstant = 1;
    public static final int _HasCustomAttribute = 2;
    public static final int _HasFieldMarshal = 3;
    public static final int _HasDeclSecurity = 4;
    public static final int _MemberRefParent = 5;
    public static final int _HasSemantics = 6;
    public static final int _MethodDefOrRef = 7;
    public static final int _MemberForwarded = 8;
    public static final int _Implementation = 9;
    public static final int _CustomAttributeType = 10;
    public static final int _ResolutionScope = 11;
    public static final int _TypeOrMethodDef = 12;


    public static final int[][] TableSet = new int[TABLE_SET_LENGTH][];

    static {
	TableSet[_TypeDefOrRef] =
	    new int[] {TypeDef.ID, TypeRef.ID, TypeSpec.ID};
	TableSet[_HasConstant] =
	    new int[] {FieldDef.ID, ParamDef.ID, PropertyDef.ID};
	TableSet[_HasCustomAttribute] =
	    new int[] {MethodDef.ID, FieldDef.ID, TypeRef.ID, TypeDef.ID,
		       ParamDef.ID, InterfaceImpl.ID, MemberRef.ID, ModuleDef.ID,
		       -1, PropertyDef.ID, EventDef.ID, -1, ModuleRef.ID,
		       TypeSpec.ID, AssemblyDef.ID, AssemblyRef.ID,
		       FileDef.ID, ExportedType.ID, ManifestResource.ID};
	TableSet[_HasFieldMarshal] =
	    new int[] {FieldDef.ID, ParamDef.ID};
	TableSet[_HasDeclSecurity] =
	    new int[] {TypeDef.ID, MethodDef.ID, AssemblyDef.ID};
	TableSet[_MemberRefParent] =
	    new int[] {-1, TypeRef.ID, ModuleRef.ID, MethodDef.ID, TypeSpec.ID};
	TableSet[_HasSemantics] =
	    new int[] {EventDef.ID, PropertyDef.ID};
	TableSet[_MethodDefOrRef] =
	    new int[] {MethodDef.ID, MemberRef.ID};
	TableSet[_MemberForwarded] =
	    new int[] {FieldDef.ID, MethodDef.ID};
	TableSet[_Implementation] =
	    new int[] {FileDef.ID, AssemblyRef.ID, ExportedType.ID};
	TableSet[_CustomAttributeType] =
	    new int[] {-1, -1, MethodDef.ID, MemberRef.ID, -1};
	TableSet[_ResolutionScope] =
	    new int[] {ModuleDef.ID, ModuleRef.ID, AssemblyRef.ID, TypeRef.ID};
        TableSet[_TypeOrMethodDef] =
                new int[]{TypeDef.ID, MethodDef.ID};
    }

    public static final int[] NoBits =
            new int[]{2, 2, 5, 1, 2, 3, 1, 1, 1, 2, 3, 2, 1};

    public static int getMask(int tableSetId) {
	return (1 << NoBits[tableSetId]) - 1;
    }

    public static int getTableId(int tableSet, int index) {
	return TableSet[tableSet][index & getMask(tableSet)];
    }

    public static int getTableIndex(int tableSet, int index) {
	return index >> NoBits[tableSet];
    }

    public static int encodeIndex(int index, int tableSetId, int tableId) {
	int[] tableSet = TableSet[tableSetId];
	for (int i = 0; i < tableSet.length; i++) {
	    if (tableSet[i] == tableId)
		return (index << NoBits[tableSetId]) | i;
	}
	throw new RuntimeException("Cannot find table #" + tableId +
				   " in table set #" + tableSetId);
    }

    //##########################################################################

    private static final String [] tableName = {
	"Module",              "TypeRef",          "TypeDef", "   FieldTrans",
	"Field",               "MethodTrans",      "Method",      "",
	"Param",               "InterfaceImpl",    "MemberRef",   "Constant",
	"CustomAttribute",     "FieldMarshal",     "DeclSecurity","ClassLayout",
	"FieldLayout",         "StandAloneSig",    "EventMap",    "",
	"Event",               "PropertyMap",      "",            "Property",
	"MethodSemantics",     "MethodImpl",       "ModuleRef",   "TypeSpec",
	"ImplMap",             "FieldRVA",         "",            "",
	"Assembly",            "AssemblyProcessor","AssemblyOS",  "AssemblyRef",
	"AssemblyRefProcessor","AssemblyRefOS",    "File",        "ExportedType",
            "ManifestResource", "NestedClass", "GenericParam", "MethodSpec",
            "GenericParamConstraint", "", "", "",
	"",                    "",                 "",            "",
	"",                    "",                 "",            "",//0x30-0x37
	"",                    "",                 "",            "",
	"",                    "",                 "",            "" //0x37-0x3f
    };

    /** Creates a table with the given id and number of rows.
     */
    public static Table newTable(PEFile file, int id, int rows) {
	Table table = null;
	switch(id) {
	case ModuleDef.ID:         table = new ModuleDef(file, rows); break;
 	case TypeRef.ID:           table = new TypeRef(file, rows); break;
 	case TypeDef.ID:           table = new TypeDef(file, rows); break;
	case FieldTrans.ID:        table = new FieldTrans(file, rows); break;
	case FieldDef.ID:          table = new FieldDef(file, rows); break;
	case MethodTrans.ID:       table = new MethodTrans(file, rows); break;
	case MethodDef.ID:         table = new MethodDef(file, rows); break;
	case ParamDef.ID:          table = new ParamDef(file, rows); break;
	case InterfaceImpl.ID:     table = new InterfaceImpl(file, rows); break;
	case MemberRef.ID:         table = new MemberRef(file, rows); break;
	case Constant.ID:          table = new Constant(file, rows); break;
	case CustomAttribute.ID:   table = new CustomAttribute(file, rows); break;
	case FieldMarshal.ID:      table = new FieldMarshal(file, rows); break;
	case DeclSecurity.ID:      table = new DeclSecurity(file, rows); break;
	case ClassLayout.ID:       table = new ClassLayout(file, rows); break;
	case FieldLayout.ID:       table = new FieldLayout(file, rows); break;
	case StandAloneSig.ID:     table = new StandAloneSig(file, rows); break;
	case EventMap.ID:          table = new EventMap(file, rows); break;
	case EventDef.ID:          table = new EventDef(file, rows); break;
	case PropertyMap.ID:       table = new PropertyMap(file, rows); break;
	case PropertyDef.ID:       table = new PropertyDef(file, rows); break;
	case MethodSemantics.ID:   table = new MethodSemantics(file, rows); break;
	case MethodImpl.ID:        table = new MethodImpl(file, rows); break;
	case ModuleRef.ID:         table = new ModuleRef(file, rows); break;
	case TypeSpec.ID:          table = new TypeSpec(file, rows); break;
	case ImplMap.ID:           table = new ImplMap(file, rows); break;
	case FieldRVA.ID:          table = new FieldRVA(file, rows); break;
	case AssemblyDef.ID:       table = new AssemblyDef(file, rows); break;
	case AssemblyProcessor.ID: table = new AssemblyProcessor(file, rows); break;
	case AssemblyOS.ID:        table = new AssemblyOS(file, rows); break;
	case AssemblyRef.ID:       table = new AssemblyRef(file, rows); break;
	case AssemblyRefProcessor.ID:
	    table = new AssemblyRefProcessor(file, rows); break;
	case AssemblyRefOS.ID:     table = new AssemblyRefOS(file, rows); break;
	case FileDef.ID:           table = new FileDef(file, rows); break;
	case ExportedType.ID:      table = new ExportedType(file, rows); break;
	case ManifestResource.ID:  table = new ManifestResource(file, rows); break;
	case NestedClass.ID:       table = new NestedClass(file, rows); break;
    case GenericParam.ID:
        table = new GenericParam(file, rows);
        break;
    case MethodSpec.ID:
        table = new MethodSpec(file, rows);
        break;
    case GenericParamConstraint.ID:
        table = new GenericParamConstraint(file, rows);
        break;
	default:
	    table = new Empty(id);
	}
// 	System.out.println("created table " + table.getName() + " with "
// 			   + table.rows + " rows");
	return table;
    }


    //##########################################################################
    // public fields

    /** Number of rows in the table. */
    public final int rows;

    /** Table ID as specified in Partition II. */
    public final int id;

    /** The file to which the table belongs. */
    protected final PEFile file;

    /** Memory mapped buffer wrapping the table. */
    protected ByteBuffer buffer;

    /**
     * specified wheter a new memory-mapped byte buffer should be created
     * for this table.
     */
    protected boolean newMapping = false;

    /** Tells wheter the table is indexed by 2-byte (short) integer
     *  or by 4-byte integer. */
    public final boolean isShort;

    private int rowSize = -1;

    // the starting position of the table relative to the beginning of the file
    private long start = -1;

    // the number of the row who can be accessed via the fields of the table
    private int currentRow = 0;

    //##########################################################################

    protected Table(PEFile file, int id, int rows) {
	this.file = file;
	this.id = id;
	this.rows = rows;//file.readInt();
	this.isShort = rows < (1 << 16);
// 	assert ((1L << id) & VALID_TABLES_MASK) != 0
// 	    : "Table does not have a vaid ID: " + byte2hex(id);
    }

    /**
     * Additional table initialization.
     * @return the starting position of the next table in the stream.
     */
    public final long init(long start) {
	if (rows < 1)
	    return start;
	if (this.start == -1)
	    this.start = start;
	else throw new RuntimeException
		 ("Cannot re-initialize table \'" + getTableName() + "\'");
	rowSize = getRowSize();
	int size = rows * rowSize();
	buffer = this.newMapping ? file.mapBuffer(start, size)
	    : file.getBuffer(start, size);
	return start + size;
    }


    public final String getTableName() {
	return 0 <= id && id < MAX_NUMBER ? tableName[id] : "<NoTable>";
    }

    /**
     * @return the size of the row in bytes
     */
    public final int rowSize() {
	return rowSize;
    }

    /**
     * if the underlying buffer is memory-mapped, load its contents into memory
     */
    public void load() {
	if (buffer instanceof MappedByteBuffer)
	    ((MappedByteBuffer)buffer).load();
    }

    /***/
    public final int readByte() {
	return (buffer.get() + 0x100) & 0xff;
    }

    /***/
    public final int readShort() {
	return (buffer.getShort() + 0x10000) & 0xffff;
    }

    /***/
    public final int readInt() {
	return buffer.getInt();
    }

    /***/
    public final int readStringIndex() {
	return file.StringIsShort ? readShort() : readInt();
    }

    /***/
    public final int readBlobIndex() {
	return file.BlobIsShort ? readShort() : readInt();
    }

    /***/
    public final int readGUIDIndex() {
	return file.GUIDIsShort ? readShort() : readInt();
    }

    /***/
    public final int readTableIndex(int tableId) {
	return file.getTable(tableId).isShort ? readShort() : readInt();
    }

    /***/
    public final int readTableSetIndex(int tableSetId) {
	return file.indexSize[tableSetId] == 2 ? readShort() : readInt();
    }

    /** Read the specified row and populate the fields of the instance. */
    public final void readRow(int row) {
	seekRow(row);
	int lastSeek = buffer.position();
	populateFields();
	int rowSizeRead = (int) (buffer.position() - lastSeek);
	if (rowSizeRead != rowSize())
	    throw new RuntimeException("Table ID=0x" + PEFile.byte2hex(id) +
				       ": read row size = " + rowSizeRead +
				       "; expected row size = " + rowSize());
	currentRow = row;
    }

    /** Seeks in the file the position of the specified row. */
    protected final void seekRow(int row) {
	assert row > 0 && row <= rows
	    : "Index " + row + " is not within the table with #rows = " + rows;
	buffer.position((row - 1)* rowSize());
    }

    public final int currentRow() { return currentRow; }

    public final void nextRow() { readRow(currentRow() + 1); }

    //##########################################################################
    // abstract members

    /** Assigns values to the fields of the class. */
    protected abstract void populateFields();

    /** Returns the size of a row in bytes. */
    protected abstract int getRowSize();

    //##########################################################################
    // a table with 0 rows

    private static final class Empty extends Table {
	public Empty(int id) {
	    super(null, id, 0);
	}
	protected int getRowSize() { return 0; }
	protected void populateFields() {
	    throw new RuntimeException("Table 0x" + PEFile.byte2hex(id));
	}
    }

    //##########################################################################
    // table Module; ID=0x00; p115, 21.27

    public static final class ModuleDef extends Table {
	public static final int ID = 0x00;

	/** 2-byte value; reserved - shall be 0. */
	public int Generation;

	/** Index into #String. */
	public int Name;

	/** Index into #GUID; used to distinguish between
	 *  two version of the same module. */
	public int Mvid;

	/** Index into #GUID; reserved - shall be 0. */
	public int EncId;

	/** Index into #GUID; reseved - shall be 0. */
	public int EncBaseId;

	public ModuleDef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Generation = readShort();
	    Name = readStringIndex();
	    Mvid = readGUIDIndex();
	    EncId = readGUIDIndex();
	    EncBaseId = readGUIDIndex();
	}

	protected int getRowSize() {
	    return 2 + file.getStringIndexSize() + 3*file.getGUIDIndexSize();
	}

	public String getName() {
	    return file.getString(Name);
	}

    } // class ModuleDef

    //##########################################################################
    // table TypeRef; ID=0x01; p125, 21.35

    public static final class TypeRef extends Table {
	public static final int ID = 0x1;

	/** A ResolutionScope coded index. */
	public int ResolutionScope;

	/** Index into #String. */
	public int Name;

	/** Index into #String. */
	public int Namespace;

	public TypeRef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    ResolutionScope = readTableSetIndex(_ResolutionScope);
	    Name = readStringIndex();
	    Namespace = readStringIndex();
	}

	protected int getRowSize() {
	    return file.getTableSetIndexSize(_ResolutionScope) +
		2 * file.getStringIndexSize();
	}

	public String getFullName() {
	    String namespace = file.getString(Namespace);
	    return namespace.length() == 0 ? file.getString(Name)
		: namespace + "." + file.getString(Name);
	}

    } // class TypeRef

    //##########################################################################
    // table TypeDef; ID=0x02; p120, 21.34

    public static final class TypeDef extends Table {
	public static final int ID = 0x02;

	/** 4-byte bitmask of type TypeAttributes (22.1.14). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into #String. */
	public int Namespace;

	/** TypeDefOrRef coded index. */
	public int Extends;

	/** Index into Field table.
	 */
	public int FieldList;

	/** Index into Method table. */
	public int MethodList;


	public TypeDef(PEFile file, int rows) {
	    super(file, ID, rows);
	    this.newMapping = true;
	}

	public String getFullName() {
	    String namespace = file.getString(Namespace);
	    return namespace.length() == 0 ? file.getString(Name)
		: namespace + "." + file.getString(Name);
	}

	protected void populateFields() {
	    Flags = readInt();
	    Name = readStringIndex();
	    Namespace = readStringIndex();
	    Extends = readTableSetIndex(_TypeDefOrRef);
	    FieldList = readTableIndex(FieldDef.ID);
	    MethodList = readTableIndex(MethodDef.ID);
	}

	protected int getRowSize() {
	    return 4 + 2*file.getStringIndexSize() +
		file.getTableSetIndexSize(_TypeDefOrRef) +
		file.getTableIndexSize(FieldDef.ID) +
		file.getTableIndexSize(MethodDef.ID);
	}

    } // class TypeDef

    //##########################################################################
    // Table FieldTrans; ID=0x03; undocumented

    /**
     * Undocumented table. Appears to be used for translating the Field entry
     * in the TypeDef(0x02) table into the real entry in the Fields(0x06) table
     */
    public static final class FieldTrans extends Table {
	public static final int ID = 0x03;

	public int Field;

	public FieldTrans(PEFile file, int rows) {
	    super(file, ID, rows);
	    newMapping = true;
	}

	protected void populateFields() {
	    Field = readTableIndex(FieldDef.ID);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(FieldDef.ID);
	}

    }

    //##########################################################################
    // table Field; ID=0x04; p102, 21.15

    public static final class FieldDef extends Table {
	public static final int ID = 0x04;

	/** 2-byte bitmask of type FieldAttributes (22.1.5). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into #Blob. */
	public int Signature;

	public FieldDef(PEFile file, int rows) {
	    super(file, ID, rows);
	    newMapping = true;
	}

	protected void populateFields() {
	    Flags = readShort();
	    Name = readStringIndex();
	    Signature = readBlobIndex();
	}

	protected int getRowSize() {
	    return 2 + file.getStringIndexSize() + file.getBlobIndexSize();
	}

	public String getName() { return file.getString(Name); }

	public Sig getSignature() { return file.getSignature(Signature); }

    } //class FieldDef

    //##########################################################################
    // Table MethodTrans; ID=0x05; undocumented

    /**
     * Undocumented table. Appears to be used for translating the Method entry
     * in the TypeDef(0x02) table into the real entry in the Methods(0x06) table
     */
    public static final class MethodTrans extends Table {
	public static final int ID = 0x05;

	public int Method;

	public MethodTrans(PEFile file, int rows) {
	    super(file, ID, rows);
	    newMapping = true;
	}

	protected void populateFields() {
	    Method = readTableIndex(FieldDef.ID);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(MethodDef.ID);
	}

    }

    //##########################################################################
    // table MethodDef; ID=0x06; p110, 21.24

    public static final class MethodDef extends Table {
	public static final int ID = 0x06;

	/** 4-byte constant. */
	public int RVA;

	/** 2-byte bitmask of type MethodImplAttributes (22.1.10). */
	public int ImplFlags;

	/** 2-byte bitmask of type MethodAttributes (22.1.9). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into #Blob. */
	public int Signature;

	/** Index into Param Table. */
	public int ParamList;

	public MethodDef(PEFile file, int rows) {
	    super(file, ID, rows);
	    newMapping = true;
	}

	protected void populateFields() {
	    RVA = readInt();
	    ImplFlags = readShort();
	    Flags = readShort();
	    Name = readStringIndex();
	    Signature = readBlobIndex();
	    ParamList = readTableIndex(ParamDef.ID);
	}

	protected int getRowSize() {
	    return 8 + file.getStringIndexSize() + file.getBlobIndexSize() +
		file.getTableIndexSize(ParamDef.ID);
	}

	public String getName() { return file.getString(Name); }

	public Sig getSignature() { return file.getSignature(Signature); }
    } // class Method

    //##########################################################################
    // table Param; ID=0x08; p116, 21.30

    public static final class ParamDef extends Table {
	public static final int ID = 0x08;

	/** 2-byte bitmask of type ParamAttributes (22.1.12). */
	public int Flags;

	/** 2-byte constant. */
	public int Sequence;

	/** Index into #String. */
	public int Name;

	public ParamDef(PEFile file, int rows) {
	    super(file, ID, rows);
	    newMapping = true;
	}

	protected void populateFields() {
	    Flags = readShort();
	    Sequence = readShort();
	    Name = readStringIndex();
	}

	protected int getRowSize() { return 4 + file.getStringIndexSize(); }

	public String getName() { return file.getString(Name); }

    } // class Param

    //##########################################################################
    // table InterfaceImpl, ID=0x09; p107, 21.21

    public static final class InterfaceImpl extends Table {
	public static final int ID = 0x09;

	/** Index into TypeDef table. */
	public int Class;

	/** Index into TypeDefOrRef table set. */
	public int Interface;

	public InterfaceImpl(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Class = readTableIndex(TypeDef.ID);
	    Interface = readTableSetIndex(_TypeDefOrRef);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(TypeDef.ID) +
		file.getTableSetIndexSize(_TypeDefOrRef);
	}

	/** finds the index of the first entry
	 * @param targetIndex - index in the TypeDef table - the type to look for
	 * @return the index of the first interface for the given type;
	 *         0 if the type doesn't implement any interfaces
	 */

	// binary search implementation
// 	public int findType(int targetIndex) {
// 	    int l = 1, h = rows;
// 	    int classIndex;
// 	    while (l <= h) {
// 		int mid = (l + h) / 2;
// 		seekRow(mid);
// 		classIndex = readTableIndex(TypeDef.ID);
// 		if (targetIndex <= classIndex) h = mid - 1;
// 		else l = mid + 1;
// 	    }
// 	    return (targetIndex == classIndex) ? h : 0;
// 	}

	//linear search implementation
	public int findType(int targetIndex) {
	    for (int i = 1; i <= rows; i++) {
		seekRow(i);
		if (targetIndex == readTableIndex(TypeDef.ID))
		    return i;
	    }
	    return 0;
	}

    } // class InterfaceImpl

    //##########################################################################
    // table MemberRef; ID=0x0a; p109, 21.23

    public static final class MemberRef extends Table {
	public static final int ID = 0x0a;

	/** Index into MemberRefParent table set. */
	public int Class;

	/** Index into #String. */
	public int Name;

	/** Index into #Blob. */
	public int Signature;

	public MemberRef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Class = readTableSetIndex(_MemberRefParent);
	    Name = readStringIndex();
	    Signature = readBlobIndex();
	}

	protected int getRowSize() {
	    return file.getTableSetIndexSize(_MemberRefParent) +
		file.getStringIndexSize() + file.getBlobIndexSize();
	}

	public String getName() {
	    return file.getString(Name);
	}

	public Sig getSignature() {
	    return file.getSignature(Signature);
	}

    } // class MemberRef

    //##########################################################################
    // table Constant; ID=0x0b; p95, 21.9

    public static final class Constant extends Table {
	public static final int ID = 0x0b;

	/** 1-byte constant followed by 1-byte padding 0 (see 22.1.15). */
	public int Type;

	/** Index into HasConst table set. */
	public int Parent;

	/** Index into #Blob. */
	public int Value;

	public Constant(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Type = readShort();
	    Parent = readTableSetIndex(_HasConstant);
	    Value = readBlobIndex();
	}

	protected int getRowSize() {
	    return 2 + file.getTableSetIndexSize(_HasConstant) +
		file.getBlobIndexSize();
	}

	public Object getValue() {
	    if (Type == Signature.ELEMENT_TYPE_CLASS)
		return null;
	    return file.Blob.getConstant(Type, Value);
	}


    } // class Constant

    //##########################################################################
    // table CustomAttribute; ID=0x0c; p95, 21.10

    public static final class CustomAttribute extends Table {
	public static final int ID = 0x0c;

	/** Index into any metadata table, except the CustomAttribute itself;
	 *  more precisely - index into HasCustomAttribute table set.
	 */
	public int Parent;

	/** Index into the CustomAttributeType table set. */
	public int Type;

	/** Index into #Blob. */
	public int Value;

	public CustomAttribute(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Parent = readTableSetIndex(_HasCustomAttribute);
	    Type = readTableSetIndex(_CustomAttributeType);
	    Value = readBlobIndex();
	}

	protected int getRowSize() {
	    return file.getTableSetIndexSize(_HasCustomAttribute) +
		file.getTableSetIndexSize(_CustomAttributeType) +
		file.getBlobIndexSize();
	}

	public byte[] getValue() {
	    return Value == 0 ? null : file.getBlob(Value);
	}
    } // class CustomAttribute

    //##########################################################################
    // table FieldMarshal; ID=0x0d; p105, 21.17

    public static final class FieldMarshal extends Table {
	public static final int ID = 0x0d;

	/** Index into HasFieldMarshal table set. */
	public int Parent;

	/** Index into #Blob. */
	public int NativeType;

	public FieldMarshal(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Parent = readTableSetIndex(_HasFieldMarshal);
	    NativeType = readBlobIndex();
	}

	protected int getRowSize() {
	    return file.getTableSetIndexSize(_HasFieldMarshal) +
		file.getBlobIndexSize();
	}

    } // class FieldMarshal

    //##########################################################################
    // table DeclSecurity; ID=0x0e; p97, 21.11

    public static final class DeclSecurity extends Table {
	public static final int ID = 0x0e;

	/** 2-byte value. */
	public int Action;

	/** Index into HasDeclSecurity table set. */
	public int Parent;

	/** Index into #Blob. */
	public int PermissionSet;

	public DeclSecurity(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Action = readShort();
	    Parent = readTableSetIndex(_HasDeclSecurity);
	    PermissionSet = readBlobIndex();
	}

	protected int getRowSize() {
	    return 2 + file.getTableSetIndexSize(_HasDeclSecurity) +
		file.getBlobIndexSize();
	}

    } // class DeclSecurity

    //##########################################################################
    // table ClassLayout; ID=0x0f, p92, 21.8

    public static final class ClassLayout extends Table {
	public static final int ID = 0x0f;

	/** 2-byte constant. */
	public int PackingSize;

	/** 4-byte constant. */
	public int ClassSize;

	/** Index into TypeDef table. */
	public int Parent;

	public ClassLayout(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    PackingSize = readShort();
	    ClassSize = readInt();
	    Parent = readTableIndex(TypeDef.ID);
	}

	protected int getRowSize() {
	    return 6 + file.getTableIndexSize(TypeDef.ID);
	}

    } // class ClassLayout

    //##########################################################################
    // table FieldLayout; ID=0x10; p104, 21.16

    public static final class FieldLayout extends Table {
	public static final int ID = 0x10;

	/** 4-byte constant. */
	public int Offset;

	/** Index into the Field table. */
	public int Field;

	public FieldLayout(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Offset = readInt();
	    Field = readTableIndex(FieldDef.ID);
	}

	protected int getRowSize() {
	    return 4 + file.getTableIndexSize(FieldDef.ID);
	}

    } // class FieldLayout

    //##########################################################################
    // table StandAloneSig; ID=0x11; p119, 21.33

    public static final class StandAloneSig extends Table {
	public static final int ID = 0x11;

	/** Index into #Blob. */
	public int Signature;

	public StandAloneSig(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Signature = readBlobIndex();
	}

	protected int getRowSize() { return file.getBlobIndexSize(); }

    } // class StandAloneSig

    //##########################################################################
    // table EventMap; ID=0x12; p99, 21.12

    public static final class EventMap extends Table {
	public static final int ID = 0x12;

	/** Index into the TypeDef table. */
	public int Parent;

	/** Index into the Event table. */
	public int EventList;

	public EventMap(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Parent = readTableIndex(TypeDef.ID);
	    EventList = readTableIndex(EventDef.ID);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(TypeDef.ID) +
		file.getTableIndexSize(EventDef.ID);
	}

    } // class EventMap

    //##########################################################################
    // table Event; ID=0x14; p99, 21.13

    public static final class EventDef extends Table {
	public static final int ID = 0x14;

	/** 2-byte bitmask of type EventAttribute (22.1.4). */
	public int EventFlags;

	/** Index into #String. */
	public int Name;

	/** Index into TypeDefOrRef table set. [This corresponds to the Type
	 *  of the event; it is not the Type that owns the event]
	 */
	public int EventType;

	public EventDef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    EventFlags = readShort();
	    Name = readStringIndex();
	    EventType = readTableSetIndex(_TypeDefOrRef);
	}

	protected int getRowSize() {
	    return 2 + file.getStringIndexSize() +
		file.getTableSetIndexSize(_TypeDefOrRef);
	}

	public String getName() { return file.getString(Name); }

    } // class EventDef

    //##########################################################################
    // table PropertyMap; ID=0x15; p119, 21.32

    public static final class PropertyMap extends Table {
	public static final int ID = 0x15;

	/** Index into the TypeDef table. */
	public int Parent;

	/** Index into the Property table. */
	public int PropertyList;

	public PropertyMap(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Parent = readTableIndex(TypeDef.ID);
	    PropertyList = readTableIndex(PropertyDef.ID);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(TypeDef.ID) +
		file.getTableIndexSize(PropertyDef.ID);
	}

    } // class PropertyMap

    //##########################################################################
    // table Property; ID=0x17; p117, 21.31

    public static final class PropertyDef extends Table {
	public static final int ID = 0x17;

	/** 2-byte bitmask of type PropertyAttributes (22.1.13). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into #Blob. (Indexes the signature in the #Blob) */
	public int Type;

	public PropertyDef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Flags = readShort();
	    Name = readStringIndex();
	    Type = readBlobIndex();
	}

	protected int getRowSize() {
	    return 2 + file.getStringIndexSize() +
		file.getBlobIndexSize();
	}

	public String getName() { return file.getString(Name); }

	public Sig getSignature() { return file.getSignature(Type); }

    } // class PropertyDef

    //##########################################################################
    // table MethodSemantics; ID=0x18; p114, 21.26

    public static final class MethodSemantics extends Table {
	public static final int ID = 0x18;

	/** 2-byte bitmaks of type MethodSemanticsAttribute (22.1.11). */
	public int Semantics;

	/** Index into the Method table. */
	public int Method;

	/** Index into Event or Property table (HasSemantics table set). */
	public int Association;

	public MethodSemantics(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Semantics = readShort();
	    Method = readTableIndex(MethodDef.ID);
	    Association = readTableSetIndex(_HasSemantics);
	}

	protected int getRowSize() {
	    return 2 + file.getTableIndexSize(MethodDef.ID) +
		file.getTableSetIndexSize(_HasSemantics);
	}

        public boolean isGetter()   { return (Semantics & Getter)   != 0; }
        public boolean isSetter()   { return (Semantics & Setter)   != 0; }
        public boolean isOther()    { return (Semantics & Other)    != 0; }
        public boolean isAddOn()    { return (Semantics & AddOn)    != 0; }
        public boolean isRemoveOn() { return (Semantics & RemoveOn) != 0; }
        public boolean isFire()     { return (Semantics & Fire)     != 0; }

        private static final short Setter   = (short)0x0001;
        private static final short Getter   = (short)0x0002;
        private static final short Other    = (short)0x0004;
        private static final short AddOn    = (short)0x0008;
        private static final short RemoveOn = (short)0x0010;
        private static final short Fire     = (short)0x0020;

    } // class MethodSemantics


    //##########################################################################
    // table MethodImpl; ID=0x19; p113, 21.25

    public static final class MethodImpl extends Table {
	public static final int ID = 0x19;

	/** Index into the TypeDef table. */
	public int Class;

	/** Index into MethodDefOrRef table set. */
	public int MethodBody;

	/** Index into MethodDefOrRef table set. */
	public int MethodDeclaration;

	public MethodImpl(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Class = readTableIndex(TypeDef.ID);
	    MethodBody = readTableSetIndex(_MethodDefOrRef);
	    MethodDeclaration = readTableSetIndex(_MethodDefOrRef);
	}

	protected int getRowSize() {
	    return file.getTableIndexSize(TypeDef.ID) +
		2 * file.getTableSetIndexSize(_MethodDefOrRef);
	}

    } // class MethodImpl

    //##########################################################################
    // table ModuleRef; ID=0x1a; p116, 21.28

    public static final class ModuleRef extends Table {
	public static final int ID = 0x1a;

	/** Index into #String. */
	public int Name;

	public ModuleRef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Name = readStringIndex();
	}

	protected int getRowSize() { return file.getStringIndexSize(); }

	public String getName() { return file.getString(Name); }

    } // class ModuleRef

    //##########################################################################
    // table TypeSpec; ID=0x1b; p126, 21.36

    public static final class TypeSpec extends Table {
	public static final int ID = 0x1b;

	/** Index into #Blob, where the blob is formatted
	 *  as specified in 22.2.15
	 */
	public int Signature;

	public TypeSpec(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Signature = readBlobIndex();
	}

	protected int getRowSize() { return file.getBlobIndexSize(); }

	public Sig getSignature() { return file.getSignature(Signature); }
    } // class TypeSpec

    //##########################################################################
    // table ImplMap; ID=0x1c; p107, 21.20

    public static final class ImplMap extends Table {
	public static final int ID = 0x1c;

	/** 2-byte bitmask of type PInvokeAttributes (22.1.7). */
	public int MappingFlags;

	/** Index into MemberForwarded table set. */
	public int MemberForwarded;

	/** Index into #String. */
	public int ImportName;

	/** Index into the ModuleRef table. */
	public int ImportScope;

	public ImplMap(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    MappingFlags = readShort();
	    MemberForwarded = readTableSetIndex(_MemberForwarded);
	    ImportName = readStringIndex();
	    ImportScope = readTableIndex(ModuleRef.ID);
	}

	protected int getRowSize() {
	    return 2 + file.getTableSetIndexSize(_MemberForwarded) +
		file.getStringIndexSize() +
		file.getTableIndexSize(ModuleRef.ID);
	}

    } // class ImplMap

    //##########################################################################
    // table FieldRVA; ID=0x1d; p106, 21.18

    public static final class FieldRVA extends Table {
	public static final int ID = 0x1d;

	/** 4-byte constant. */
	public int RVA;

	/** Index into the Field table. */
	public int Field;

	public FieldRVA(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    RVA = readInt();
	    Field = readTableIndex(Table.FieldDef.ID);
	}

	protected int getRowSize() {
	    return 4 + file.getTableIndexSize(FieldDef.ID);
	}

    }

    //##########################################################################
    // table Assembly; ID=0x20; p90, 21.2

    public static final class AssemblyDef extends Table {
	public static final int ID = 0x20;

	/** 4-byte constatnt of type AssemblyHashAlgorithm, clause 22.1.1 */
	public int HashAlgId;

	/** 2-byte constant */
	public int MajorVersion;

	/** 2-byte constant */
	public int MinorVersion;

	/** 2-byte constant */
	public int BuildNumber;

	/** 2-byte constant */
	public int RevisionNumber;

	/** 4-byte constant */
	public int Flags;

	/** index into #Blob */
	public int PublicKey;

	/** index into #String */
	public int Name;

	/** index into #String */
	public int Culture;

	public AssemblyDef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    HashAlgId = readInt();
	    MajorVersion = readShort();
	    MinorVersion = readShort();
	    BuildNumber = readShort();
	    RevisionNumber = readShort();
	    Flags = readInt();
	    PublicKey = readBlobIndex();
	    Name = readStringIndex();
	    Culture = readStringIndex();
	}

	protected int getRowSize() {
	    return 16 + file.getBlobIndexSize() + 2*file.getStringIndexSize();
	}

    } // class AssemblyDef

    //##########################################################################
    // table AssemblyProcessor; ID=0x21; p91, 21.4

    public static final class AssemblyProcessor extends Table {
	public static final int ID = 0x21;

	/** 4-byte constant. */
	public int Processor;

	public AssemblyProcessor(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Processor = readInt();
	}

	protected int getRowSize() { return 4; }

    }

    //##########################################################################
    // table AssemblyOS; ID = 0x22; p90, 21.3

    public static final class AssemblyOS extends Table {
	public static final int ID = 0x22;

	/** 4-byte constant. */
	public int OSPlatformID;

	/** 4-byte constant. */
	public int OSMajorVersion;

	/** 4-byte constant. */
	public int OSMinorVersion;

	public AssemblyOS(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    OSPlatformID = readInt();
	    OSMajorVersion = readInt();
	    OSMinorVersion = readInt();
	}

	protected int getRowSize() { return 12; }

    }

    //##########################################################################
    // table AssemblyRef; ID = 0x23; pp91, 21.5

    public static final class AssemblyRef extends Table {
	public static final int ID = 0x23;

	/** 2-byte constant. */
	public int MajorVersion;

	/** 2-byte constant. */
	public int MinorVersion;

	/** 2-byte constant. */
	public int BuildNumber;

	/** 2-byte constant. */
	public int RevisionNumber;

	/** 4-byte bitmask of type AssemblyFlags (22.1.2). */
	public int Flags;

	/** index into #Blob. */
	public int PublicKeyOrToken;

	/** index into #String. */
	public int Name;

	/** index into #String. */
	public int Culture;

	/** index into #Blob. */
	public int HashValue;

	public AssemblyRef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    MajorVersion = readShort();
	    MinorVersion = readShort();
	    BuildNumber = readShort();
	    RevisionNumber = readShort();
	    Flags = readInt();
	    PublicKeyOrToken = readBlobIndex();
	    Name = readStringIndex();
	    Culture = readStringIndex();
	    HashValue = readBlobIndex();
	}

	protected int getRowSize() {
	    return 12 + 2*file.getBlobIndexSize() + 2*file.getStringIndexSize();
	}

	public String getName() { return file.getString(Name); }
    }

    //##########################################################################
    // table AssemblyRefProcessor; ID=0x24; p92, 21.7

    public static final class AssemblyRefProcessor extends Table {
	public static final int ID = 0x24;

	/** 4-byte constant. */
	public int Processor;

	/** Index into the AssemblyRef table. */
	public int AssemblyRef;

	public AssemblyRefProcessor(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Processor = readInt();
	    AssemblyRef = readTableIndex(Table.AssemblyRef.ID);
	}

	protected int getRowSize() {
	    return 4 + file.getTableIndexSize(Table.AssemblyRef.ID);
	}

    } // class AssemblyRefProcessor

    //##########################################################################
    // table AssemblyRefOS; ID=0x25; p92, 21.6

    public static final class AssemblyRefOS extends Table {
	public static final int ID = 0x25;

	/** 4-byte constant. */
	public int OSPlatformId;

	/** 4-byte constant. */
	public int OSMajorVersion;

	/** 4-byte constant. */
	public int OSMinorVersion;

	/** Index into the AssemblyRef table. */
	public int AssemblyRef;

	public AssemblyRefOS(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    OSPlatformId = readInt();
	    OSMajorVersion = readInt();
	    OSMinorVersion = readInt();
	    AssemblyRef = readTableIndex(Table.AssemblyRef.ID);
	}

	protected int getRowSize() {
	    return 12 + file.getTableIndexSize(Table.AssemblyRef.ID);
	}

    } // class AssemblyRefOS

    //##########################################################################
    // table File; ID=0x26; p106, 21.19

    public static final class FileDef extends Table {
	public static final int ID = 0x26;

	/** 4-byte bitmask of type FileAttributes (22.1.6). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into #Blob. */
	public int HashValue;

	public FileDef(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Flags = readInt();
	    Name = readStringIndex();
	    HashValue = readBlobIndex();
	}

	protected int getRowSize() {
	    return 4 + file.getStringIndexSize() + file.getBlobIndexSize();
	}

	public String getName() {
	    return file.getString(Name);
	}

    } // class FileDef

    //##########################################################################
    // table ExportedType; ID=0x27; p100, 21.14

    public static final class ExportedType extends Table {
	public static final int ID = 0x27;

	/** 4-byte bitmask of type TypeAttribute (22.1.6). */
	public int Flags;

	/** 4-byte index into a TypeDef table of
	 *  another module in this assembly.
	 */
	public int TypeDefId;

	/** Index into #String. */
	public int TypeName;

	/** Index into #Stream. */
	public int TypeNamespace;

	/** Index into one of two tables as follows:
	 *   - 'File' table, where that entry says which module
	 *     in the current assembly holds the TypeDef
	 *   - 'ExportedType' table, where that entry is
	 *     the enclosing Type of the current nested Type
	 */
	public int Implementation;

	public ExportedType(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Flags = readInt();
	    TypeDefId = readInt();
	    TypeName = readStringIndex();
	    TypeNamespace = readStringIndex();
	    Implementation = readTableSetIndex(_Implementation);
	}

	protected int getRowSize() {
	    return 8 + 2*file.getStringIndexSize() +
		file.getTableSetIndexSize(_Implementation);
	}

	public String getFullName() {
	    String namespace = file.getString(TypeNamespace);
	    return namespace.length() == 0 ? file.getString(TypeName)
		: namespace + "." + file.getString(TypeName);
	}

    } // class ExportedType

    //##########################################################################
    // table ManifestResource; ID=0x28; p108, 21.22

    public static final class ManifestResource extends Table {
	public static final int ID = 0x28;

	/** 4-byte constant. */
	public int Offset;

	/** 4-byte bitmask of type ManifestResourceAttributes (22.1.8). */
	public int Flags;

	/** Index into #String. */
	public int Name;

	/** Index into the Implementation table set. */
	public int Implementation;

	public ManifestResource(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    Offset = readInt();
	    Flags = readInt();
	    Name = readStringIndex();
	    Implementation = readTableSetIndex(_Implementation);
	}

	protected int getRowSize() {
	    return  8 + file.getStringIndexSize() +
		file.getTableSetIndexSize(_Implementation);
	}

    } // class ManifestResource

    //##########################################################################
    // table NestedClass; ID=0x29; p116, 21.29

    public static final class NestedClass extends Table {
	public static final int ID = 0x29;

	/** Index into the TypeDef table. */
	public int NestedClass;

	/** Index into the TypeDef table. */
	public int EnclosingClass;

	public NestedClass(PEFile file, int rows) { super(file, ID, rows); }

	protected void populateFields() {
	    NestedClass = readTableIndex(TypeDef.ID);
	    EnclosingClass = readTableIndex(TypeDef.ID);
	}

	protected int getRowSize() {
	    return 2 * file.getTableIndexSize(TypeDef.ID);
	}

    } // class NestedClass

    //##########################################################################
    // table GenericParam; ID=0x2a; p137, 22.20

    public static final class GenericParam extends Table {
        public static final int ID = 0x2a;

        public int Number;
        public int Flags;
        public int Owner;  // a TypeOrMethodDef (§24.2.6) coded index
        public int Name; // a non-null index into the String heap

        private java.util.Map /*<Integer, java.util.Set<Integer>>*/ GenericParamIdxesForMethodDefIdx =
                new java.util.HashMap();
        private java.util.Map /*<Integer, java.util.Set<Integer>>*/ GenericParamIdxesForTypeDefIdx  =
                new java.util.HashMap();

        private void addToMap(int key, int value, java.util.Map IdxesForIdx) {
            java.util.Set /*<Integer>*/ bucket = (java.util.Set)IdxesForIdx.get(Integer.valueOf(key));
            if(bucket == null) {
                bucket = new java.util.HashSet();
                IdxesForIdx.put(Integer.valueOf(key), bucket);
            }
            bucket.add(Integer.valueOf(value));
        }

        /** Indexes of rows in the GenericParam table representing type parameters defined by the type given by
         * its row index TypeDefIdx (in the TypeDef table).
         * No need to position the current record before invoking this method.  */
        public int[] getTVarIdxes(int TypeDefIdx) {
            if(!mapsPopulated) {
                initMaps();
            }
            java.util.Set bucket = (java.util.Set)GenericParamIdxesForTypeDefIdx.get(Integer.valueOf(TypeDefIdx));
            if(bucket == null) {
                bucket = java.util.Collections.EMPTY_SET;
            }
            int[] res = new int[bucket.size()];
            java.util.Iterator /*<Integer>*/ it = bucket.iterator();
            for(int i = 0; i < bucket.size(); i++) {
                res[i] = ((Integer)it.next()).intValue();
            }
            return res;
        }

        /** Indexes of rows in the GenericParam table representing type parameters defined by the method given by
         * its row index MethodDefIdx (in the MethodDef table)
         * No need to position the current record before invoking this method.  */
        public int[] getMVarIdxes(int MethodDefIdx) {
            if(!mapsPopulated) {
                initMaps();
            }
            java.util.Set bucket = (java.util.Set)GenericParamIdxesForMethodDefIdx.get(Integer.valueOf(MethodDefIdx));
            if(bucket == null) {
                bucket = java.util.Collections.EMPTY_SET;
            }
            int[] res = new int[bucket.size()];
            java.util.Iterator /*<Integer>*/ it = bucket.iterator();
            for(int i = 0; i < bucket.size(); i++) {
                res[i] = ((Integer)it.next()).intValue();
            }
            return res;
        }

        private boolean mapsPopulated = false;

        private void initMaps() {
            mapsPopulated = true;
            for (int currentParamRow = 1; currentParamRow <= rows; currentParamRow++) {
                int currentOwner = file.GenericParam(currentParamRow).Owner;
                int targetTableId = Table.getTableId(Table._TypeOrMethodDef, currentOwner);
                int targetRow = currentOwner >> Table.NoBits[Table._TypeOrMethodDef];
                if(targetTableId == TypeDef.ID){
                    addToMap(targetRow, currentParamRow, GenericParamIdxesForTypeDefIdx);
                } else if(targetTableId == MethodDef.ID) {
                    addToMap(targetRow, currentParamRow, GenericParamIdxesForMethodDefIdx);
                } else {
                    throw new RuntimeException();
                }
            }
        }

        public GenericParam(PEFile file, int rows) {
            super(file, ID, rows);
            this.newMapping = true;
        }

        protected void populateFields() {
            Number = readShort();
            Flags = readShort();
            Owner = readTableSetIndex(_TypeOrMethodDef);
            Name = readStringIndex();
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean isInvariant() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x0003) == 0;
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean isCovariant() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x0003) == 1;
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean isContravariant() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x0003) == 2;
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean isReferenceType() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x001C) == 4;
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean isValueType() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x001C) == 8;
        }

        /** This method assumes populateFields() has been just called to set Flags for the current record */
        public boolean hasDefaultConstructor() {
            /* 23.1.7 Flags for Generic Parameters [GenericParamAttributes tributes] */
            return (Flags & 0x001C) == 0x0010;
        }

        protected int getRowSize() {
            return 2 + 2 + file.getTableSetIndexSize(_TypeOrMethodDef) + file.getStringIndexSize();
            /* Columns:
                 Number (2 bytes),
                 Flags (2 bytes),
                 Owner (coded token of type TypeOrMethodDef),
                 Name (offset in the #Strings stream).
            */
        }

        public String getName() {
            return file.getString(Name);
        }

    } // class GenericParam


    //##########################################################################
    // table GenericParamConstraint; ID=0x2c; p139, 22.20

    public static final class GenericParamConstraint extends Table {
        public static final int ID = 0x2c;

        public int Owner; // an index into the GenericParam table
        public int Constraint; // a TypeDefOrRef (§24.2.6) coded index

        public GenericParamConstraint(PEFile file, int rows) {
            super(file, ID, rows);
            this.newMapping = true;
        }

        protected void populateFields() {
            Owner = readTableIndex(GenericParam.ID);
            Constraint = readTableSetIndex(_TypeDefOrRef);
        }

        protected int getRowSize() {
            return file.getTableIndexSize(GenericParam.ID) + file.getTableSetIndexSize(_TypeDefOrRef);
            /* Columns:
                 Owner (RID in the GenericParam table),
                 Constraint (coded token of type TypeDefOrRef).
            */
        }

        private boolean mapPopulated = false;

        /** Indexes of rows (in the TypeDef, TypeRef, or TypeSpec tables) denoting the base class (if any)
         * and interfaces (if any) that the generic parameter (of TVar or MVar kind) should support,  where
         * that generic parameter is represented by its index into the GenericParam table. */
        public int[] getTypeDefOrRefIdxes(int genParamIdx) {
            if(!mapPopulated) {
                initMap();
            }
            java.util.Set bucket = (java.util.Set)TypeDefOrRefIdxesForGenParamIdx.get(Integer.valueOf(genParamIdx));
            if(bucket == null) {
                bucket = java.util.Collections.EMPTY_SET;
            }
            int[] res = new int[bucket.size()];
            java.util.Iterator /*<Integer>*/ it = bucket.iterator();
            for(int i = 0; i < bucket.size(); i++) {
                res[i] = ((Integer)it.next()).intValue();
            }
            return res;
        }


        private void initMap() {
            mapPopulated = true;
            for (int currentConstraintRow = 1; currentConstraintRow <= rows; currentConstraintRow++) {
                int targetGenericParam = file.GenericParamConstraint(currentConstraintRow).Owner;
                int value = file.GenericParamConstraint.Constraint;
                addToMap(targetGenericParam, value);
            }
        }

        private java.util.Map /*<Integer, java.util.Set<Integer>>*/ TypeDefOrRefIdxesForGenParamIdx  =
                new java.util.HashMap();

        private void addToMap(int key, int value) {
            java.util.Set /*<Integer>*/ bucket = (java.util.Set)TypeDefOrRefIdxesForGenParamIdx.get(Integer.valueOf(key));
            if(bucket == null) {
                bucket = new java.util.HashSet();
                TypeDefOrRefIdxesForGenParamIdx.put(Integer.valueOf(key), bucket);
            }
            bucket.add(Integer.valueOf(value));
        }

    } // class GenericParamConstraint

    //##########################################################################
    // table MethodSpec; ID=0x2b; p149, in Sec. 22.29 of Partition II

    public static final class MethodSpec extends Table {
        public static final int ID = 0x2b;

        /* an index into the MethodDef or MemberRef table, specifying which generic method this row is an instantiation of.
           A MethodDefOrRef (Sec. 24.2.6) coded index  */
        public int Method;

        /* an index into the Blob heap (Sec. 23.2.15), holding the signature of this instantiation */
        public int Instantiation;

        public MethodSpec(PEFile file, int rows) {
            super(file, ID, rows);
            this.newMapping = true;
        }

        protected void populateFields() {
            Method = readTableSetIndex(_MethodDefOrRef);
            Instantiation = readBlobIndex();
        }

        protected int getRowSize() {
            return file.getTableSetIndexSize(_MethodDefOrRef) + file.getBlobIndexSize();
        }


    } // class MethodSpec
    //##########################################################################

}  // class Table
