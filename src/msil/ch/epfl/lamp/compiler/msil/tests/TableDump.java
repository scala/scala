// $Id$

package ch.epfl.lamp.compiler.msil.tests;

import ch.epfl.lamp.compiler.msil.PEFile;
import ch.epfl.lamp.compiler.msil.util.Table;
import ch.epfl.lamp.compiler.msil.util.Table.*;

import java.io.PrintStream;
import java.io.FileNotFoundException;

public class TableDump extends PEFile {

    //##########################################################################

    public TableDump(String filename) throws FileNotFoundException {
	super(filename);
    }

    /***/
    public void dump(PrintStream out) {
	out.println("CLI RVA: " + CLI_RVA);
	out.println("Optional header size: " + optHeaderSize);
	out.println("Number of sections: " + numOfSections);
	out.println();

	for (int i = 0; i < sections.length; i++) {
	    sections[i].dump(out);
	    out.println();
	}

	out.println("MetaData Offset:   0x" + Integer.toHexString(posMetadata));
	out.println("Number of streams: " + numOfStreams);

	out.println("#~ stream");       Meta.dump(out);    out.println();
	out.println("#Strings stream"); Strings.dump(out); out.println();
	if (US != null) {
	    out.println("#US stream");      US.dump(out);      out.println();
	}
	out.println("#GUID stream");    GUID.dump(out);    out.println();
	out.println("#Blob stream");    Blob.dump(out);    out.println();

	out.println("Heap Sizes IndexedSeq = 0x0" + Integer.toHexString(heapSizes));
	out.println();

 	for(int i = 0; i < Table.MAX_NUMBER; i++)
 	    if(getTable(i).rows > 0) {
 		dump(out, getTable(i));
 		out.println();
 	    }

    }

    /** Dumps the contents of this table. */
    public void dump(PrintStream out, Table table) {
	out.println("Table:" + "  ID = 0x" + byte2hex(table.id));
	out.println("\tname = " + table.getTableName());
	out.println("\trows =  " + table.rows);
	//out.println("\tStart pos in file = 0x" + Long.toHexString(table.start));
 	for (int i = 1; i <= table.rows; i++)
 	    dumpRow(out, table, i);
    }

    public void dumpIndex(PrintStream out, int tableSetId, int index) {
	int tableId = Table.getTableId(tableSetId, index);
	int row =  Table.getTableIndex(tableSetId, index);
	out.print(getTable(tableId).getTableName());
        out.print('[');
	out.print(getTable(tableId).isShort ? short2hex(row) : int2hex(row));
        out.print(']');
    }

    public void dumpRow(PrintStream out, Table table, int row) {
	table.readRow(row);
	out.print(table.getTableName());
	out.print("[" + short2hex(row) + "]: ");
	dumpRow(out, table);
	out.println();
    }

    /** Prints the current content of the fields of the class. */
    public void dumpRow(PrintStream out, Table table) {
	if (table instanceof ModuleDef) {
	    ModuleDef t = (ModuleDef)table;
	    out.print("Generation = 0x" + short2hex(t.Generation));
	    out.print("; Name = " + getString(t.Name));
	    //out.print("; Mvid = (" + bytes2hex(getGUID(Mvid)) + ")");
	} else if (table instanceof TypeRef) {
	    TypeRef t = (TypeRef)table;
	    out.print("FullName = " + t.getFullName());
	    out.print("; ResolutionScope = 0x" + int2hex(t.ResolutionScope));
	} else if (table instanceof TypeDef) {
	    TypeDef t = (TypeDef)table;
	    out.print("Flags = 0x"); out.print(int2hex(t.Flags));
	    out.print("; FullName = "); out.print(t.getFullName());
	    out.print("; Extends = ");
	    dumpIndex(out, Table._TypeDefOrRef, t.Extends);
	    out.print("; FieldList = "); out.print(t.FieldList);
	    out.print("; MethodList = "); out.print(t.MethodList);
	} else if (table instanceof FieldTrans) {
	    FieldTrans t = (FieldTrans)table;
	    out.print("Field = "); out.print(t.Field);
	} else if (table instanceof FieldDef) {
	    FieldDef t = (FieldDef)table;
	    out.print("Flags = 0x" + short2hex(t.Flags));
	    out.print("; Name = " + t.getName());
	    out.print("; Signature = (" +
		      bytes2hex(getBlob(t.Signature)) + ")");
	} else if (table instanceof MethodTrans) {
	    MethodTrans t = (MethodTrans)table;
	    out.print("Method = "); out.print(t.Method);
	} else if (table instanceof MethodDef) {
	    MethodDef t = (MethodDef)table;
	    out.print("Flags = 0x" + short2hex(t.Flags));
	    out.print("; Name = " + t.getName());
	    out.print("; ParamList = " + t.ParamList);
	    out.print("; Signature = (" +
		      bytes2hex(getBlob(t.Signature)) + ")");
	} else if (table instanceof ParamDef) {
	    ParamDef t = (ParamDef)table;
	    out.print("Flags = 0x" + short2hex(t.Flags));
	    out.print("; Name = " + t.getName());
	    out.print("; Sequence = " + t.Sequence);
	} else if (table instanceof InterfaceImpl) {
	    InterfaceImpl t = (InterfaceImpl)table;
	    out.print("Class = 0x" + short2hex(t.Class));// + " (ref to: ");
	    //TypeDef td = (TypeDef) getTable(TypeDef.ID);
	    //td.readRow(Class);
	    //td.dumpRow(out);
	    out.print("; Interface = 0x" + short2hex(t.Interface));
	} else if (table instanceof MemberRef) {
	    MemberRef t = (MemberRef)table;
	    out.print("Name = " + t.getName());
	    out.print("; Signature = (" +
		      bytes2hex(getBlob(t.Signature)) + ")");
	    out.print("; Class = " + t.Class);
	} else if (table instanceof Constant) {
	    Constant t = (Constant)table;
	    out.print("Parent = "); dumpIndex(out, Table._HasConstant, t.Parent);
	    out.print("; Type = 0x" + byte2hex(t.Type));
 	    out.print("; Value = (" + bytes2hex(getBlob(t.Value)));
	    out.print("); Value = " + t.getValue());
	} else if (table instanceof CustomAttribute) {
	    CustomAttribute t = (CustomAttribute)table;
	    //out.print("Parent = 0x" + int2hex(t.Parent));
	    out.print("Parent = ");
            dumpIndex(out, Table._HasCustomAttribute, t.Parent);
	    //out.print("; Type = 0x" + short2hex(t.Type));
	    out.print("; Type = ");
            dumpIndex(out, Table._CustomAttributeType, t.Type);
	    out.print("; Value = (" + bytes2hex(t.getValue()) + ")");
	} else if (table instanceof FieldMarshal) {
	    FieldMarshal t = (FieldMarshal)table;
	    out.print("NativeType = (");
	    out.print(bytes2hex(getBlob(t.NativeType)) + ")");
	} else if (table instanceof DeclSecurity) {
	    DeclSecurity t = (DeclSecurity)table;
	    out.print("Action = 0x" + short2hex(t.Action));
	    out.print("; PermissionSet = (" +
		      bytes2hex(getBlob(t.PermissionSet)) + ")");
	} else if (table instanceof ClassLayout) {
	    ClassLayout t = (ClassLayout)table;
	    out.print("PackingSize = 0x" + short2hex(t.PackingSize));
	    out.print("; ClassSize = 0x" + int2hex(t.ClassSize));
	    out.print(": Parent = " + t.Parent + " (ref to: ");
	    dumpRow(out, this.TypeDef(t.Parent));
	    out.print(")");
	} else if (table instanceof FieldLayout) {
	    FieldLayout t = (FieldLayout)table;
	    out.print("Offset = 0x" + int2hex(t.Offset));
	    out.print("; Field = (ref to: ");
	    dumpRow(out, this.FieldDef(t.Field));
	    out.print(")");
	} else if (table instanceof StandAloneSig) {
	    StandAloneSig t = (StandAloneSig)table;
	    out.print("StandAloneSig: Signature = (" +
		      bytes2hex(getBlob(t.Signature)) + ")");
	} else if (table instanceof EventMap) {
	    EventMap t = (EventMap)table;
	    out.print("Parent = 0x" + int2hex(t.Parent) + " (ref to: ");
	    dumpRow(out, this.TypeDef(t.Parent));
	    out.print("); EventList = 0x"); out.print(int2hex(t.EventList));
	} else if (table instanceof EventDef) {
	    EventDef t = (EventDef)table;
	    out.print("EventFlags = 0x" + short2hex(t.EventFlags));
	    out.print("; Name = " + t.getName());
            out.print("; EventType = 0x" + int2hex(t.EventType));
	} else if (table instanceof PropertyMap) {
	    PropertyMap t = (PropertyMap)table;
	    out.print("Parent = " + t.Parent + " (ref to: ");
	    dumpRow(out, this.TypeDef(t.Parent));
	    out.print(")");
	} else if (table instanceof PropertyDef) {
	    PropertyDef t = (PropertyDef)table;
	    out.print("Flags = 0x" + short2hex(t.Flags));
	    out.print("; Name = " + t.getName());
	    out.print("; Type = (" + bytes2hex(getBlob(t.Type)) + ")");
	} else if (table instanceof MethodSemantics) {
	    MethodSemantics t = (MethodSemantics)table;
	    out.print("Semantics = 0x" + short2hex(t.Semantics));
	    out.print("; Method = 0x" + int2hex(t.Method) + " (ref to: ");
	    dumpRow(out, this.MethodDef(t.Method));
	    out.print("); Association = 0x" + int2hex(t.Association));
	} else if (table instanceof MethodImpl) {
	    MethodImpl t = (MethodImpl)table;
	    out.print("Class = (ref to: ");
	    dumpRow(out, this.TypeDef(t.Class));
	    out.print(")");
	} else if (table instanceof ModuleRef) {
	    ModuleRef t = (ModuleRef)table;
	    out.print("Name = " + t.getName());
	} else if (table instanceof TypeSpec) {
	    TypeSpec t = (TypeSpec)table;
	    out.print("Signature = (" +
		      bytes2hex(getBlob(t.Signature)) + ")");
	} else if (table instanceof ImplMap) {
	    ImplMap t = (ImplMap)table;
	    out.print("ImportName = " + getString(t.ImportName));
	} else if (table instanceof FieldRVA) {
	    FieldRVA t = (FieldRVA)table;
	    out.print("RVA = 0x" + int2hex(t.RVA));
	    out.print("; Field = (ref to: ");
	    dumpRow(out, this.FieldDef(t.Field));
	    out.print(")");
	} else if (table instanceof AssemblyDef) {
	    AssemblyDef t = (AssemblyDef)table;
	    out.print("Flags = 0x" + int2hex(t.Flags));
	    out.print(" ; Name = " + getString(t.Name));
	    out.print("; Culture = " + getString(t.Culture));
	    out.print(" ; Version = " + t.MajorVersion + ".");
	    out.print(t.MinorVersion + "." + t.BuildNumber);
	    out.print("." + t.RevisionNumber);
	    out.print("; HashAlgId = 0x" + int2hex(t.HashAlgId));
	    out.print("; PublicKey = (");
	    out.print(bytes2hex(getBlob(t.PublicKey)) + ")");
	} else if (table instanceof AssemblyProcessor) {
	    AssemblyProcessor t = (AssemblyProcessor)table;
	    out.print("Processor = 0x" + int2hex(t.Processor));
	} else if (table instanceof AssemblyOS) {
	    AssemblyOS t = (AssemblyOS)table;
	    out.print("!?!");
	} else if (table instanceof AssemblyRef) {
	    AssemblyRef t = (AssemblyRef)table;
	    out.print("Flags = 0x" + int2hex(t.Flags));
	    out.print("; Name = " + getString(t.Name));
	    out.print("; Culture = " + getString(t.Culture));
	    out.print("; Version = " + t.MajorVersion + "." + t.MinorVersion);
	    out.print("." + t.BuildNumber + "." + t.RevisionNumber);
	    out.print("; PublicKeyOrToken = (" +
		      bytes2hex(getBlob(t.PublicKeyOrToken)) + ")");
	    out.print("; HashValue = (" +
		      bytes2hex(getBlob(t.HashValue)) + ")");
	} else if (table instanceof AssemblyRefProcessor) {
	    AssemblyRefProcessor t = (AssemblyRefProcessor)table;
	    out.print("!?!");
	} else if (table instanceof AssemblyRefOS) {
	    AssemblyRefOS t = (AssemblyRefOS)table;
	    out.print("!?!");
	} else if (table instanceof FileDef) {
	    FileDef t = (FileDef)table;
	    out.print("Flags = 0x" + int2hex(t.Flags));
	    out.print("; Name = " + t.getName());
	    out.print("; HashValue = (" + bytes2hex(getBlob(t.HashValue)) +")");
	} else if (table instanceof ExportedType) {
	    ExportedType t = (ExportedType)table;
	    out.print("FullName = " + t.getFullName());
	} else if (table instanceof ManifestResource) {
	    ManifestResource t = (ManifestResource)table;
	    out.print("Name = " + getString(t.Name));
	    out.print("; Flags = 0x" + int2hex(t.Flags));
	} else if (table instanceof NestedClass) {
	    NestedClass t = (NestedClass)table;
	    out.print(this.TypeDef(t.EnclosingClass).getFullName());
	    out.print("/");
	    out.print(this.TypeDef(t.NestedClass).getFullName());
	} else
	    throw new RuntimeException("Unknown table " + table.getClass());
    }

    //##########################################################################

    public static void main(String[] args) {
	if (args.length < 1) {
	    System.err.println("You must supply a filename!");
	    System.exit(1);
	}

	TableDump file = null;
	try {
	    file = new TableDump(args[0]);
	} catch (FileNotFoundException e) { e.printStackTrace(); }

	if (args.length > 1) {
            nextarg:
	    for (int i = 1; i < args.length; i++) {
		String name = args[i];
		for (int tableId = 0; tableId < Table.MAX_NUMBER; tableId++) {
		    Table table = file.getTable(tableId);
		    if ((table.rows > 0) && name.equals(table.getTableName())) {
			file.dump(System.out, table);
			System.out.println();
			continue nextarg;
		    }
		}
                System.err.println("No such table: " + name);
	    }
	} else
	    file.dump(System.out);
    }

    //##########################################################################
}
