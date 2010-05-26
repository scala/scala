/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.PEFile;
import ch.epfl.lamp.compiler.msil.PEFile.Sig;
import ch.epfl.lamp.compiler.msil.util.Table;
import ch.epfl.lamp.compiler.msil.util.Table.*;

import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;

/** Represents a module corresponding to a PE/COFF file
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEModule extends Module {

    //##########################################################################

    protected final PEFile pefile;

    private final int definingRow;

    private Type[] typeRefs = null;

    protected PEModule(PEFile pefile, int definingRow, String scopeName,
		       Assembly assem)
    {
	super(pefile.getName(), pefile.getAbsolutePath(), scopeName, assem);
	this.pefile = pefile;
        this.definingRow = definingRow;
	pefile.initModule(this);
	pefile.TypeDef.load(); // load into memory
	//loadTypes();
	//pefile.FieldDef.load();
	//pefile.MethodDef.load();
	loadGlobals();
    }

    //##########################################################################

    public Type GetType(String typeName) {
        initTypes();
	Object o = typesMap.get(typeName);
	if (o == null) {
	    //System.out.println("PEModule.GetType(): Unable to find type "
	    //                   + typeName + " int module " + this);
	    return null;
	}
	return o instanceof Type ? (Type)o
	    : getTypeDef(((Integer)o).intValue());
    }


    /** Load information about the types defined in this module.
     */
    protected void loadTypes() {
	typeRefs = new Type[pefile.TypeRef.rows];
	final int nbTypes = pefile.TypeDef.rows;
	for (int row = 2; row <= nbTypes; row++) {
	    String name = pefile.TypeDef(row).getFullName();
	    typesMap.put(name, new Integer(row));
	}
	this.types = new Type[nbTypes - 1];
	for (int row = 2; row <= nbTypes; row++) {
	    getTypeDef(row);
	}
    }

    /** Return the type defined at the given row in the TypeDef table.
     */
    Type getTypeDef(int row) {
	if (this.types[row - 2] != null)
	    return this.types[row - 2];

	TypeDef type = pefile.TypeDef(row);
	int attrs = type.Flags;
	String name = type.getFullName();

	Type declType = null;
	if (TypeAttributes.isNested(attrs)) {
	    for (int i = 1; i <= pefile.NestedClass.rows; i++) {
		pefile.NestedClass.readRow(i);
		if (pefile.NestedClass.NestedClass == row)
		    declType = getTypeDef
			(pefile.NestedClass.EnclosingClass);
	    }
	}
	Type t = new PEType
            (this, attrs, name, declType, Type.AuxAttr.None, pefile, row);
	types[row - 2] = t;
	addType(t);
	return t;
    }

    /**
     * Load the desription of the module-global fields and methods
     */
    protected void loadGlobals() {
	//TODO:
    }

    protected void loadCustomAttributes(Type attributeType) {
        initAttributes(this, 1, Table.ModuleDef.ID, attributeType);
    }

    /** Return the type referenced by the given row in the TypeRef table.
     */
    Type getTypeRef(int row) {
        return getTypeRef(row, null);
    }

    /** Return the type referenced by the given row in the TypeRef table
     *  only if it resides in the given assembly.
     *  <i>Used by initCustomAttributes to avoid unnecessary loading
     *  of referenced assemblies.</i>
     */
    Type getTypeRef(int row, Assembly inAssembly) {
	Type type = typeRefs[row - 1];
	if (type != null)
	    return type;

	Table.TypeRef tr = pefile.TypeRef;
	tr.readRow(row);
	int tableId = Table.getTableId(Table._ResolutionScope,
				       tr.ResolutionScope);
	int refRow = tr.ResolutionScope >> Table.NoBits[Table._ResolutionScope];
	String typeName = tr.getFullName();
	pefile.getTable(tableId).readRow(refRow);
	switch (tableId) {
	case AssemblyRef.ID:
	    String name = pefile.AssemblyRef.getName();
            if (inAssembly != null && !inAssembly.GetName().Name.equals(name))
                return null;
            Assembly assem = getAssembly(name);
	    type = assem.GetType(typeName);
	    if (type == null) {
                // HACK: the IKVM.OpenJDK.Core assembly is compiled against mscorlib.dll v2.0
                // The MSIL library cannot parse the v2.0 mscorlib because of generics, so we
                // use the v1.0
                // However, the java.io.FileDescriptor.FlushFileBuffers method uses a type
                // Microsoft.Win32.SafeHandles.SafeFileHandle, which only exists in mscorlib
                // v2.0
                // For now, jsut return Object (fine as long as we don't use that method).
                Assembly asmb = getAssembly("mscorlib");
                type = asmb.GetType("System.Object");
		//throw new RuntimeException("Failed to locate type " +
                                           //typeName + " in assembly " + assem);
	    }
	    break;
	case ModuleDef.ID:
	    assert refRow == 1;
	    type = this.GetType(typeName);
	    //assert type != null;
	    break;
	case TypeRef.ID:
	    type = getTypeRef(refRow);
	    break;
	case ModuleRef.ID:
            type = getAssembly(pefile.ModuleRef.getName()).GetType(typeName);
	default:
	    throw new RuntimeException(refRow + "@" + pefile.getTable(tableId).getTableName()/* PEFile.byte2hex(tableId)*/);
	}
	if (typeRefs[row - 1] != null)
	    System.out.println("TypeRef[" + PEFile.short2hex(row) + "] " +
			       "changing type " + typeRefs[row - 1] +
			       " for type " + type);
	typeRefs[row - 1] = type;
	assert type != null : "Couldn't find type " + typeName;
	return type;
    }

    private Assembly getAssembly(String name) {
        Assembly assem = Assembly.getAssembly(name);
        if (assem != null)
            return assem;
        java.io.File dir = pefile.getParentFile();
        assem = Assembly.LoadFrom(dir, name);
        if (assem != null)
            return assem;
        try {
            dir = pefile.getUnderlyingFile().getCanonicalFile().getParentFile();
        } catch (java.io.IOException e) {
            throw new RuntimeException(e);
        }
        assem = Assembly.LoadFrom(dir, name);
        if (assem != null)
            return assem;
        throw new RuntimeException("Cannot find assembly: " + name);

    }

    /** Return the type corresponding to TypeDefOrRef coded index.
     *  @param index - TypeDefOrRef coded index according to 23.2.6.
     */
    public Type getTypeDefOrRef(int index) {
	int tableId = Table.getTableId(Table._TypeDefOrRef, index);
	int row = index >> Table.NoBits[Table._TypeDefOrRef];
	Type type = null;
	switch (tableId) {
	case Table.TypeDef.ID:
	    type = getTypeDef(row);
	    break;
	case Table.TypeRef.ID:
	    return getTypeRef(row);
	case Table.TypeSpec.ID:
	    throw new RuntimeException("PEModule.getTypeDefOrRef(): TypeSpec");
	default:
	    throw new RuntimeException("PEModule.getTypeDefOrRef(): oops!");
	}
	return type;
    }

    /** Returns the method defined at the given row of the MethodDef table
     *  by looking up the type that defines the method.
     */
    MethodBase getMethod(int row) {
	for (int i = 0; i < types.length; i++) {
	    PEType type = (PEType)types[i];
	    if ((type.methodListBeg <= row) && (row < type.methodListEnd)) {
		type.initMethods();
		return type.methoddefs[row - type.methodListBeg];
	    }
	}
	throw new RuntimeException("In module " + this
				   + ": cannot find type defining method 0x"
				   + PEFile.int2hex(row));
    }

    /** Returns the member referenced by the given row of the MemberRef table.
     */
    protected MemberInfo getMemberRef(int row) {
        return getMemberRef(row, null);
    }

    /** Returns the member referenced by the given row of the MemberRef table
     *  if defined in the given assembly.
     *  <i>Used by initCustomAttributes to avoid unnecessary loading of
     *  referenced assemblies</i>
     */
    protected MemberInfo getMemberRef(int row, Assembly inAssembly) {
	MemberInfo member = null;
	MemberRef mref = pefile.MemberRef;
	mref.readRow(row);
	int mtbl = Table.getTableId(Table._MemberRefParent, mref.Class);
	int mind = Table.getTableIndex(Table._MemberRefParent, mref.Class);
	switch (mtbl) {
	case TypeRef.ID:
	    Type type = getTypeRef(mind, inAssembly);
            if (type == null)
                return null;
            Sig sig = mref.getSignature();
            int callconv = sig.readByte(); // should be 0x20
            int paramCount = sig.decodeInt();
	    //sig.skipByte(Signature.ELEMENT_TYPE_BYREF); //from MethodDef
	    Type retType = sig.decodeRetType();
	    Type[] paramType = new Type[paramCount];
	    for (int i = 0; i < paramCount; i++)
		paramType[i] = sig.decodeParamType();

            String memberName = mref.getName();
            if (memberName.equals(ConstructorInfo.CTOR) ||
                memberName.equals(ConstructorInfo.CCTOR))
            {
                member = type.GetConstructor(paramType);
            } else {
                member = type.GetMethod(memberName, paramType);
            }
            assert member != null : type + "::" + memberName;
	    break;
	case ModuleRef.ID:
	case MethodDef.ID:
	case TypeSpec.ID:
	    throw new RuntimeException("initCustomAttributes: "
                                       + pefile.getTable(mtbl).getTableName());
	}
	return member;
    }

    protected void initCustomAttributes(Type attributeType) {
        initAttributes(this, definingRow, Table.ModuleDef.ID, attributeType);
    }

    // explicitly only package-visible
    void initAttributes(CustomAttributeProvider cap, int definingRow,
                         int sourceTableId, Type attributeType)
    {
	int parentIndex = Table.encodeIndex(definingRow,
                                            Table._HasCustomAttribute,
                                            sourceTableId);
	Table.CustomAttribute attrs = pefile.CustomAttribute;
	for (int row = 1; row <= attrs.rows; row++) {
            ConstructorInfo attrConstr = null;
	    attrs.readRow(row);
	    if (attrs.Parent == parentIndex) {
		int tableId = Table.getTableId(Table._CustomAttributeType,
					       attrs.Type);
		int ind = Table.getTableIndex(Table._CustomAttributeType,
					      attrs.Type);
		switch (tableId) {
		case MethodDef.ID:
		    attrConstr = (ConstructorInfo)this.getMethod(ind);
		    break;
		case MemberRef.ID:
		    //System.out.println(PEFile.short2hex(ind) + "@MemberRef");
                    Assembly attrAssem =
                        attributeType == null ? null : attributeType.Assembly();
		    MemberInfo mi = this.getMemberRef(ind, attrAssem);
                    if (mi != null) {
                        assert mi instanceof ConstructorInfo
                            : "Expected ConstructorInfo; found " + mi;
                        attrConstr = (ConstructorInfo)mi;
                    }
		    break;
		default:
		    throw new RuntimeException();
		}
                if (attrConstr != null
                    && (attrConstr.DeclaringType == attributeType
                        || attributeType == null))
                    cap.addCustomAttribute(attrConstr, attrs.getValue());
	    }
	}
    }

    //##########################################################################

} // class PEModule
