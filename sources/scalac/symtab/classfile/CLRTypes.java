/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Comparator;

import java.io.File;

import scalac.Global;
import scalac.CompilerCommand;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolNameWriter;

import ch.epfl.lamp.compiler.msil.*;

/**
 * Collects all types from all reference assemblies.
 */
public final class CLRTypes {

    //##########################################################################

    private static CLRTypes instance;

    /** Return the unique instance of the CLRTypes class */
    public static CLRTypes instance() {
	assert instance != null;
	return instance;
    }

    /** Initialize the CLRTypes */
    public static void init(CompilerCommand args) {
	instance = new CLRTypes(args);
    }

    //##########################################################################

    public final Type BYTE;
    public final Type UBYTE;
    public final Type CHAR;
    public final Type SHORT;
    public final Type USHORT;
    public final Type INT;
    public final Type UINT;
    public final Type LONG;
    public final Type ULONG;
    public final Type FLOAT;
    public final Type DOUBLE;
    public final Type BOOLEAN;
    public final Type VOID;
    public final Type ENUM;

    public final Type OBJECT;
    public final Type STRING;
    public final Type STRING_ARRAY;

    public final Type SCALA_SYMTAB_ATTR;

    public final Type PICO_META_ATTR;

    private final SymbolNameWriter snw = new SymbolNameWriter();

    private Type[] types;

    private final CompilerCommand args;

    private CLRTypes(CompilerCommand args) {
	this.args = args;
	scala.tools.util.ClassPath.addFilesInPath(
            assemrefs, args.assemrefs.value);
	Assembly mscorlib = findAssembly("mscorlib.dll");
	Type.initMSCORLIB(mscorlib);
        findAssembly("vjscor.dll");
	findAssembly("vjslib.dll");
	findAssembly("scala.dll");
	findAllAssemblies();

	BYTE    = getType("System.SByte");
	UBYTE   = getType("System.Byte");
	CHAR    = getType("System.Char");
	SHORT   = getType("System.Int16");
	USHORT  = getType("System.UInt16");
	INT     = getType("System.Int32");
	UINT    = getType("System.UInt32");
	LONG    = getType("System.Int64");
	ULONG   = getType("System.UInt64");
	FLOAT   = getType("System.Single");
	DOUBLE  = getType("System.Double");
	BOOLEAN = getType("System.Boolean");
	VOID    = getType("System.Void");
	ENUM    = getType("System.Enum");

	OBJECT = getType("System.Object");
	STRING = getType("System.String");
	STRING_ARRAY = getType("System.String[]");

	SCALA_SYMTAB_ATTR = Type.GetType("scala.runtime.SymtabAttribute");
        PICO_META_ATTR = Type.GetType("scala.runtime.MetaAttribute");

        assert PICO_META_ATTR != null;

	Type[] types = Type.EmptyTypes;
	Iterator as = assemblies.iterator();
	while (as.hasNext()) {
	    Type[] atypes = ((Assembly)as.next()).GetTypes();
	    int j = 0;
	    for (int i = 0; i < atypes.length; i++)
		// skip nested types
		if (atypes[i].DeclaringType == null)
		    atypes[j++] = atypes[i];
	    Type[] btypes = new Type[types.length + j];
	    System.arraycopy(types, 0, btypes, 0, types.length);
	    System.arraycopy(atypes, 0, btypes, types.length, j);
	    types = btypes;
	}

        Comparator typeNameComparator =
            new Comparator() {
                public int compare(Object o1, Object o2) {
                    Type t1 = (Type)o1;
                    Type t2 = (Type)o2;
                    return t1.FullName.compareTo(t2.FullName);
                }
            };

	Arrays.sort(types, typeNameComparator);
	this.types = types;
    }

    //##########################################################################
    // type mapping and lookup

    private final Map syms2members = new HashMap();
    private final Map members2syms = new HashMap();

    public void map(Symbol sym, MemberInfo m) {
	syms2members.put(sym, m);
	members2syms.put(m, sym);
    }

    public MemberInfo getMember(Symbol sym) {
	return (MemberInfo)syms2members.get(sym);
    }

    public Symbol getSymbol(MemberInfo m) {
	return (Symbol)members2syms.get(m);
    }

    public Type getType(String name) {
	Type t = Type.GetType(name);
	//assert t != null : name;
	return t;
    }

    public Type mkArrayType(Type elemType) {
	return getType(elemType.FullName + "[]");
    }

    //##########################################################################
    // assembly loading methods

    // a list of all loaded assemblies
    private final List assemblies = new LinkedList();

    // a set of all directories and assembly files
    private final Set/*<File>*/ assemrefs = new LinkedHashSet();

    /** Load the assembly with the given name
     */
    private Assembly findAssembly(String name) {
	// see if the assembly is referenced directly
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    File file = (File)assems.next();
	    if (!file.getName().equals(name))
		continue;
	    Assembly assem = Assembly.LoadFrom(file.getPath());
	    if (assem != null) {
		assems.remove();
		assemblies.add(assem);
		return assem;
	    }
	}
	// look in directories specified with the '-r' option
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    File d = (File)assems.next();
	    if (!d.isDirectory())
		continue;
	    File file = new File(d, name);
	    if (file.exists()) {
		Assembly assem = Assembly.LoadFrom(file.getPath());
		if (assem != null) {
		    assemblies.add(assem);
		    return assem;
		}
	    }
	}
	// try in the current directory
	File file = new File(".", name);
	if (file.exists()) {
	    Assembly assem = Assembly.LoadFrom(file.getPath());
	    if (assem != null) {
		assemblies.add(assem);
		return assem;
	    }
	}
	//the Global instance is not yet constructed; use the Reporter from args
	args.reporter().error(null, "cannot find assembly " + name +
			      "; use the -r option to specify its location");
	throw Debug.abort();
    }

    /** Load the rest of the assemblies specified with the '-r' option
     */
    private void findAllAssemblies() {
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    File f = (File)assems.next();
	    if (f.isFile()) {
		Assembly assem = Assembly.LoadFrom(f.getPath());
		if (assem != null) {
		    assemblies.add(assem);
		}
	    }
	    assems.remove();
	}
	assert assemrefs.isEmpty();
    }

    //##########################################################################
    // collect the members contained in a given namespace

    /** Find the position of the first type whose name starts with
     *  the given prefix; return the length of the types array if no match
     *  is found so the result can be used to terminate loop conditions
     */
    private int findFirst(String prefix) {
	int m = 0, n = types.length - 1;
	while (m < n) {
	    int l = (m + n) / 2;
	    int res = types[l].FullName.compareTo(prefix);
	    if (res < 0) m = l + 1;
	    else n = l;
	}
	return types[m].FullName.startsWith(prefix) ? m : types.length;
    }

    /** Collects the members contained in the given Scala package (namespace)
     */
    void collectMembers(Symbol pakage, Map/*<String,Type>*/ typesMap,
        Set/*<String>*/ namespacesSet)
    {
	String namespace = pakage.isRoot() ? "" : snw.toString(pakage) + ".";
	int nl = namespace.length();
	for (int i = findFirst(namespace);
	     i < types.length && types[i].FullName.startsWith(namespace);
	     i++)
	{
	    Type type = types[i];
 	    if (type.FullName.equals("java.lang.Object")
		|| type.FullName.equals("java.lang.String")) {
 		continue;
 	    }
	    int k = type.FullName.indexOf(".", nl);
	    if (k < 0) {
		typesMap.put(type.Name, type);
	    } else {
		namespacesSet.add(type.Namespace.substring(nl, k));
	    }
	}
    }

    /** Returns the namespace of the given package */
    String getNameSpaceOf(Symbol pakage) {
        assert pakage.hasPackageFlag() || pakage.isRoot(): Debug.show(pakage);
        return pakage.isRoot() ? "" : snw.toString(pakage);
    }

    //##########################################################################
}  // CLRTypes
