/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.List;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;

import java.io.File;

import ch.epfl.lamp.compiler.msil.*;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.TermSymbol;
import scalac.symtab.ClassSymbol;
import scalac.symtab.Scope;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.NameTransformer;
import scalac.Global;

/**
 */
public class CLRPackageParser extends SymbolLoader {

    //##########################################################################

    Type BYTE;
    Type UBYTE;
    Type CHAR;
    Type SHORT;
    Type USHORT;
    Type INT;
    Type UINT;
    Type LONG;
    Type ULONG;
    Type FLOAT;
    Type DOUBLE;
    Type BOOLEAN;
    Type VOID;

    Type OBJECT;
    Type STRING;
    Type STRING_ARRAY;

    protected CLRClassParser completer;

    private CLRPackageParser(Global global) {
	super(global);
    }

    public static CLRPackageParser instance;
    public static CLRPackageParser create(Global global) {
	if (instance != null)
	    return instance;
	instance = new CLRPackageParser(global);
	instance.completer = new CLRClassParser(global, instance);
	instance.init();
	return instance;
    }

    Type[] types;

    public Type[] getTypes() { return types; }

    private boolean initialized = false;
    public void init() {
	if (initialized) return;
	String[] asnames = scalac.util.ClassPath.parse(global.args.assemrefs.value);
	for (int i = 0; i < asnames.length; i++)
	    assemrefs.add(asnames[i]);
	Assembly mscorlib = findAssembly("mscorlib.dll");
	Type.initMSCORLIB(mscorlib);

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
	//ENUM    = getType("System.Enum");

	OBJECT = getType("System.Object");
	STRING = getType("System.String");
	STRING_ARRAY = getType("System.String[]");

 	findAssembly("vjslib.dll");
   	findAssembly("scala.dll");
   	//findAssembly("scalalib.dll");
	findAllAssemblies();

	Type[] types = Type.EmptyTypes;
	Iterator as = assemblies.iterator();
	while (as.hasNext()) {
	    Type[] atypes = ((Assembly)as.next()).GetTypes();
	    int j = 0;
	    for (int i = 0; i < atypes.length; i++)
		if (/*atypes[i].IsPublic && */atypes[i].DeclaringType == null)
		    atypes[j++] = atypes[i];
	    Type[] btypes = new Type[types.length + j];
	    System.arraycopy(types, 0, btypes, 0, types.length);
	    System.arraycopy(atypes, 0, btypes, types.length, j);
	    types = btypes;
	}
	this.types = types;
	initialized = true;
    }


    private java.util.Map syms2members = new HashMap();
    private java.util.Map members2syms = new HashMap();

    //##########################################################################

    public void map(Symbol sym, MemberInfo m) {
	syms2members.put(sym, m);
	members2syms.put(m, sym);
	//System.out.println("" + sym + " => " + m);
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

    protected final java.util.List assemblies = new LinkedList();

    protected final java.util.List assemrefs = new LinkedList();

    /** Load the assembly with the given name
     */
    private Assembly findAssembly(String name) {
	// see if the assembly is referenced directly
	File file = null;
	Assembly assem = null;
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    String assemname = (String)assems.next();
	    file = new File(assemname);
	    if (!file.getName().equals(name))
		continue;
	    assem = Assembly.LoadFrom(file.getPath());
	    if (assem != null) {
		assems.remove();
		assemblies.add(assem);
		return assem;
	    }
	}
	// look in directories specified with the '-r' option
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    File d = new File((String)assems.next());
	    if (!d.isDirectory())
		continue;
	    file = new File(d, name);
	    if (file.exists()) {
		assem = Assembly.LoadFrom(file.getPath());
		if (assem != null) {
		    assemblies.add(assem);
		    return assem;
		}
	    }
	}
	// try in the current directory
	file = new File(".", name);
	if (file.exists()) {
	    assem = Assembly.LoadFrom(file.getPath());
	    if (assem != null) {
		assemblies.add(assem);
		return assem;
	    }
	}
	global.fail("Cannot find assembly " + name
		    + "; use the -r option to specify its location");
	return null;
    }

    /** Load the rest of the assemblies specified with the '-r' option
     */
    private void findAllAssemblies() {
	//System.out.println("assembly references left: " + assemrefs);
	for (Iterator assems = assemrefs.iterator(); assems.hasNext();) {
	    File f = new File((String)assems.next());
	    if (f.isFile()) {
		Assembly assem = Assembly.LoadFrom(f.getPath());
		if (assem != null) {
		    assemblies.add(assem);
		    //System.out.println("Loaded assembly " + assem);
		}
	    }
	    assems.remove();
	}
	assert assemrefs.isEmpty();
    }

    //##########################################################################
    // main functionality

    protected String doComplete(Symbol p) {
	// for future use
        return "!!!";
    }

    /**
     */
    public void importCLRTypes(Symbol p, Scope members, PackageParser pp) {
	if (p.isRoot()) {
	    for (int i = 0; i < types.length; i++) {
		int j = types[i].FullName.indexOf('.');
		if (j < 0) continue;
		String namespace = types[i].FullName.substring(0, j);
		importCLRNamespace(namespace, p, members, pp);
	    }
	    return;
	}
        String n1 = "";
        for (Symbol q = p; !q.isRoot(); q = q.owner()) {
            n1 = NameTransformer.decode(q.name) + "." + n1;
            if (q == global.definitions.JAVA_CLASS
                || q == global.definitions.SCALA_CLASS)
                return;
        }
	for (int i = 0; i < types.length; i++) {
	    String fullname = types[i].FullName;
	    if (!fullname.startsWith(n1))
		continue;
	    //System.out.println("importing type: " + fullname);
	    int j = n1.length();
	    int k = fullname.indexOf('.', j);
	    String name = fullname.substring(j, k < 0 ? fullname.length() : k);
	    Name n = Name.fromString(name);
	    if (k < 0) {
		// it's a class
                Symbol clazz = p.newLoadedClass(JAVA, n.toTypeName(), completer, members);
		map(clazz, types[i]);
	    } else {
		importCLRNamespace(name, p, members, pp);
	    }
	}
    }

    /** Imports a CLR namespace as a scala package.
     */
    protected void importCLRNamespace(String namespace, Symbol p,
				   Scope members, PackageParser pp)
    {
	Name n = Name.fromString(namespace);
	if (members.lookup(n) == Symbol.NONE) {
	    //System.out.println("importing namespace " + namespace);
	    p.newLoadedPackage(n, pp, members);
	}
    }

    //##########################################################################
}
