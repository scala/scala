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
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Arrays;
import java.util.Comparator;

import java.io.File;
import scala.tools.util.AbstractFile;
import scala.tools.util.ByteArrayFile;

import ch.epfl.lamp.compiler.msil.*;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.Scope;
import scalac.symtab.SymbolNameWriter;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.NameTransformer;
import scalac.Global;

/**
 */
public class CLRPackageParser extends SymbolLoader {

    //##########################################################################

    public Type BYTE;
    public Type UBYTE;
    public Type CHAR;
    public Type SHORT;
    public Type USHORT;
    public Type INT;
    public Type UINT;
    public Type LONG;
    public Type ULONG;
    public Type FLOAT;
    public Type DOUBLE;
    public Type BOOLEAN;
    public Type VOID;
    public Type ENUM;

    public Type OBJECT;
    public Type STRING;
    public Type STRING_ARRAY;

    public Type SCALA_SYMTAB_ATTR;

    protected CLRClassParser completer;

    private final SymbolNameWriter snw = new SymbolNameWriter();

    private CLRPackageParser(Global global) {
	super(global);
    }

    public static CLRPackageParser instance;
    public static CLRPackageParser instance(Global global) {
	if (instance == null) {
	    instance = new CLRPackageParser(global);
	    instance.completer = new CLRClassParser(global, instance);
	    instance.init();
	}
	return instance;
    }

    private Type[] types;

    private boolean initialized = false;
    public void init() {
	if (initialized) return;
	scala.tools.util.ClassPath.addFilesInPath(
            assemrefs, global.args.assemrefs.value);
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
	ENUM    = getType("System.Enum");

	OBJECT = getType("System.Object");
	STRING = getType("System.String");
	STRING_ARRAY = getType("System.String[]");

 	findAssembly("vjslib.dll");
   	findAssembly("scala.dll");

	SCALA_SYMTAB_ATTR = Type.GetType("scala.support.SymtabAttribute");

	Type[] types = Type.EmptyTypes;
	Iterator as = assemblies.iterator();
	while (as.hasNext()) {
	    Type[] atypes = ((Assembly)as.next()).GetTypes();
	    int j = 0;
	    for (int i = 0; i < atypes.length; i++)
		// skip nested types
		if (/*atypes[i].IsPublic && */atypes[i].DeclaringType == null)
		    atypes[j++] = atypes[i];
	    Type[] btypes = new Type[types.length + j];
	    System.arraycopy(types, 0, btypes, 0, types.length);
	    System.arraycopy(atypes, 0, btypes, types.length, j);
	    types = btypes;
	}
	 Comparator typeNameComparator = new Comparator() {
		 public int compare(Object o1, Object o2) {
		     Type t1 = (Type)o1;
		     Type t2 = (Type)o2;
		     return t1.FullName.compareTo(t2.FullName);
		 }
	     };

	Arrays.sort(types, typeNameComparator);
	this.types = types;
	initialized = true;
    }

    //##########################################################################

    private java.util.Map syms2members = new HashMap();
    private java.util.Map members2syms = new HashMap();

    public Type[] getTypes() { return types; }

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
    protected final java.util.List assemblies = new LinkedList();

    // a set of all directories and assembly files
    protected final Set/*<File>*/ assemrefs = new LinkedHashSet();

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
	global.fail("Cannot find assembly " + name
		    + "; use the -r option to specify its location");
	return null;
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

    private Symbol pakage;
    private HashMap/*<String,Type>*/ typesMap;
    private Set/*<String>*/ namespacesSet;

    private static final String[] BANNED = new String[] {
	"scala.AnyVal", "scala.Array", "scala.Boolean", "scala.Byte",
	"scala.Char", "scala.Double", "scala.Float", "scala.Function0",
	"scala.Function1", "scala.Function2", "scala.Function3",
	"scala.Function4", "scala.Function5", "scala.Function6",
	"scala.Function7", "scala.Function8", "scala.Function9", "scala.Int",
	"scala.Long", "scala.MatchError", "scala.Ref", "scala.ScalaObject",
	"scala.Short", "scala.Type", "scala.Unit", "scala.runtime.NativeLoop",
	"scala.runtime.ResultOrException", "scala.runtime.RunTime",
	"java.lang.Object", "java.lang.String", "java.lang.CharSequence",
	"java.lang.StringBuffer", "java.lang.Byte", "java.lang.Float",
	"java.lang.Double", "java.lang.Cloneable"
    };
    private static final Set BANNED_TYPES = new HashSet();
    static {
	for (int i = 0; i < BANNED.length; i++) {
	    BANNED_TYPES.add(BANNED[i]);
	}
    }

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
    private void enumerateMembers(Symbol pakage) {
	if (this.pakage == pakage)
	    return;
	this.pakage = pakage;
	typesMap = new HashMap();
	namespacesSet = new LinkedHashSet();

	String namespace = pakage.isRoot() ? "" : snw.toString(pakage) + ".";
	int nl = namespace.length();
	for (int i = findFirst(namespace);
	     i < types.length && types[i].FullName.startsWith(namespace);
	     i++)
	{
	    Type type = types[i];
	    if (BANNED_TYPES.contains(type.FullName)) {
		//System.out.println("Skipping CLR type " + type);
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

    /** return a mapping from type names to types contained
     *  in the given Scala package (namespace)
     */
    HashMap getTypes(Symbol pakage) {
	enumerateMembers(pakage);
	return typesMap;
    }

    /** Returns a set of the namespaces contained
     *  in the given Scala package (namespace)
     */
    Set getNamespaces(Symbol pakage) {
	enumerateMembers(pakage);
	return namespacesSet;
    }

    boolean shouldLoadClassfile(Symbol pakage, String name) {
	String fullname = snw.toString(pakage) + "." + name;
	return BANNED_TYPES.contains(fullname);
    }

    //##########################################################################
    // main functionality

    protected String doComplete(Symbol root) {
        assert !root.isRoot() && root.isPackage() : Debug.show(root);
        Symbol pakage = root.isRoot() ? root : root.moduleClass();
	Scope members = new Scope();

	// import the types contained in the namespace
	for (Iterator i = getTypes(pakage).values().iterator(); i.hasNext(); ) {
	    Type type = (Type)i.next();
	    importType(type, pakage, members);
	}

	// import the namespaces contained in the namespace
	for (Iterator i = getNamespaces(pakage).iterator(); i.hasNext(); ) {
	    String namespace = (String)i.next();
	    importNamespace(namespace, pakage, members);
	}

        pakage.setInfo(scalac.symtab.Type.compoundType
		       (scalac.symtab.Type.EMPTY_ARRAY, members, pakage));
        return "namespace " + Debug.show(pakage);
    }

    /** Imports a CLR type in a scala package (only called from PackageParser)
     */
    void importType(Type type, Symbol pakage, Scope members) {
	// discard top level types
	if (type.Namespace.equals("")) {
	    return;
	}

	assert snw.toString(pakage).equals(type.Namespace)
	    : Debug.show(pakage) + " << " + type.FullName;
	Object[] symtab_attr=type.GetCustomAttributes(SCALA_SYMTAB_ATTR, false);
	AbstractFile symtab = null;
	for (int l = 0; l < symtab_attr.length; l++) {
	    MemberInfo.Attribute a = (MemberInfo.Attribute)symtab_attr[l];
	    if (a.GetType() == SCALA_SYMTAB_ATTR) {
		symtab = new ByteArrayFile
		    (type.FullName, type.Assembly.GetName().toString(),
		     a.getValue());
		break;
	    }
	}
	SymbolLoader loader = symtab == null ? this.completer
	    : new SymblParser(global, symtab);

	Name classname = Name.fromString(type.Name).toTypeName();
	Symbol clazz = pakage.newLoadedClass(JAVA, classname, loader, members);
	Type moduleType = getType(type.FullName + "$");
// 	if (moduleType != null)
// 	    System.out.println("Module implementation class: " + moduleType);
	map(clazz, type);
// 	map(clazz, moduleType != null ? moduleType : type);
    }

    /** Imports a CLR namespace as a scala package
     *  (only called from PackageParser)
     */
    void importNamespace(String namespace, Symbol p, Scope members) {
	Name name = Name.fromString(namespace);
	if (members.lookup(name) == Symbol.NONE) {
	    p.newLoadedPackage(name, this, members);
	}
    }

    //##########################################################################
} // CLRPackageParser
