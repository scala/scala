/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.Table;
import ch.epfl.lamp.compiler.msil.util.Table.AssemblyDef;
import ch.epfl.lamp.compiler.msil.util.Table.ModuleDef;

import java.util.HashMap;
import java.util.Iterator;
import java.io.File;
import java.io.FileNotFoundException;

/**
 * Defines an Assembly, which is a reusable, versionable, and self-describing
 * building block of a common language runtime application.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class Assembly extends CustomAttributeProvider {

    //##########################################################################
    // static members

    // all the assemblies
    public static final HashMap assemblies = new HashMap();

    /** Loads an assembly from the specified path. */
    public static Assembly LoadFrom(String assemblyFileName) {
	File afile = new File(assemblyFileName);
	return LoadFrom(afile.getParentFile(), afile.getName());
    }

    /** Loads an assembly with the given name from the given directory. */
    public static Assembly LoadFrom(File dir, String name) {
	File file = null;
	PEFile pefile = null;
// 	try {
// 	    if (dir == null)
// 		dir = new File(".");
// 	    dir = dir.getCanonicalFile();
// 	} catch (java.io.IOException e) {}

	if (name.toUpperCase().endsWith(".EXE") || name.toUpperCase().endsWith(".DLL")) {
		file = new File(dir, name);
		pefile = getPEFile(file);
		name = name.substring(0, name.length() - 4);
	}

	File adir = pefile == null ? new File(dir, name) : null;

	if (pefile == null) {
	    file = new File(dir, name + ".dll");
	    pefile = getPEFile(file);
	}
	if (pefile == null) {
	    file = new File(dir, name + ".DLL");
	    pefile = getPEFile(file);
	}
	if (pefile == null && adir.exists()) {
	    file = new File(adir, name + ".dll");
	    pefile = getPEFile(file);
	}
	if (pefile == null && adir.exists()) {
	    file = new File(adir, name + ".DLL");
	    pefile = getPEFile(file);
	}

	if (pefile == null) {
	    file = new File(dir, name + ".exe");
	    pefile = getPEFile(file);
	}
	if (pefile == null) {
	    file = new File(dir, name + ".EXE");
	    pefile = getPEFile(file);
	}
	if (pefile == null && adir.exists()) {
	    file = new File(adir, name + ".exe");
	    pefile = getPEFile(file);
	}
	if (pefile == null && adir.exists()) {
	    file = new File(adir, name + ".EXE");
	    pefile = getPEFile(file);
	}

	if (pefile == null)
	    throw new RuntimeException("Cannot find assembly " + new File(dir, name));
	return getPEAssembly(pefile);
    }

    private static Assembly getPEAssembly(PEFile pefile) {
	AssemblyDef assem = pefile.AssemblyDef;
	if (assem == null)
	    throw new RuntimeException("File " + pefile
				       + " does not contain a manifest");
	assem.readRow(1);
	String name = pefile.getString(assem.Name);
	Assembly a = (Assembly) assemblies.get(name);
	if (a != null) {
	    return a;
	}

	AssemblyName an = new AssemblyName();
	an.Name = pefile.getString(assem.Name);
	an.Version = new Version(assem.MajorVersion, assem.MinorVersion,
				 assem.BuildNumber, assem.RevisionNumber);
	an.SetPublicKey(pefile.getBlob(assem.PublicKey));
	return new PEAssembly(pefile, an);
    }

    protected static PEFile getPEFile(File f) {
	PEFile pefile = null;
	try { pefile = new PEFile(f.getAbsolutePath()); }
	catch (FileNotFoundException e) {}
	catch (RuntimeException e) {
            java.lang.System.out.println("swallowed RuntimeException at getPEFile");
    }
	return pefile;
    }

    //##########################################################################
    // public fields

    /** The entry point of this assembly. */
    public MethodInfo EntryPoint;

    /** the display name of the assembly. */
    public final String FullName;

    //##########################################################################
    // constructor

    protected Assembly(AssemblyName an, boolean external) {
	assemblyName = an;
	FullName = an.toString();
    if(external) {
	  assemblies.put(an.Name, this);
    }
	//System.out.println("assemblies after adding the current one: " + assemblies);
    }

    protected Assembly(AssemblyName an) {
      this(an, false);
    }

    protected static Assembly getAssembly(String name) {
	return (Assembly) assemblies.get(name);
    }

    //##########################################################################
    // instrumental methods

    /** @return the file from which this assembly was loaded. */
    public File getFile() {
	throw new RuntimeException("Not supported");
    }

    /** Gets the specified module in this assembly. Works on filenames. */
    public Module GetModule(String name) {
	initModules();
	return (Module)modulesMap.get(name);
    }

    /** Get all the modules of the assembly. */
    public Module[] GetModules() {
	initModules();
	return (Module[])modulesMap.values().
	    toArray(new Module[modulesMap.size()]);
    }

    /** Get the corresponding type. */
    public Type GetType(String name) {
	initModules();
	Iterator modules = modulesMap.values().iterator();
	Type t = null;
	while (t == null && modules.hasNext()) {
	    t = ((Module)modules.next()).GetType(name);
	}
	return t;
    }

    /** @return an array of all types defined in the assembly. */
    public synchronized Type[] GetTypes() {
 	if (types != null)
	    return (Type[])types.clone();
	initModules();

	Iterator modules = modulesMap.values().iterator();
	Type[] newTypes = ((Module)modules.next()).GetTypes();
	while (modules.hasNext()) {
	    Module module = (Module)modules.next();
	    Type[] mtypes = module.GetTypes();
	    Type[] oldTypes = newTypes;
	    newTypes = new Type[oldTypes.length + mtypes.length];
	    System.arraycopy(oldTypes, 0, newTypes, 0, oldTypes.length);
	    System.arraycopy(mtypes, 0, newTypes, oldTypes.length, mtypes.length);
	}
	types = newTypes;
	return (Type[]) types.clone();
    }

    public AssemblyName GetName() {
	return assemblyName;
    }

    public String toString() {
	return FullName;
    }

    //##########################################################################
    // protected members

    // the assembly name
    protected final AssemblyName assemblyName;

    // all the types exported by the assembly
    protected Type[] types = null;

    // the module defined in this assembly (only one right now)
    private final HashMap/*<String, Module>*/ modulesMap = new HashMap();

    protected void addType(Type type) {
	Type.addType(type);
    }

    protected void addModule(String name, Module module) {
	modulesMap.put(name, module);
    }

    private boolean initModules = true;
    protected final void initModules() {
	if (initModules) {
	    loadModules();
	    initModules = false;
	}
    }

    /** used for lazy construction of the Assembly. */
    protected abstract void loadModules();

    void dumpTypes() {
	Type[] types = GetTypes();
	for (int i = 0; i < types.length; i++)
	    System.out.println(types[i]);
    }

    //##########################################################################

}  // class Assembly
