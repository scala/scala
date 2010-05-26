/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import java.util.Map;
import java.util.HashMap;

/**
 * Defines and represents a module. Get an instance of ModuleBuilder
 * by calling DefineDynamicModule
 * A module is a portable executable file of type .dll or .exe consisting
 * of one or more classes and interfaces. There may be multiple namespaces
 * contained in a single module, and a namespace may span multiple modules.
 * One or more modules deployed as a unit compose an assembly.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class Module extends CustomAttributeProvider {

    //##########################################################################
    // public fields

    /** String representing the name of the module with the path removed. */
    public final String Name;

    /** String representing the fully qualified name and path to this module. */
    public final String FullyQualifiedName;

    /** String representing the name of the module. */
    public String ScopeName;

    /** The Assembly the Module belongs to. */
    public final Assembly Assembly;

    //##########################################################################
    // constructor

    protected Module(String name, String filename,
		     String scopeName, Assembly assembly)
    {
	this.Name = name;
	this.FullyQualifiedName = filename;
	this.ScopeName = scopeName;
	this.Assembly = assembly;
    }

    //##########################################################################
    // public methods

    /** Returns the specified class, performing a case-sensitive search. */
    public Type GetType(String name) {
        initTypes();
	return (Type) typesMap.get(name);
    }

    /**
     * @return all the classes defined within this module.
     */
    public Type[] GetTypes() {
        initTypes();
	return (Type[]) types.clone();
    }

    /**
     * @return the global field with the specified name.
     */
    public FieldInfo GetField(String name) {
	for (int i = 0; i < fields.length; i++)
	    if (fields[i].Name.equals(name))
		return fields[i];
	return null;
    }

    /**
     * @return an array of the global fields of the module
     */
    public FieldInfo[] GetFields() {
	return (FieldInfo[]) fields.clone();
    }

    /**
     * @return - the global method with the specified name
     */
    public MethodInfo GetMethod(String name) {
	for (int i = 0; i < methods.length; i++)
	    if (methods[i].Name.equals(name))
		return methods[i];
	return null;
    }

    /**
     * @return - an array of all the global methods defined in this modules.
     */
    public MethodInfo[] GetMethods() {
	return (MethodInfo[]) methods.clone();
    }

    /**
     */
    public String toString() { return Name; }

    //########################################################################
    // protected members

    // all the types defined in this module
    protected final Map typesMap = new HashMap();

    // all the types defined in this module
    protected Type[] types;

    // the global fields of the module
    protected FieldInfo[] fields = FieldInfo.EMPTY_ARRAY;

    // the global methods of the module
    protected MethodInfo[] methods = MethodInfo.EMPTY_ARRAY;

    protected Type addType(Type type) {
	addType(type.FullName, type);
	Assembly.addType(type);
	return type;
    }

    protected Type addType(String name, Type type) {
	assert type!= null;
	typesMap.put(name, type);
	return type;
    }

    private boolean initTypes = true;
    protected final void initTypes() {
        if (initTypes) {
            loadTypes();
            initTypes = false;
        }
    }

    protected void loadTypes() {}

    private boolean initGlobals = true;
    protected final void initGlobals() {
        if (initGlobals) {
            loadGlobals();
            initGlobals = false;
        }
    }

    protected void loadGlobals() {}

    //##########################################################################

}  // class Module
