/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil._
import java.util.HashMap
import java.util.HashSet
import java.util.ArrayList
import java.io.IOException

/**
 * Defines and represents a dynamic assembly.
 * A dynamic assembly is an assembly that is created using the compiler.msil
 * emit APIs. The dynamic modules in the assembly are saved when the dynamic
 * assembly is saved using the Save method. To generate an executable, the
 * SetEntryPoint method must be called to identify the method that is the
 * entry point to the assembly. Assemblies are saved as DLL by default,
 * unless SetEntryPoint requests the generation of a console application
 * or a Windows-based application.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class AssemblyBuilder(name: AssemblyName)
      extends Assembly(name)
      with ICustomAttributeSetter
      with Visitable
{
    //##########################################################################
    // public methods

    /**
     * Defines a dynamic module with the given name that will be saved
     * to the specified file. No symbol information is emitted.
     */
    def DefineDynamicModule(name: String, fileName: String): ModuleBuilder = {
	val module = new ModuleBuilder(name, fileName, "" + null, this)
	addModule(name, module)
	return module
    }

    /** Returns the dynamic module with the specified name. */
    def GetDynamicModule(name: String): ModuleBuilder = {
	return GetModule(name).asInstanceOf[ModuleBuilder]
    }

    /** Saves this dynamic assembly to disk. */
    @throws(classOf[IOException])
    def Save(fileName: String) {
    generatedFiles = new ArrayList()
	ILPrinterVisitor.printAssembly(this, fileName)
    }

    @throws(classOf[IOException])
    def Save(destPath: String, sourceFilesPath: String) {
    generatedFiles = new ArrayList()
    ILPrinterVisitor.printAssembly(this, destPath, sourceFilesPath)
    }

    /** Returns the list of generated files from calling Save(). */
    def GetGeneratedFiles(): Array[String] = {
    return generatedFiles.toArray(new Array[String](generatedFiles.size())).asInstanceOf[Array[String]]
    }

    /** Sets the entry point for this dynamic assembly. */
    def SetEntryPoint(entryMethod: MethodInfo) {
	EntryPoint = entryMethod
    }

    /** Sets a custom attribute. */
    def SetCustomAttribute(constr: ConstructorInfo, value: Array[Byte]) {
	addCustomAttribute(constr, value)
    }

    //##########################################################################
    // protected members

    // the access properties -  Save, Run, RunAndSave
    private var access : Int = _

    // all extern assemblies used in this assembly builder
    protected var externAssemblies = new HashSet[Assembly]()

    // register an extern assembly
    protected def registerExternAssembly(assembly: Assembly) {
	externAssemblies.add(assembly)
    }

    // get all extern Assemblies used in this Assembly Builder
    def getExternAssemblies(): Array[Assembly] = {
	externAssemblies = new HashSet[Assembly](Assembly.assemblies.values().asInstanceOf[java.util.Collection[Assembly]])
	externAssemblies.remove(this)
	return externAssemblies.toArray(new Array[Assembly](0)).asInstanceOf[Array[Assembly]]
    }

    def loadModules() {}

    // contains list of generated .msil files after calling Save()
    var generatedFiles: ArrayList[String] = new ArrayList[String]()

    //##########################################################################
    //##########################################################################

    /** the apply method for a visitor */
    @throws(classOf[IOException])
    def apply(v: Visitor) {
	v.caseAssemblyBuilder(this)
    }

    //##########################################################################
}

object AssemblyBuilderFactory {
    /**
     * Defines a dynamic assembly with the specified name.
     */
    def DefineDynamicAssembly(name: AssemblyName): AssemblyBuilder = {
    //Assembly.reset()
    return new AssemblyBuilder(name)
    }
}
