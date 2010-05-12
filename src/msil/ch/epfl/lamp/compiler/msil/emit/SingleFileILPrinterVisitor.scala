/*
 * System.Reflection.Emit-like API for writing .NET assemblies in MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import java.io.FileWriter
import java.io.BufferedWriter
import java.io.PrintWriter
import java.io.IOException
import java.util.Iterator
import java.util.HashMap
import java.util.Arrays

import ch.epfl.lamp.compiler.msil._
import ch.epfl.lamp.compiler.msil.emit
import ch.epfl.lamp.compiler.msil.util.Table

/**
 * The MSIL printer Visitor. It prints a complete
 * assembly in a single file. Then this file can be compiled by ilasm.
 *
 * @author Nikolay Mihaylov
 * @author Daniel Lorch
 * @version 1.0
 */
final class SingleFileILPrinterVisitor(_fileName: String) extends ILPrinterVisitor {
    var fileName: String = _fileName

    out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)))

   /**
     * Visit an AssemblyBuilder
     */
   @throws(classOf[IOException])
   def caseAssemblyBuilder(assemblyBuilder: AssemblyBuilder) {
	ILPrinterVisitor.currAssembly = assemblyBuilder

	// first get the entryPoint
	this.entryPoint = assemblyBuilder.EntryPoint

	// all external assemblies
	as = assemblyBuilder.getExternAssemblies()
	Arrays.sort(as, assemblyNameComparator)

        assemblyBuilder.generatedFiles.add(fileName)
	printAssemblyBoilerplate()

	// print each module
        var m: Array[Module] = assemblyBuilder.GetModules()
        nomembers = true
        for(val i <- 0 until m.length) {
	    print(m(i).asInstanceOf[ModuleBuilder])
	}

        nomembers = false
        for(val i <- 0 until m.length) {
	    print(m(i).asInstanceOf[ModuleBuilder])
	}
	// close out file
	out.close()
	ILPrinterVisitor.currAssembly = null
    }

    /**
     * Visit a ModuleBuilder
     */
    @throws(classOf[IOException])
    def caseModuleBuilder(module: ModuleBuilder) {
	// print module declaration
	currentModule = module
        if (nomembers) {
            print(".module \'"); print(module.Name); println("\'")
            printAttributes(module)
        }

	if (!module.globalsCreated)
	    module.CreateGlobalFunctions()

	var m: Array[MethodInfo] = module.GetMethods()
        for(val i <- 0 until m.length) {
	    print(m(i).asInstanceOf[MethodBuilder])
	}

	var t: Array[Type] = module.GetTypes()
        for(val i <- 0 until t.length) {
	    print(t(i).asInstanceOf[TypeBuilder])
	}
	currentModule = null
    }

}  // class SingleFileILPrinterVisitor
