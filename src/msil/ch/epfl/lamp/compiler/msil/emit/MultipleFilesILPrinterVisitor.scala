/*
 * System.Reflection.Emit-like API for writing .NET assemblies in MSIL
 */

// $Id$

package ch.epfl.lamp.compiler.msil.emit

import java.io.File
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
 * The MSIL printer Vistor. It prints a complete
 * assembly into seperate files. Then these files can be compiled by ilasm.
 *
 * @author Nikolay Mihaylov
 * @author Daniel Lorch
 * @version 1.0
 */
final class MultipleFilesILPrinterVisitor(destPath: String, sourceFilesPath: String) extends ILPrinterVisitor {
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
	ILPrinterVisitor.currAssembly = null
    }

    /**
     * Visit a ModuleBuilder
     */
    @throws(classOf[IOException])
    def caseModuleBuilder(module: ModuleBuilder) {
    val assemblyBuilder = ILPrinterVisitor.currAssembly.asInstanceOf[AssemblyBuilder]

	// print module declaration
	currentModule = module

	// global methods typically contain the main method
	if (!module.globalsCreated)
	    module.CreateGlobalFunctions()

	var m: Array[MethodInfo] = module.GetMethods()

	// "Types" contain all the classes
	var t: Array[Type] = module.GetTypes()
        for(val i <- 0 until t.length) {
        val tBuilder       = t(i).asInstanceOf[TypeBuilder]
        val sourceFilename = tBuilder.sourceFilename
        val sourceFilepath = new File(tBuilder.sourceFilepath).getCanonicalPath
        val sourcePath     = new File(sourceFilesPath).getCanonicalPath
		var append         = false

        if(!sourceFilepath.startsWith(sourcePath)) {
            throw new IOException("Source file " + sourceFilename + " must lie inside sourcepath " + sourcePath)
        }

        assert(sourceFilepath.endsWith(".scala"), "Source file doesn't end with .scala")
        val relativeFilename = sourceFilepath.substring(sourcePath.length, sourceFilepath.length() - 6) + ".msil"
        val fileName         = new File(destPath, relativeFilename)
        if(assemblyBuilder.generatedFiles.contains(fileName.getPath)) {
            append = true
        } else {
            fileName.getParentFile().mkdirs()
            assemblyBuilder.generatedFiles.add(fileName.getPath)
        }

	    out = new PrintWriter(new BufferedWriter(new FileWriter(fileName, append)))
		// only write assembly boilerplate and class prototypes
		if (!append && nomembers) {
			printAssemblyBoilerplate()

			print(".module \'"); print(module.Name); println("\'")
		    printAttributes(module)
        }

	    print(t(i).asInstanceOf[TypeBuilder])
	    out.close()
	}

    // now write the global methods (typically contains the "main" method)
	if(!nomembers) {
       var globalMethods: File = new File(destPath, ILPrinterVisitor.currAssembly.GetName().Name + ".msil")
       val append = assemblyBuilder.generatedFiles.contains(globalMethods.getPath)

		out = new PrintWriter(new BufferedWriter(new FileWriter(globalMethods, append)))

        // make sure we're the first in the list (ilasm uses the first file name to guess the output file name)
        assemblyBuilder.generatedFiles.add(0, globalMethods.getPath)

		// if this file hasn't been created by one of the classes, write boilerplate
		if(!append) {
			printAssemblyBoilerplate()

			print(".module \'"); print(module.Name); println("\'")
		    printAttributes(module)
		}

                for(val i <- 0 until m.length) {
	   		print(m(i).asInstanceOf[MethodBuilder])
		}

		out.close()
	}

	currentModule = null
    }

}  // class MultipleFilesILPrinterVisitor
