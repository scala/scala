/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.Table;
import ch.epfl.lamp.compiler.msil.util.Table.*;

import java.io.File;

import java.util.Map;
import java.util.HashMap;

/** Represents an assembly that resides in a real .NET assembly
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEAssembly extends Assembly {

    private final PEFile pefile;

    private PEModule mainModule;

    public PEAssembly(PEFile pefile, AssemblyName an) {
	super(an, true);
	this.pefile = pefile;
	String name = pefile.ModuleDef(1).getName();
	mainModule = new PEModule(pefile, 1, name, this);
	addModule(name, mainModule);
        //initModules();
    }

    protected void loadModules() {
	File parentDir = pefile.getParentFile();
	FileDef fd = pefile.FileDef;
	for (int row = 1; row <= fd.rows; row++) {
	    fd.readRow(row);
	    String filename = fd.getName();
	    File f = new File(parentDir, filename);
	    PEFile pe = Assembly.getPEFile(f);
	    if (pe == null) {
		f = new File(filename);
		pe = Assembly.getPEFile(f);
		if (pe == null)
		    continue;
// 		throw new RuntimeException("Cannot find file " + filename +
// 					   " referenced by assembly " + this);
	    }
	    String name = pe.ModuleDef(1).getName();
	    PEModule module = new PEModule(pe, 1, name, this);
	    addModule(name, module);
	}
    }

    public File getFile() {
	return pefile.getUnderlyingFile();
    }

    protected void loadCustomAttributes(Type attributeType) {
        initModules();
        mainModule.initAttributes(this, 1, Table.AssemblyDef.ID, attributeType);
    }

    //##########################################################################

} // class PEAssembly
