/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.Iterator;
import java.util.HashMap;
import java.util.Set;

import scala.tools.util.AbstractFile;
import scala.tools.util.Position;

import scalac.Global;
import scalac.symtab.Scope;
import scalac.symtab.SourceCompleter;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Debug;

import scalac.symtab.SymbolNameWriter;

/**
 * This class implements a package member loader. It can be used to
 * complete package class symbols.
 */
public class PackageParser extends SymbolLoader {

    //########################################################################
    // Private Fields

    /** The directory to read */
    private final AbstractFile directory;

    /** Are we targeting the MSIL? */
    private boolean forMSIL;

    private final CLRPackageParser clrParser;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public PackageParser(Global global, AbstractFile directory) {
        super(global);
        this.directory = directory;
	this.forMSIL = global.target == global.TARGET_MSIL;
	this.clrParser = forMSIL ? CLRPackageParser.instance() : null;
    }

    //########################################################################
    // Protected Methods

    /** Completes the package symbol by loading all its members. */
    protected String doComplete(Symbol root) {
        assert root.isRoot() || root.isPackage(): Debug.show(root);
        Symbol peckage = root.isRoot() ? root : root.moduleClass();
        // collect JVM and source members
	boolean isRoot = peckage.isRoot();
        HashMap sources = new HashMap();
        HashMap classes = new HashMap();
        HashMap packages = new HashMap();
        for (Iterator i = directory.list(); i.hasNext(); ) {
            AbstractFile file = (AbstractFile)i.next();
            String filename = file.getName();
            if (file.isDirectory()) {
                if (filename.equals("META-INF")) continue;
                packages.put(filename, file);
                continue;
            }
            if (!isRoot && filename.endsWith(".class")) {
                String name = filename.substring(0, filename.length() - 6);
                if (!classes.containsKey(name)) classes.put(name, file);
                continue;
            }
            if (!isRoot && filename.endsWith(".scala")) {
                String name = filename.substring(0, filename.length() - 6);
                if (!sources.containsKey(name)) sources.put(name, file);
                continue;
            }
	}

	HashMap types = null;
	Set namespaces = null;
	ch.epfl.lamp.compiler.msil.Type type = null;
	if (forMSIL) {
	    types = clrParser.getTypes(peckage);
	    namespaces = clrParser.getNamespaces(peckage);
	}

        // create JVM and source members
        Scope members = new Scope();
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            AbstractFile cfile = (AbstractFile)classes.remove(name);
	    if (forMSIL) {
		type = (ch.epfl.lamp.compiler.msil.Type)types.remove(name);
		if (global.separate && forMSIL && type != null) {
// 		    if (type.Assembly.getFile().lastModified()
// 			> sfile.lastModified()) {
			types.put(name, type);
 			continue;
// 		    }
		}
	    } else if (global.separate && cfile != null) {
                if (cfile.lastModified() > sfile.lastModified()) {
		    classes.put(name, cfile);
		    continue;
                }
            }
            packages.remove(name);
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new SourceCompleter(global, sfile);
            peckage.newLoadedClass(0, classname, loader, members);
        }

        for (Iterator i = classes.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
	    //assert !types.containsKey(name) : types.get(name);
	    if (!forMSIL || clrParser.shouldLoadClassfile(peckage, name)) {
		AbstractFile cfile = (AbstractFile)entry.getValue();
		packages.remove(name);
		Name classname = Name.fromString(name).toTypeName();
		SymbolLoader loader = new ClassParser(global, cfile);
		peckage.newLoadedClass(JAVA, classname, loader, members);
	    }
        }

	if (forMSIL) {
	    // import the CLR types contained in the package (namespace)
	    for (Iterator i = types.values().iterator(); i.hasNext(); ) {
		type = (ch.epfl.lamp.compiler.msil.Type)i.next();
		clrParser.importType(type, peckage, members);
	    }
	}

        for (Iterator i = packages.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile dfile = (AbstractFile)entry.getValue();
            SymbolLoader loader = new PackageParser(global, dfile);
            peckage.newLoadedPackage(Name.fromString(name), loader, members);
	    if (forMSIL)
		namespaces.remove(name);
        }

	if (forMSIL) {
 	    // import the CLR namespaces contained in the package (namespace)
 	    for (Iterator i = namespaces.iterator(); i.hasNext(); ) {
 		String namespace = (String)i.next();
		Name name = Name.fromString(namespace);
		if (members.lookup(name) == Symbol.NONE) {
		    peckage.newLoadedPackage(name, this, members);
		}
 	    }
	}

        // initialize package
        peckage.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, peckage));
        return "directory path '" + directory + "'";
    }

    //########################################################################
}
