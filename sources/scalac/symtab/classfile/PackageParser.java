/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.io.IOException;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;

import ch.epfl.lamp.util.Position;

import scalac.Global;
import scalac.symtab.Scope;
import scalac.symtab.SourceCompleter;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.Type;
import scalac.util.AbstractFile;
import scalac.util.Name;
import scalac.util.SourceRepresentation;

/**
 * This class implements a package member loader. It can be used to
 * complete package class symbols.
 */
public class PackageParser extends SymbolLoader {

    //########################################################################
    // Private Fields

    /** The JVM class file parser */
    private final ClassParser classCompletion;

    /** The CLR package parser */
    private final CLRPackageParser importer;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public PackageParser(Global global) {
        super(global);
        this.classCompletion = new ClassParser(global);
	this.importer = (global.target == global.TARGET_MSIL)
	    ? CLRPackageParser.create(global) : null;
	if (global.reporter.verbose)
	    global.reporter.inform("classpath = " + global.classPath);
    }

    //########################################################################
    // Protected Methods

    /** Completes the package symbol by loading all its members. */
    protected String doComplete(Symbol peckage) {
	boolean isRoot = peckage.isRoot();
        String dirname = isRoot ? null
            : SourceRepresentation.externalizeFileName(peckage, "/");

        // collect JVM and source members
        HashMap sources = new HashMap();
        HashMap classes = new HashMap();
        HashSet packages = new HashSet();
        String[] base = global.classPath.components();
        for (int i = 0; i < base.length; i++) {
            AbstractFile dir = AbstractFile.open(base[i], dirname);
            if (dir == null) continue;
            try {
                String[] filenames = dir.list();
                if (filenames == null) continue;
                for (int j = 0; j < filenames.length; j++) {
                    String fname = filenames[j];
                    if (fname.endsWith("/") && !fname.equals("META-INF/")) {
                        String name = fname.substring(0, fname.length() - 1);
                        packages.add(name);
                        continue;
                    }
                    if (!isRoot && fname.endsWith(".class")) {
                        String name = fname.substring(0, fname.length() - 6);
                        if (!classes.containsKey(name))
                            classes.put(name, dir.open(fname));
                        continue;
                    }
                    if (!isRoot && fname.endsWith(".scala")) {
                        String name = fname.substring(0, fname.length() - 6);
                        if (!sources.containsKey(name))
                            sources.put(name, dir.open(fname));
                        continue;
                    }
                }
            } catch (IOException exception) {
                if (global.debug) exception.printStackTrace();
                continue;
            }
	}

        // create JVM and source members
        Scope members = new Scope();
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            AbstractFile cfile = (AbstractFile)classes.remove(name);
            if (global.separate && cfile != null) {
                if (cfile.lastModified() > sfile.lastModified()) {
                    classes.put(name, cfile);
                    continue;
                }
            }
            packages.remove(name);
            Name classname = Name.fromString(name).toTypeName();
            SourceCompleter completer = new SourceCompleter(global);
            peckage.newLoadedClass(0, classname, completer, members);
        }
        for (Iterator i = classes.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile cfile = (AbstractFile)entry.getValue();
            packages.remove(name);
            Name classname = Name.fromString(name).toTypeName();
            peckage.newLoadedClass(JAVA, classname, classCompletion, members);
        }
        for (Iterator i = packages.iterator(); i.hasNext(); ) {
            String name = (String)i.next();
            peckage.newLoadedPackage(Name.fromString(name), this, members);
        }

        // collect and create CLR members
        if (importer != null) importer.importCLRTypes(peckage, members, this);

        // initialize package
        peckage.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, peckage));
        return dirname == null ? "anonymous package" : "package '"+dirname+"'";
    }

    //########################################################################
}
