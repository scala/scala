/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import ch.epfl.lamp.util.*;
import scalac.*;
import scalac.symtab.*;
import scalac.util.*;
import java.io.*;
import java.util.HashMap;

public class PackageParser extends MetadataParser {

    /** the class parser
     */
    public ClassParser classCompletion;
    public SymblParser symblCompletion; // provisional

    protected final CLRPackageParser importer;

    public PackageParser(Global global) {
        super(global);
        this.classCompletion = new ClassParser(global);
	this.symblCompletion = new SymblParser(global); // provisional
	if (global.reporter.verbose)
	    System.out.println("classpath = " + global.classPath);//debug
	importer = (global.target == global.TARGET_MSIL)
	    ? CLRPackageParser.create(global) : null;
    }

    /** complete package type symbol p by loading all package members
     */
    protected void doComplete(Symbol p) {
        long msec = System.currentTimeMillis();
        Scope members = new Scope();
        String dirname = null;
	HashMap/*<Symbol, AbstractFile>*/ symFile = new HashMap();
        if (!p.isRoot()) {
            dirname = SourceRepresentation.externalizeFileName(p, "/");
        }
        String[] base = global.classPath.components();
        for (int i = 0; i < base.length; i++) {
            includeMembers(
		AbstractFile.open(base[i], dirname),
		p, members, symFile);
	}
 	if (global.target == global.TARGET_MSIL)
 	    importer.importCLRTypes(p, members, this);
        p.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, p));
        if (dirname == null)
            dirname = "anonymous package";
        global.operation("scanned " + dirname + " in " +
                    (System.currentTimeMillis() - msec) + "ms");
    }

    private boolean isMostRecent(AbstractFile f, Symbol previous, HashMap symFile) {
	if (previous == Symbol.NONE || previous.isPackage()) return true;
	if (previous.pos != Position.NOPOS) return false;
	AbstractFile pf = (AbstractFile) symFile.get(previous);
	if (f.getName().endsWith(".scala")) {
	    if (pf.getName().endsWith(".scala")) return false;
	    if (!global.separate) return true;
	}
	if (f.getName().endsWith(".class")) {
	    if (pf.getName().endsWith(".class")) return false;
	    if (!global.separate) return false;
	}
	return f.lastModified() > pf.lastModified();
    }

    /** read directory of a classpath directory and include members
     *  in package/module scope
     */
    protected void includeMembers(AbstractFile dir, Symbol p, Scope locals,
				  HashMap symFile) {
        if (dir == null)
            return;
	boolean inclClasses = p != global.definitions.ROOT_CLASS;
        String[] filenames = null;
        try {
            if ((filenames = dir.list()) == null)
                return;
            for (int j = 0; j < filenames.length; j++) {
                String fname = filenames[j];
		AbstractFile f = dir.open(fname);
                if (inclClasses && fname.endsWith(".class")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 6))
			.toTypeName();
		    if (isMostRecent(f, locals.lookup(n), symFile)) {
		        ClassSymbol clazz = new ClassSymbol(n, p, classCompletion);
			// todo: needed?
		        clazz.allConstructors().setInfo(
			    classCompletion.staticsParser(clazz));
			clazz.module().setInfo(
			    classCompletion.staticsParser(clazz));
		        // enter class
		        locals.enter(clazz);
		        // enter module
                        Scope.Entry e = locals.lookupEntry(clazz.module().name);
                        if (e != Scope.Entry.NONE) {
                            // we already have a package of the same name; delete it
                            locals.unlink(e);
                        }
                        locals.enter(clazz.module());
			symFile.put(clazz, f);
                    }
                } else if (inclClasses && fname.endsWith(".scala")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 6))
			.toTypeName();
		    if (isMostRecent(f, locals.lookup(n), symFile)) {
                        SourceCompleter completer = new SourceCompleter(global);
                        ClassSymbol clazz = new ClassSymbol(n, p, completer);
			//todo: needed?
                        clazz.allConstructors().setInfo(completer);
			clazz.module().setInfo(completer);
                        // enter class
                        locals.enter(clazz);
			locals.enter(clazz.module());
			symFile.put(clazz, f);
                    }
                } else if (fname.endsWith("/") && !fname.equals("META-INF/")) {
                    Name n = Name.fromString(fname.substring(0, fname.length() - 1));
                    if (locals.lookup(n) == Symbol.NONE) {
                        TermSymbol module = TermSymbol.newJavaPackageModule(n, p, this);
                        locals.enter(module);
			//todo: moduleClass needs to be entered?
			locals.enter(module.moduleClass());
		    }
                }
            }
        } catch (IOException e) {
        }
    }


}
