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

public class PackageParser extends Type.LazyType {

    /** the global compilation environment
     */
    protected Global global;

    /** the class parser
     */
    public ClassParser classCompletion;
    public SymblParser symblCompletion; // provisional

    public PackageParser(Global global) {
        this.global = global;
        this.classCompletion = new ClassParser(global);
	this.symblCompletion = new SymblParser(global); // provisional
	if (global.reporter.verbose)
	    System.out.println("classpath = " + global.classPath);//debug
    }

    /** complete package type symbol p by loading all package members
     */
    public void complete(Symbol p) {
        long msec = System.currentTimeMillis();
        Scope members = new Scope();
        String dirname = null;
        Name name = p.fullName();
	HashMap/*<Symbol, AbstractFile>*/ symFile = new HashMap();
        if (name.length() == 0) {
            // includeMembers(AbstractFile.open(null, "."), p, members, false);
        } else {
            dirname = SourceRepresentation.externalizeFileName(name);
            if (!dirname.endsWith("/"))
                dirname += "/";
        }
        String[] base = global.classPath.components();
        for (int i = 0; i < base.length; i++) {
            includeMembers(
		AbstractFile.open(base[i], dirname),
		p, members, dirname != null, symFile);
	}
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
	if (!global.separate) {
	    if (f.getName().endsWith(".scala") &&
		pf.getName().endsWith(".class")) return true;
	    if (f.getName().endsWith(".class") &&
		pf.getName().endsWith(".scala")) return false;
	}
	return f.lastModified() > pf.lastModified();
    }

    /** read directory of a classpath directory and include members
     *  in package/module scope
     */
    protected void includeMembers(AbstractFile dir, Symbol p, Scope locals,
				  boolean inclClasses, HashMap symFile) {
        if (dir == null)
            return;
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
		        // enter module, except for scala.Object class
		        // todo: why not there also?.
		        if (!(n == Names.Object.toTypeName() &&
			      p.fullName().toTermName() == Names.scala)) {
			    Scope.Entry e = locals.lookupEntry(clazz.module().name);
			    if (e != Scope.Entry.NONE) {
			        // we already have a package of the same name; delete it
			        locals.unlink(e);
			    }
			    locals.enter(clazz.module());
		        }
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

/*
		} else if (inclClasses && fname.endsWith(".symbl")) {
		    //todo: compare dates between symbl and scala.
                    Name n = Name.fromString(fname.substring(0, fname.length() - 6))
			.toTypeName();
		    Symbol sym = locals.lookup(n);
		    if (sym == Symbol.NONE ||
			sym.isPackage() ||
			sym.rawInfoAt(Symbol.FIRST_ID) instanceof ClassParser &&
			!(sym.rawInfoAt(Symbol.FIRST_ID) instanceof SymblParser)) {
			ClassSymbol clazz = new ClassSymbol(n, p, symblCompletion);
			//todo: needed
			clazz.allConstructors().setInfo(symblCompletion);
			clazz.module().setInfo(symblCompletion);
			locals.enter(clazz);
			locals.enter(clazz.module());
		    }
*/
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
                }
            }
        } catch (IOException e) {
        }
    }
}
