/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import scala.tools.util.AbstractFile;

import scalac.Global;
import scalac.symtab.Scope;
import scalac.symtab.SourceCompleter;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.SymbolOrigin;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Debug;

/**
 * This class implements a package member loader. It can be used to
 * complete package class symbols.
 */
public class PackageParser extends SymbolLoader {

    //########################################################################
    // Protected Fields

    /** The directory to read */
    protected final AbstractFile directory;

    /** A table to collect .scala files */
    protected final HashMap/*<String,AbstractFile>*/ sources = new HashMap();
    /** A table to collect .symbl files */
    protected final HashMap/*<String,AbstractFile>*/ symbols = new HashMap();
    /** A table to collect .class files */
    protected final HashMap/*<String,AbstractFile>*/ classes = new HashMap();
    /** A table to collect subdirectories */
    protected final HashMap/*<String,AbstractFile>*/ packages = new HashMap();

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public PackageParser(Global global, AbstractFile directory) {
        super(global);
        this.directory = directory;
        assert directory != null;
    }

    //########################################################################
    // Protected Methods

    /** Is the given name a valid input file base name? */
    protected boolean isValidName(String name) {
        return name.length() > 0
            &&!name.endsWith("$class")
            && name.indexOf("$$anon") == -1;
    }

    /** Returns a new package parser for the given directory. */
    protected PackageParser newPackageParser(AbstractFile directory) {
        return new PackageParser(global, directory);
    }

    /**
     * Collects all members of the package. This method is invoked by
     * method "doComplete". It should not be invoked otherwise.
     */
    protected void collectAllMembers(Symbol clasz) {
        for (Iterator i = directory.list(); i.hasNext(); ) {
            AbstractFile file = (AbstractFile)i.next();
            String filename = file.getName();
            if (file.isDirectory()) {
                if (filename.equals("META-INF")) continue;
                packages.put(filename, file);
                continue;
            }
            if (filename.endsWith(".class")) {
                String name = filename.substring(0, filename.length() - 6);
                if (!isValidName(name)) continue;
                if (!classes.containsKey(name)) classes.put(name, file);
                continue;
            }
            if (filename.endsWith(".symbl")) {
                String name = filename.substring(0, filename.length() - 6);
                if (!isValidName(name)) continue;
                if (!symbols.containsKey(name)) symbols.put(name, file);
                continue;
            }
            if (filename.endsWith(".scala")) {
                String name = filename.substring(0, filename.length() - 6);
                if (!isValidName(name)) continue;
                if (!sources.containsKey(name)) sources.put(name, file);
                continue;
            }
	}
    }

    /**
     * Removes from the members collected by "collectAllMembers" all
     * those that are hidden. This method is invoked by method
     * "doComplete". It should not be invoked otherwise.
     */
    protected void removeHiddenMembers(Symbol clasz) {
        // Classes/Objects in the root package are hidden.
        if (clasz.isRoot()) sources.clear();
        if (clasz.isRoot()) symbols.clear();
        if (clasz.isRoot()) classes.clear();
        // For all files "<N>.class" find the longest M such that
        // there is a file "<M>.symbl" and M equals N or "<M>$" is a
        // prefix of N. If the file "<N>.class" is less recent than
        // the file "<M>.symbl" ignore the ".class" file. Otherwise,
        // if M equals N, ignore the ".symbl" file.
        for (Iterator i = classes.entrySet().iterator(); i.hasNext(); ) {
            if (symbols.isEmpty()) break;
            Entry entry = (Entry)i.next();
            String cname = (String)entry.getKey();
            AbstractFile cfile = (AbstractFile)entry.getValue();
            for (String zname = cname; true; ) {
                AbstractFile zfile = (AbstractFile)symbols.get(zname);
                if (zfile != null) {
                    if (cfile.lastModified() <= zfile.lastModified()) {
                        i.remove();
                    } else if (zname == cname) {
                        symbols.remove(zname);
                    }
                    break;
                }
                int index = zname.lastIndexOf('$');
                if (index < 0) break;
                zname = zname.substring(0, index);
            }
        }
        // Source versions hide compiled versions except if separate
        // compilation is enabled and the compiled version is more
        // recent. In that case the compiled version hides the source
        // version.
        boolean separate = global.separate;
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            if (symbols.isEmpty() && classes.isEmpty()) break;
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            AbstractFile zfile = (AbstractFile)symbols.get(name);
            AbstractFile cfile = (AbstractFile)classes.get(name);
            boolean hidden = false;
            if (zfile != null)
                if (separate && zfile.lastModified() > sfile.lastModified())
                    hidden = true;
                else
                    symbols.remove(name);
            if (cfile != null)
                if (separate && cfile.lastModified() > sfile.lastModified())
                    hidden = true;
                else
                    classes.remove(name);
            if (hidden) i.remove();
        }
        // Packages are hidden by classes/objects with the same name.
        packages.keySet().removeAll(sources.keySet());
        packages.keySet().removeAll(symbols.keySet());
        packages.keySet().removeAll(classes.keySet());
    }

    /**
     * Creates symbols for all members left by method
     * "removeHiddenMembers". This method is invoked by method
     * "doComplete". It should not be invoked otherwise.
     */
    protected Scope createMemberSymbols(Symbol clasz) {
        Scope members = new Scope();
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new SourceCompleter(global, sfile);
            SymbolOrigin origin = SymbolOrigin.ScalaFile(sfile);
            clasz.newLoadedClass(0, classname, loader, members, origin);
        }
        for (Iterator i = symbols.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile zfile = (AbstractFile)entry.getValue();
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new SymblParser(global, zfile);
            SymbolOrigin origin = SymbolOrigin.SymblFile(zfile);
            clasz.newLoadedClass(0, classname, loader, members, origin);
        }
        for (Iterator i = classes.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile cfile = (AbstractFile)entry.getValue();
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new ClassParser(global, cfile);
            SymbolOrigin origin = SymbolOrigin.ClassFile(cfile, null);
            clasz.newLoadedClass(JAVA, classname, loader, members, origin);
        }
        for (Iterator i = packages.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile dfile = (AbstractFile)entry.getValue();
            Name packagename = Name.fromString(name);
            SymbolLoader loader = newPackageParser(dfile);
            SymbolOrigin origin = SymbolOrigin.Directory(dfile);
            clasz.newLoadedPackage(packagename, loader, members, origin);
        }
        return members;
    }

    /** Completes the package symbol by loading all its members. */
    protected String doComplete(Symbol root) {
        assert root.isRoot() || root.isPackage(): Debug.show(root);
        Symbol clasz = root.isRoot() ? root : root.moduleClass();
        collectAllMembers(clasz);
        removeHiddenMembers(clasz);
        Scope members = createMemberSymbols(clasz);
        clasz.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, clasz));
        return "directory path '" + directory + "'";
    }

    //########################################################################
}
