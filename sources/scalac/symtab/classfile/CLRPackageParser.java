/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import scala.tools.util.AbstractFile;
import scala.tools.util.ByteArrayFile;

import scalac.Global;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Modifiers;
import scalac.symtab.SymbolLoader;
import scalac.symtab.SourceCompleter;
import scalac.symtab.SymbolNameWriter;

import ch.epfl.lamp.compiler.msil.Type;
import ch.epfl.lamp.compiler.msil.Attribute;

/**
 * Package/namespace member loader for the CLR.
 */
public final class CLRPackageParser extends PackageParser {

    //##########################################################################

    public CLRPackageParser(Global global, AbstractFile directory) {
        super(global, directory);
    }

    private final SymbolNameWriter snw = new SymbolNameWriter();

    protected String doComplete(Symbol root) {
        assert root.isRoot() || root.isPackage(): Debug.show(root);
        Symbol peckage = root.isRoot() ? root : root.moduleClass();
        if (directory != null)
            preInitialize(peckage, false);

        final CLRTypes clrTypes = CLRTypes.instance();
        java.util.Map types = clrTypes.getTypes(peckage);
        Set namespaces = clrTypes.getNamespaces(peckage);

        // create JVM and source members
        Scope members = new Scope();
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            Type type = (Type)types.remove(name);
            if (global.separate && type != null
                /*&& type.Assembly().getFile().lastModified() > sfile.lastModified()*/
                )
            {
                types.put(name, type);
            } else {
                packages.remove(name);
                Name classname = Name.fromString(name).toTypeName();
                SymbolLoader loader = new SourceCompleter(global, sfile);
                peckage.newLoadedClass(0, classname, loader, members);
            }
        }

        // import the CLR types contained in the package (namespace)
        for (Iterator i = types.values().iterator(); i.hasNext(); ) {
            Type type = (Type)i.next();

            // discard top level types
            if (type.Namespace.equals("")) {
                Global.instance.operation("Ignoring top-level type " + type);
                continue;
            }

            assert snw.toString(peckage).equals(type.Namespace)
                : Debug.show(peckage) + " << " + type.FullName;
            AbstractFile symfile = null;
            if (type.IsDefined(clrTypes.SCALA_SYMTAB_ATTR, false)) {
                Object[] attrs = type.GetCustomAttributes
                    (clrTypes.SCALA_SYMTAB_ATTR, false);
                assert attrs.length == 1 : attrs.length;
                Attribute a = (Attribute)attrs[0];
                assert a.GetType() == clrTypes.SCALA_SYMTAB_ATTR : a.toString();
                byte[] symtab = (byte[])a.getConstructorArguments()[0];
                symfile = new ByteArrayFile
                    (type.FullName, "[" + type.Assembly().GetName() + "]",
                     symtab);
            }
            SymbolLoader loader = symfile != null
                ? new SymblParser(Global.instance, symfile)
                : new CLRClassParser(Global.instance, type);

            Name classname = Name.fromString(type.Name).toTypeName();
            Symbol clazz = peckage.newLoadedClass
                (Modifiers.JAVA, classname, loader, members);
            clrTypes.map(clazz, type);
            //Type moduleType = getType(type.FullName + "$");
            //map(clazz, moduleType != null ? moduleType : type);
        }

        for (Iterator i = packages.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile dfile = (AbstractFile)entry.getValue();
            SymbolLoader loader = new CLRPackageParser(global, dfile);
            peckage.newLoadedPackage(Name.fromString(name), loader, members);
            namespaces.remove(name);
        }

        // import the CLR namespaces contained in the package (namespace)
        for (Iterator i = namespaces.iterator(); i.hasNext(); ) {
            String namespace = (String)i.next();
            Name name = Name.fromString(namespace);
            Symbol p = members.lookup(name);
            if (p == Symbol.NONE) {
                SymbolLoader loader = new CLRPackageParser(global, null);
                peckage.newLoadedPackage(name, this, members);
            } else {
                System.out.println("package already in scope: " + Debug.show(p));
            }
        }

        // initialize package
        peckage.setInfo(scalac.symtab.Type.compoundType
                        (scalac.symtab.Type.EMPTY_ARRAY, members, peckage));
        return "namespace '" + snw.toString(peckage) + "'";
    }

    //##########################################################################
}  // CLRPackageParser
