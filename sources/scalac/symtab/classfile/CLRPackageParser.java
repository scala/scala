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

import scala.tools.util.AbstractFile;
import scala.tools.util.ByteArrayFile;
import scala.tools.util.VirtualDirectory;

import scalac.Global;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;

import ch.epfl.lamp.compiler.msil.Type;
import ch.epfl.lamp.compiler.msil.Attribute;

/**
 * Package/namespace member loader for the CLR.
 */
public final class CLRPackageParser extends PackageParser {

    //########################################################################
    // Private Constants

    /** An empty directory */
    private static final AbstractFile EMPTY = new VirtualDirectory("<empty>");

    //########################################################################
    // Protected Fields

    /** A table to collect types */
    protected final HashMap types = new HashMap();

    //########################################################################
    // Public Constructors

    public CLRPackageParser(Global global, AbstractFile directory) {
        super(global, directory);
    }

    //########################################################################
    // Protected Methods

    protected PackageParser newPackageParser(AbstractFile directory) {
        return new CLRPackageParser(global, directory);
    }

    protected void collectAllMembers(Symbol clasz) {
        super.collectAllMembers(clasz);
        HashSet namespaces = new HashSet();
        CLRTypes.instance().collectMembers(clasz, types, namespaces);
        for (Iterator i = namespaces.iterator(); i.hasNext(); ) {
            String namespace = (String)i.next();
            if (!packages.containsKey(namespace))
                packages.put(namespace, EMPTY);
        }
    }

    protected void removeHiddenMembers(Symbol clasz) {
        // Ignore all ".symbl" and ".class" files.
        symbols.clear();
        classes.clear();
        super.removeHiddenMembers(clasz);
        // Classes/Objects in the root package are hidden.
        if (clasz.isRoot()) { types.clear(); }
        // Source versions hide compiled versions except if separate
        // compilation is enabled and the compiled version is more
        // recent. In that case the compiled version hides the source
        // version.
        boolean separate = global.separate;
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            if (types.isEmpty()) break;
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            Type type = (Type)types.get(name);
            boolean hidden = false;
            if (type != null)
                if (separate /* !!! && type.Assembly().getFile().lastModified() > sfile.lastModified() */)
                    hidden = true;
                else
                    types.remove(name);
            if (hidden) i.remove();
        }
        // Packages are hidden by classes/objects with the same name.
        packages.keySet().removeAll(types.keySet());
    }

    protected Scope createMemberSymbols(Symbol clasz) {
        CLRTypes clrTypes = CLRTypes.instance();
        String namespace = clrTypes.getNameSpaceOf(clasz);
        Scope members = super.createMemberSymbols(clasz);

        // import the CLR types contained in the package (namespace)
        for (Iterator i = types.values().iterator(); i.hasNext(); ) {
            Type type = (Type)i.next();

            assert namespace.equals(type.Namespace)
                : Debug.show(clasz, namespace) + " << " + type.FullName;
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
            Symbol clazz = clasz.newLoadedClass
                (JAVA, classname, loader, members);
            clrTypes.map(clazz, type);
            //Type moduleType = getType(type.FullName + "$");
            //map(clazz, moduleType != null ? moduleType : type);
        }

        return members;
    }

    protected String doComplete(Symbol root) {
        String base = super.doComplete(root);
        base = directory == EMPTY ? "" : base + " and ";
        String namespace = CLRTypes.instance().getNameSpaceOf(root);
        return base + "namespace '" + namespace + "'";
    }

    //########################################################################
}  // CLRPackageParser
