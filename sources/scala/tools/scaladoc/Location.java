/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.net.URL;
import java.net.URI;
import java.io.File;
import scalac.symtab.Symbol;
import java.util.Map;
import java.util.HashMap;
import scalac.util.Debug;

/**
 * This class manages the correspondance between a symbol and
 * its URL.
 */
public class Location {

    static public final String ROOT_NAME    = "root-page";
    static public final String HTML_SUFFIX  = ".html";
    static public final String CLASS_SUFFIX = "-class";

    /////////////////// AS SEEN FROM //////////////////////

    /**
     * Returns a representation of the URL u1 relative to u2.
     * Examples:
     *   "A/B/C"  as seen from "A"      is  "A/B/C"
     *   "A"      as seen from "A/B/C"  is  "../../A"
     *   "A/B#R"  as seen from "A"      is  "A/B#R"
    */
    static public URI asSeenFrom(URI u, URI v) {
	File f_u = new File(u.getPath());
	File f_v = new File(v.getPath());
	try {
	    return
		new URI(asSeenFrom(f_u, f_v).getPath()
			+ (u.getFragment() != null ? "#" + u.getFragment() : ""))
		.normalize();
	} catch(Exception e) { return null; }
    }
    // where
    static private File asSeenFrom(File f1, File f2) {
	    return new File(pathToRoot(f2), f1.getPath());
    }
    // where
    static private File pathToRoot(File f) {
	File parent = f.getParentFile();
	if (parent == null)
	    return new File(".");
	else
	    return new File(pathToRoot(parent), "..");
    }

    /////////////////// UNIQUE URL //////////////////////

    /** Returns the URI of a given symbol. */
    static private final Map/*<Symbol, URI>*/ uris = new HashMap();
    static public URI getURI(Symbol sym) {
	if (uris.get(sym) == null) {
	    URI uri;
	    try {
		if (sym.isModuleClass())
		    uri = getURI(sym.module());
		else if (sym.isRoot() || sym.isClass() || sym.isModule() || sym.isPackage())
		    uri = new URI(getPath(sym).toString() + HTML_SUFFIX);
		else if (sym.isParameter())
		    uri = getURI(sym.classOwner());
		else
		    uri = new URI(getURI(sym.owner()).toString() + "#" + nextFreeId(sym.owner()));
		uris.put(sym, uri);
	    } catch(Exception e) { throw Debug.abort(sym.defString()); }
	}
	return (URI) uris.get(sym);
    }
    // where
    static private URI getPath(Symbol sym) {
	try {
	    if (sym.isModuleClass())
		return getPath(sym.module());
	    else if (sym.isRoot())
		return new URI(ROOT_NAME);
	    else if (sym.owner().isRoot())
		return new URI(getName(sym));
	    else
		return new URI(getPath(sym.owner()).toString() + File.separator + getName(sym));
	} catch(Exception e) { return null; }
    }
    // where
    static public String getName(Symbol sym) {
	return sym.isClass() ? sym.nameString() + CLASS_SUFFIX : sym.nameString();
    }
    // where
    static private final Map/*<Symbol, Integer>*/ ids = new HashMap();
    static private int nextFreeId(Symbol sym) {
	if (ids.get(sym) == null)
	    ids.put(sym, new Integer(0));
	int i = ((Integer) ids.get(sym)).intValue();
	ids.put(sym, new Integer(i + 1));
	return i;
    }

}
