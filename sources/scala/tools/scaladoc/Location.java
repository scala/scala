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
    //static public final String CLASS_SUFFIX = "(class)";

    /////////////////// UNIQUE URL //////////////////////

    static public String get(Symbol sym) {
	return getURI(sym).toString();
    }
    /** Returns the URI of a given symbol. */
    static private final Map/*<Symbol, URI>*/ uris = new HashMap();
    static public URI getURI(Symbol sym) {
	if (uris.get(sym) == null) {
	    URI uri;
	    try {
		if (sym.isModuleClass())
		    uri = getURI(sym.module());
		else if (sym.isRoot() || sym.isClass() || sym.isModule() || sym.isPackage() || sym.isPackageClass())
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
	return sym.isClass() ? sym.simpleName().toString() + CLASS_SUFFIX : sym.simpleName().toString();
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

    static protected URI makeURI(String uri) {
	try {
	    return new URI(uri);
	} catch(Exception e) { throw Debug.abort(e); }
    }
}
