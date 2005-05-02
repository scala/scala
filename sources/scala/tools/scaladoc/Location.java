/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
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
//     static public URI getURI(Symbol sym) {
// 	if (uris.get(sym) == null) {
// 	    URI uri;
// 	    try {
// 		if (sym.isModuleClass())
// 		    uri = getURI(sym.module());
// 		else if (sym.isRoot() || sym.isClass() || sym.isModule() || sym.isPackage() || sym.isPackageClass())
// 		    uri = new URI(getPath(sym).toString() + HTML_SUFFIX);
// 		else if (sym.isParameter())
// 		    uri = getURI(sym.classOwner());
// 		else
// 		    uri = new URI(getURI(sym.owner()).toString() + "#" + nextFreeId(sym.owner()));
// 		uris.put(sym, uri);
// 	    } catch(Exception e) { throw Debug.abort(sym.defString()); }
// 	}
// 	return (URI) uris.get(sym);
//     }
    static public URI getURI(Symbol sym) {
	if (uris.get(sym) == null) {
	    URI uri = null;
	    try {
		if (sym.isModule())
		    uri = getURI(sym.moduleClass());
		else if (sym.isClassType())
		    uri = new URI(getPath(sym).toString() + HTML_SUFFIX);
		else if (sym.isParameter())
		    uri = getURI(sym.classOwner());
		else
		    uri = new URI(getURI(sym.owner()).toString() + "#" + nextFreeId(sym.owner()));
		uris.put(sym, uri);
	    } catch (Exception e) { throw Debug.abort(sym.defString()); }
	}
	return (URI) uris.get(sym);
    }
    // where
    static private URI getPath(Symbol sym) {
        assert sym.isClassType(): Debug.show(sym);
	try {
	    if (sym.isRoot())
		return new URI(ROOT_NAME);
	    else if (sym.owner().isRoot())
		return new URI(getName(sym));
	    else {
	        // !!! separator character in URI paths is '/'
		return new URI(getPath(sym.owner()).toString() + "/" + getName(sym));
            }
	} catch(Exception e) { return null; }
    }
    // where
    static public String getName(Symbol sym) {
        if (sym.isModule()) return getName(sym.moduleClass());
        assert sym.isClassType(): Debug.show(sym);
        String name = sym.simpleName().toString();
        if (!sym.isRoot() && !sym.isModuleClass()) name += CLASS_SUFFIX;
        return name;
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
	}
        catch(Exception e) {
            throw Debug.abort(e);
        }
    }

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
	    /// !!! separator character in URI paths is '/'
	    String uriPath = asSeenFrom(f_u, f_v).getPath().replace('\\', '/');
	    return
		new URI(uriPath
			+ (u.getFragment() != null ? "#" + u.getFragment() : ""))
		.normalize();
	}
	catch (Exception e) {
	    throw Debug.abort(e);
        }
    }
    // where
    /** f must be of the form ('.' | ['.' '/' ] A/...) */
    static private File pathToRoot(File f) {
	if (f.equals(hereFile) || f.getParentFile() == null)
	    return new File(".");
	else
	    return new File(pathToRoot(f.getParentFile()), "..");
    }
    // where
    /*
    // where
    static private File asSeenFrom(File f1, File f2) {
	return new File(pathToRoot(f2), f1.getPath());
    }
    */
    /** Compute the minimal path from one file to another.
     * f1 and f2 should be of the form (A/...)
     */
    static private File asSeenFrom(File f1, File f2) {
	if (f1.equals(f2))
	    return new File(f1.getName());
	else {
	    File lcp = longestCommonPrefix(f1, f2);
	    File relf1 = subtractPrefix(lcp, f1);
	    File relf2 = subtractPrefix(lcp, f2);
	    return new File(pathToRoot(relf2), relf1.getPath());
	}
    }
    // where
    /** p and q should be of the form (A/...) */
    static private File longestCommonPrefix(File p, File q) {
	if (prefixes(p, q))
	    return p;
	else if (p.getParentFile() == null)
	    return new File(".");
	else
	    return longestCommonPrefix(p.getParentFile(), q);
    }
    // where
    /** Test if p is a prefix of q.
     * p and q should be of the form (A/...)
     */
    static private boolean prefixes(File p, File q) {
	if (p.equals(q))
	    return true;
	else if (q.getParentFile() == null)
	    return false;
	else
	    return prefixes(p, q.getParentFile());
    }
    // and
    static private File hereFile = new File(".");
    static private File subtractPrefix(File p, File f) {
	if (p.equals(hereFile))
	    return f;
	else
	    return subtractPrefix(p, f, hereFile);
    }
    static private File subtractPrefix(File p, File f, File acc) {
	if (p.equals(f))
	    return acc;
	else if (f.getParentFile() == null)
	    return null;
	else
	    return subtractPrefix(p, f.getParentFile(),
			    (acc.equals(hereFile) ?
			     new File(f.getName()) :
			     new File(f.getName(), acc.getPath())));
    }

    static protected URI mkURI(String uri) {
	try {
	    return new URI(uri);
	}
        catch (Exception e) {
            throw Debug.abort(e);
        }
    }

    public static void main(String[] args) {
	File p = new File("A", "B");
	File q = new File("A", new File("B", "C").getPath());
	File r = new File("C");
	System.out.println("" + longestCommonPrefix(p, q));
	System.out.println("" + longestCommonPrefix(p, r));
	System.out.println("" + subtractPrefix(longestCommonPrefix(p, r), p));

	System.out.println("" + asSeenFrom(q, p));
	System.out.println("" + asSeenFrom(p, r));
    }

}
