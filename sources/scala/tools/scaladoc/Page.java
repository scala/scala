/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.io.File;
import java.net.URI;
import java.io.Writer;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import ch.epfl.lamp.util.XMLAttribute;
import ch.epfl.lamp.util.HTMLPrinter;
import ch.epfl.lamp.util.HTMLRepresentation;
import scalac.util.Debug;

/**
 * This class represents a web page. The user of this class has to
 * call the method <code>open</code>, then fill the page and finally
 * call the method <code>close</code>.
 */
class Page extends HTMLPrinter {

    /** Root directory.
     */
    protected File rootDirectory;

    /** Page URL relative to the root directory.
     */
    protected URI uri;

    /** Frame where to print the contents of links that appear on the
     * page.
     */
    public String destinationFrame;

    /** Build a page.
     */
    public Page(File rootDirectory, URI uri, String destinationFrame,
		String title, HTMLRepresentation representation,
		String stylesheet/*, String script*/) {
	super(getWriter(rootDirectory, uri), title, representation,
	      asSeenFrom(mkURI(stylesheet), uri).toString()/*, script*/);
	this.rootDirectory = rootDirectory;
	this.uri = uri;
	this.destinationFrame = destinationFrame;
    }

    /** Get a writer to the page.
     */
    protected static Writer getWriter(File rootDirectory, URI uri) {
	try {
	    File f = new File(rootDirectory, uri.toString());
	    f.getParentFile().mkdirs();
	    return new BufferedWriter(new FileWriter(f));
	} catch(IOException e) {
	    throw Debug.abort(e);
	}
    }

    /** Open the page.
     */
    public void open() {}

    /** Close the page.
     */
    public void close() {
	try {
	    getCodePrinter().getWriter().close();
	} catch (IOException e) {
	    throw Debug.abort(e);
	}
    }

    /** Compute a relative link.
     */
    public String rel(String url) {
	return asSeenFrom(mkURI(url), uri).toString();
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
	    return
		new URI(asSeenFrom(f_u, f_v).getPath()
			+ (u.getFragment() != null ? "#" + u.getFragment() : ""))
		.normalize();
	} catch(Exception e) { return null; }
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
	} catch(Exception e) { throw Debug.abort(e); }
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
