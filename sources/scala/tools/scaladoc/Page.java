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

    /** Page URL relative to the root directory.
     */
    URI uri;

    /** Frame where to print the contents of links that appear on the
     * page.
     */
    public String destinationFrame;

    /** Build a page.
     */
    public Page(Writer writer, URI uri, String destinationFrame,
		String title, HTMLRepresentation representation,
		String stylesheet, String script) {
	super(writer, title, representation, stylesheet, script);
	this.uri = uri;
	this.destinationFrame = destinationFrame;
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
}
