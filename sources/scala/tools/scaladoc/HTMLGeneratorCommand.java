/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.ArrayList;
import java.util.List;

import ch.epfl.lamp.util.HTMLPrinter;
import ch.epfl.lamp.util.HTMLRepresentation;

import scalac.CompilerCommand;
import scalac.util.BooleanOptionParser;
import scalac.util.ClassPath;
import scalac.util.OptionParser;
import scalac.util.Reporter;
import scalac.util.StringOptionParser;
import scalac.util.Strings;

/**
 * The class <code>HTMLGeneratorCommand</code> describes the options
 * passed as arguments to the HTML generator command.
 */
public class HTMLGeneratorCommand extends CompilerCommand {

    //########################################################################
    // Public Fields

    public final StringOptionParser docencoding;
    public final StringOptionParser docmodule;
    public final StringOptionParser docmodulePath;
    public final StringOptionParser doctitle;
    public final StringOptionParser doctype;
    public final StringOptionParser stylesheet;
    public final StringOptionParser windowtitle;

    public final BooleanOptionParser noindex;
    public final BooleanOptionParser validate;

    //########################################################################
    // Public Constructors

    /**
     * Creates an instance variable for an HTML generator command.
     *
     * @param product
     * @param version
     * @param reporter
     * @param phases
     */
    public HTMLGeneratorCommand(String product, String version,
        Reporter reporter, HTMLGeneratorPhases phases)
    {
        this(product, version, "<source files>", reporter, phases);
    }

    /**
     * Creates an instance variable for an HTML generator command.
     *
     * @param product
     * @param version
     * @param syntax
     * @param reporter
     * @param phases
     */
    public HTMLGeneratorCommand(String product, String version, String syntax,
        Reporter reporter, HTMLGeneratorPhases phases)
    {
        super(product, version, syntax, reporter, phases);

        this.docencoding = new StringOptionParser(this,
            "docencoding",
            "Output encoding name (" + HTMLRepresentation.DEFAULT_DOCENCODING + ", etc.)",
            "name",
            HTMLRepresentation.DEFAULT_DOCENCODING);

        this.docmodule = new StringOptionParser(this,
	    "docmodule",
            "Specify module used by scaladoc",
            "class",
            "scala.tools.scaladoc.StandardDocModule");

        this.docmodulePath = new StringOptionParser(this,
            "docmodulepath",
            "Specify where to find doc module class files",
            "path",
            ClassPath.CLASS_PATH);

        this.doctitle = new StringOptionParser(this,
	    "doctitle",
            "Include title for the overview page",
            "html-code",
            HTMLGenerator.DEFAULT_DOCTITLE);

        this.doctype = new StringOptionParser(this,
            "doctype",
            "Output type name (" + HTMLRepresentation.DEFAULT_DOCTYPE + ", etc.)",
            "name",
            HTMLRepresentation.DEFAULT_DOCTYPE);

        this.stylesheet = new StringOptionParser(this,
            "stylesheetfile",
            "File to change style of the generated documentation",
            "path",
            HTMLPrinter.DEFAULT_STYLESHEET);

        this.windowtitle = new StringOptionParser(this,
	    "windowtitle",
            "Browser window title for the documentation",
            "text",
            HTMLGenerator.DEFAULT_WINDOWTITLE);

        this.noindex = new BooleanOptionParser(this,
            "noindex",
            "Do not generate index",
            false);

        this.validate = new BooleanOptionParser(this,
            "validate",
            "Add a link at the bottom of each generated page",
            false);

        remove(nowarn);
        remove(verbose);
        remove(debug);
        remove(explaintypes);
        remove(uniqid);
        remove(types);
        remove(prompt);
        remove(separate);
        remove(classpath);
        remove(sourcepath);
        remove(bootclasspath);
        remove(extdirs);
        // outpath;
        remove(target);
        remove(noimports);
        remove(nopredefs);
        remove(skip);
        remove(check);
        remove(print);
        remove(printer);
        remove(printfile);
        remove(graph);
        remove(stop);
        remove(log);

        // similar order as javadoc options
        add(2, windowtitle);
        add(3, doctitle);
        add(4, noindex);
        add(5, docmodule);
        add(6, docmodulePath);
        add(7, stylesheet);
        add(8, doctype);
        add(9, docencoding);
        add(10, validate);
    }

    //########################################################################
}
