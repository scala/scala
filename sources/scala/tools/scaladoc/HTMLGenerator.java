/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-04, LAMP/EPFL           **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scala.tools.scaladoc;

import java.io.Writer;
import java.io.BufferedWriter;
import java.io.StringWriter;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.IOException;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;
import java.util.regex.*;

import ch.epfl.lamp.util.XMLAttribute;
import ch.epfl.lamp.util.HTMLPrinter;
import ch.epfl.lamp.util.HTMLRepresentation;
import ch.epfl.lamp.util.Pair;
import ch.epfl.lamp.util.XHTMLPrinter;

import scala.tools.util.Position;

import scalac.Global;
import scalac.Unit;
import scalac.symtab.Kinds;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.Type.*;
import scalac.symtab.SymbolTablePrinter;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Strings;
import SymbolBooleanFunction;
import scalac.util.ScalaProgramArgumentParser;

/**
 * The class <code>HTMLGenerator</code> generates
 * the HTML documentation for a given Scala library.
 */
public abstract class HTMLGenerator {

    /*
     * Names of predefined page names and page sections.
     */
    protected final String FRAME_PAGE            = "index.html";
    protected final String ROOT_PAGE             = Location.ROOT_NAME + ".html";
    protected final String PACKAGE_INDEX_PAGE     = "package-index-page.html";
    protected final String HELP_PAGE             = "help-page.html";
    protected final String SEARCH_SECTION        = "search-section";
    protected final String INDEX_PAGE            = "index-page.html";
    protected final String PACKAGE_PAGE          = "package-page.html";

    /*
     * Names of frames.
     */
    protected final String ROOT_FRAME     = "rootFrame";
    protected final String PACKAGES_FRAME = "packagesFrame";
    protected final String CLASSES_FRAME  = "classesFrame";
    protected final String SELF_FRAME     = "_self";

    /**
     * HTML DTD
     */
    protected final String[] HTML_DTD = new String[] { "xhtml1-transitional.dtd",
                                                        "xhtml-lat1.ent",
                                                        "xhtml-special.ent",
                                                        "xhtml-symbol.ent" };

    /**
     * HTML validator.
     */
    protected HTMLValidator xhtml;

    /*
     * XML attributes.
     */
    protected final XMLAttribute[] ATTRS_DOCTAG =
        new XMLAttribute[]{
            new XMLAttribute("style", "margin-top:10px;")
        };
    protected final XMLAttribute[] ATTRS_ENTITY =
        new XMLAttribute[]{ new XMLAttribute("class", "entity") };

    protected final XMLAttribute[] ATTRS_LIST =
        new XMLAttribute[] { new XMLAttribute("class", "list")};

    protected final XMLAttribute[] ATTRS_MEMBER =
        new XMLAttribute[]{
            new XMLAttribute("cellpadding", "3"),
            new XMLAttribute("class", "member")
        };
    protected final XMLAttribute[] ATTRS_MEMBER_DETAIL =
        new XMLAttribute[]{
            new XMLAttribute("cellpadding", "3"),
            new XMLAttribute("class", "member-detail")
        };

    protected final XMLAttribute[] ATTRS_MEMBER_TITLE =
        new XMLAttribute[]{ new XMLAttribute("class", "member-title") };

    protected final XMLAttribute[] ATTRS_MODIFIERS =
        new XMLAttribute[]{
            new XMLAttribute("valign", "top"),
            new XMLAttribute("class", "modifiers")
        };
    protected final XMLAttribute[] ATTRS_NAVIGATION =
        new XMLAttribute[]{ new XMLAttribute("class", "navigation") };

    protected final XMLAttribute[] ATTRS_NAVIGATION_LINKS =
        new XMLAttribute[]{
            new XMLAttribute("valign", "top"),
            new XMLAttribute("class", "navigation-links")
        };
    protected final XMLAttribute[] ATTRS_NAVIGATION_ENABLED = new XMLAttribute[]{
            new XMLAttribute("class", "navigation-enabled") };

    protected final XMLAttribute[] ATTRS_NAVIGATION_SELECTED = new XMLAttribute[]{
            new XMLAttribute("class", "navigation-selected") };

    protected final XMLAttribute[] ATTRS_NAVIGATION_PRODUCT =
        new XMLAttribute[]{
            new XMLAttribute("align", "right"),
            new XMLAttribute("valign", "top"),
            new XMLAttribute("style", "white-space:nowrap;"),
            new XMLAttribute("rowspan", "2")
        };
    protected final XMLAttribute[] ATTRS_PAGE_TITLE = new XMLAttribute[]{
            new XMLAttribute("class", "page-title")
        };
    protected final XMLAttribute[] ATTRS_SIGNATURE =
        new XMLAttribute[]{ new XMLAttribute("class", "signature") };

    protected final XMLAttribute[] ATTRS_TITLE_SUMMARY =
        new XMLAttribute[]{
            new XMLAttribute("colspan", "2"),
            new XMLAttribute("class", "title")
        };
    protected final XMLAttribute[] ATTRS_VALIDATION =
        new XMLAttribute[]{
            new XMLAttribute("style", "margin-top:5px; text-align:center; font-size:9pt;")
        };

    /** HTML meta information.
     */
    public static final String PRODUCT =
        System.getProperty("scala.product", "scaladoc");
    public static final String VERSION =
        System.getProperty("scala.version", "unknown version");
    protected final String GENERATOR = PRODUCT + " (" + VERSION + ")";
    protected final SimpleDateFormat df = new SimpleDateFormat("EEE MMM d HH:mm:ss z yyyy");
    protected final XMLAttribute[] ATTRS_META =
        new XMLAttribute[]{ new XMLAttribute("generator", GENERATOR) };
    protected String getGenerator() {
    	return "Generated by " + GENERATOR + " on " + df.format(new Date());
    }

    /** Global compiler environment.
     */
    protected final Global global;

    /** Directory where to put generated HTML pages.
     */
    protected File directory;

    /** Comments associated with symbols.
     */
    protected Map/*<Symbol, Comment>*/ comments = new HashMap();

    /** The underlying HTML printer.
     */
    public Page page;

    /** The current URI.
     */
    protected URI uri;

    /**
     * The underlying symbol table printer.
     */
    protected MySymbolTablePrinter symtab;

    /**
     * The underlying document representation of the generated documentation.
     */
    protected HTMLRepresentation representation;

    /**
     * The command option settings.
     */
    protected String windowtitle;
    protected String doctitle;
    protected String stylesheet;
    protected boolean noindex;
    protected boolean validate;
    protected boolean launchServer;
    protected int port;

    /**
     * HTML pages may be generated recursively,
     * so we need to save active printers.
     */
    protected final Stack stack = new Stack();

    /**
     * Navigation context.
     */
    private final int ROOT_NAV_CONTEXT   = 0; // on the root page
    private final int INDEX_NAV_CONTEXT  = 1; // on the index page
    private final int HELP_NAV_CONTEXT   = 2; // on the help page
    private final int CONTAINER_NAV_CONTEXT = 3; // on a container page different from the root.

    /**
     * Variables used when loading this documentation generator.
     */
    public static final String DEFAULT_DOCTITLE = "";
    public static final String DEFAULT_WINDOWTITLE = "Generated Documentation";

    /** Root scope.
     */
    protected final Symbol root;

    /** Documented Symbols.
     */
    protected SymbolBooleanFunction isDocumented;

    /** ML module factory.
     */
    public abstract TypeIsomorphism newTypeIso(Global global);

    /** Page promises: Map[String, Promise].
     */
    public Map promises;

    /**
     * Creates a new instance.
     *
     * @param global
     */
    protected HTMLGenerator(Global global) {
	this.global = global;
	this.root = global.definitions.ROOT_CLASS;
	this.uri = Location.makeURI(".");
        this.promises = new HashMap();

        assert global.args instanceof HTMLGeneratorCommand;
        HTMLGeneratorCommand args = (HTMLGeneratorCommand) global.args;
        this.representation = new HTMLRepresentation(
            args.doctype.value,
            args.docencoding.value,
            HTMLRepresentation.DEFAULT_DOCLANGUAGE);
        this.windowtitle = args.windowtitle.value;
        this.doctitle = args.doctitle.value;
        this.stylesheet = args.stylesheet.value;
        this.noindex = args.noindex.value;
        this.validate = args.validate.value;
        this.launchServer = args.server.value;
        try {
            this.port = Integer.parseInt(args.port.value);
        }
        catch (NumberFormatException e) {
            this.port = 1280;
        }
        Symbol[] packages = getPackages(args.packages);
        final DocSyms docSyms = new DocSyms(global, packages);
        this.isDocumented = new SymbolBooleanFunction() {
		public boolean apply(Symbol sym) {
		    return docSyms.contains(sym) && ScalaSearch.isRelevant(sym)
                        && !getComment(sym).containsTag("@ignore");
		}
	    };
    }

    /**
     * Adapt the URI to the use context. If the usage context is the
     * server, then the URI for the page <code>index.html</code> has
     * the following shape: <pre>pageServlet?page=index.html</pre>. If
     * the usage context is a static page, then the URI is given
     * relative to the given page: <pre>../../index.html</pre>
     */
    public URI adaptURI(URI uri, URI pageUri) {
        if (launchServer)
            return Location.mkURI(PAGE_SERVLET_NAME + "?" +"page=" + uri.getPath() + "#" + uri.getFragment());
        else
            return Location.asSeenFrom(uri, pageUri);
    }

    /**
     * Compute the URI for a symbol page. The URI is expressed
     * relatively to the given page.
     */
    protected URI definitionURI(Symbol sym, Page page) {
        URI defUri = Location.getURI(sym);
        return adaptURI(defUri, page.uri);
    }

    /**
     * Compute the URI for a symbol page. The URI is expressed
     * relatively to the current page.
     */
    protected URI definitionURI(Symbol sym) {
        return definitionURI(sym, page);
    }

    /** Get the list pf packages to be documented.
     */
    protected Symbol[] getPackages(ScalaProgramArgumentParser option) {
	if (option.main != null) {
	    Symbol[] packages = new Symbol[option.args.length + 1];
	    packages[0] = global.definitions.getModule(option.main);
	    for(int i = 0; i < option.args.length; i++)
		packages[i+1] = global.definitions.getModule(option.args[i]);
	    return packages;
	}
	else
	    return new Symbol[] { root };
    }

    /** Get a file writer to a page.
     */
    protected static Writer fileWriter(File rootDirectory, String uri) {
        try {
            File f = new File(rootDirectory, uri);
            f.getParentFile().mkdirs();
            return new BufferedWriter(new FileWriter(f));
        } catch(IOException e) { throw Debug.abort(e); }
    }

    /**
     * Open a new documentation page and make it the current page.
     * @param uri   URL of the page
     * @param title Title of the page
     */
    protected void createPrinters(Writer writer, URI uri, String title, String destinationFrame) {
	stack.push(page);
	stack.push(symtab);
	// Create a new page.
	page = new Page(writer, uri, destinationFrame,
			title, representation,
                        adaptURI(Location.mkURI(HTMLPrinter.DEFAULT_STYLESHEET), uri).toString(),
                        adaptURI(Location.mkURI(HTMLPrinter.DEFAULT_JAVASCRIPT), uri).toString());
	// Create a printer to print symbols and types.
	symtab = SymbolTablePrinterFactory.makeHTML(this, page, isDocumented);
	page.open();
    }

    /**
     * Close the current page.
     */
    protected void closePrinters() {
        //page.close();
        symtab = (MySymbolTablePrinter) stack.pop();
        page = (Page) stack.pop();
    }

    /**
     * Check if the outpath is valid.
     */
    private boolean checkOutpath()  {
        String text = "Output path \"" + global.outpath + "\" ";
        boolean ok = false;
        try {
            directory = new File(global.outpath);
            if (! directory.exists())
                global.reporter.error(null, text + "does not exist");
            else if (! directory.isDirectory())
                global.reporter.error(null, text + "is not a directory");
            else if (! directory.canWrite())
                global.reporter.error(null, text + "cannot be modified");
            else
                ok = true;
        } catch (NullPointerException e) {
            global.reporter.error(null, e.getMessage());
        }
        return ok;
    }

    /**
     * Main function.
     */
    public void apply() {

        if (!launchServer) {
            if (! checkOutpath())
                return;
        }

        this.xhtml = new HTMLValidator(getResourceURL(HTML_DTD[0]));

        // Page with list of packages
        Promise packageIndexPage = new PackageIndexPromise();
        promises.put(packageIndexPage.name(), packageIndexPage);

        // Class and object pages
        ScalaSearch.foreach(root,
			    new ScalaSearch.SymFun() {
				public void apply(Symbol sym) {
				    if (ScalaSearch.isContainer(sym) &&
					isDocumented.apply(sym)) {
                                        Promise containerPage = new ContainerPromise(sym);
                                        promises.put(containerPage.name(), containerPage);
 					if (sym.isPackage() || sym.isPackageClass()) {
                                            Promise containerIndexPage = new ContainerIndexPromise(sym);
                                            promises.put(containerIndexPage.name(), containerIndexPage);
                                        }
				    }
				}
			    }
			    );

	if (!noindex) {
            // Page with index of Scala documented entities
            Promise indexPage = new IndexPromise();
            promises.put(indexPage.name(), indexPage);
        }

        // Help page
        Promise helpPage = new HelpPromise();
        promises.put(helpPage.name(), helpPage);

	// Frame description page
        Promise framePage = new FramePromise();
        promises.put(framePage.name(), framePage);

        // Style sheet
        Promise styleSheetPage = new ResourcePromise(HTMLPrinter.DEFAULT_STYLESHEET, null);
        promises.put(styleSheetPage.name(), styleSheetPage);

        // Script
        Promise javaScriptPage = new ResourcePromise(HTMLPrinter.DEFAULT_JAVASCRIPT, null);
        promises.put(javaScriptPage.name(), javaScriptPage);

        if (launchServer) {
            // Launch HTTP server
            launchServer();
        }
        else {
            // Generate statically all the pages
            Iterator i = promises.values().iterator();
            while(i.hasNext()) {
                Promise pagePromise = (Promise) i.next();
                Writer writer = fileWriter(directory, pagePromise.name());
                pagePromise.writeOn(writer);
                try {
                    writer.close();
                } catch(IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Launch the HTTP server.
     */
    protected void launchServer() {
        Servlet pageServlet = new PageServlet();
        Servlet searchServlet = new SearchServlet();
        Servlet[] servlets = new Servlet[]{ pageServlet, searchServlet };
        try {
            HTTPServer webServer = new HTTPServer(directory, port, servlets);
            webServer.start();
        }
        catch (IOException e) {
            System.out.println("Server could not start because of an "
                               + e.getClass());
            System.out.println(e);
        }
        // to prevent going to the next phase (implies checking
        // errors when parsing a type after RefCheck)
        try {
            synchronized(this) { wait(); }
        }
        catch(InterruptedException e) {
            System.err.println("Error while waiting.");
            System.exit(0);
        }
    }

    /**
     * Returns the comment associated with a given symbol.
     *
     * @param sym
     */
    protected Comment getComment(Symbol sym) {
	Comment comment = (Comment) comments.get(sym);
	if (comment == null) {
            Pair p = (Pair) global.mapSymbolComment.get(sym);
            if (p != null) {
                String s = (String) p.fst;
                Unit unit = (Unit) p.snd;
                comment = new Comment(s, sym, unit, xhtml);
            }
            else { // comment inheritance
                Symbol overriden = ScalaSearch.overridenBySymbol(sym);
                if (overriden == Symbol.NONE)
                    comment = new Comment(null, sym, null, xhtml);
                else
                    comment = getComment(overriden);
                //s = "/** (Inherited comment) " + getComment(overriden).rawText + "*/";
            }
            comments.put(sym, comment);
	}
	return comment;
    }

    /**
     * Filters modifiers so that modifiers added by the analyzer are
     * not printed.
     */
    protected String filterModifiers(Symbol sym) {
        int flags = sym.flags;
        if (sym.isPackage() || sym.isPackageClass()) {
            if ((flags & Modifiers.FINAL) != 0)
                flags = flags - Modifiers.FINAL;
        }
        if (sym.isModule()) {
            if ((flags & Modifiers.FINAL) != 0)
                flags = flags - Modifiers.FINAL;
        }
        if (sym.isTrait()) {
            if ((flags & Modifiers.ABSTRACT) != 0)
                flags = flags - Modifiers.ABSTRACT;
            if ((flags & Modifiers.INTERFACE) != 0)
                flags = flags - Modifiers.INTERFACE;
        }
        return Modifiers.Helper.toString(flags);
    }

    /**
     * Generates a HTML page for a class or object definition.
     */
    class ContainerPromise  extends Promise {

        protected Symbol sym;

        public ContainerPromise(Symbol sym) {
            this.sym = sym;
        }

        public String name() { return Location.getURI(sym).toString(); }

        protected void computeIn(Writer writer) {
            String title = Location.getName(sym);
            createPrinters(writer, Location.getURI(sym), title, SELF_FRAME);
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();

            if (sym.isRoot())
                addNavigationBar(ROOT_NAV_CONTEXT);
            else
                addNavigationBar(CONTAINER_NAV_CONTEXT);
            page.printlnHLine();

            addTitle(sym);
            addDocumentationComment(sym);
            page.printlnHLine();

            String[] titles = new String[]{ "Field", "Method", "Object",
                                            "Trait", "Class", "Package" }; // "Constructor"
            String[] inherited = new String[]{ "Fields", "Methods", "Objects",
                                               "Traits", "Classes", "Packages" };
            Symbol[][] members =
                ScalaSearch.splitMembers(ScalaSearch.members(sym, isDocumented));
            for (int i = 0; i < members.length; i++) {
                addMemberSummary(members[i], titles[i] + " Summary");
                if (i == 1) addInheritedMembers(sym, inherited[i]);
            }
            for (int i = 0; i < titles.length; i++)
                addMemberDetail(members[i], titles[i] + " Detail");

            page.printlnHLine();
            if (sym.isRoot())
                addNavigationBar(ROOT_NAV_CONTEXT);
            else
                addNavigationBar(CONTAINER_NAV_CONTEXT);
            if (validate)
                addValidationBar();

            page.printFootpage();
            closePrinters();
        }
    }

    /**
     * Writes the product name and version to the current page.
     *
     * @param attrs
     */
    protected void addDocumentationTitle(XMLAttribute[] attrs, Page page) {
        page.printlnOTag("div", attrs).indent();
        page.println(doctitle).undent();
        page.printlnCTag("div");
    }

    protected void addDocumentationTitle(XMLAttribute[] attrs) {
        addDocumentationTitle(attrs, page);
    }

    protected void addSearchSection(Page page) {
	page.printlnOTag("form", new XMLAttribute[] {
            new XMLAttribute("action", "/" + SEARCH_SERVLET_NAME),
            new XMLAttribute("method", "get") }).indent();

        page.printlnOTag("table", new XMLAttribute[] {
            new XMLAttribute("border", "0") }).indent();

        // Text field
        page.printlnOTag("tr").indent();
        page.printlnOTag("td").indent();
        page.printlnSTag("input", new XMLAttribute[] {
            new XMLAttribute("name", "searchString"),
            new XMLAttribute("id", "word"),
            new XMLAttribute("size", "100%"),
            // new XMLAttribute("maxlength", "30"),
            new XMLAttribute("type", "text"),
        });
        // Button
	page.printlnSTag("input", new XMLAttribute[] {
            new XMLAttribute("type", "submit"),
            new XMLAttribute("value", "Search"),
        }).undent();
        page.printlnCTag("td").undent();
	page.printlnCTag("tr");

        page.printlnOTag("tr").indent();
        page.printlnOTag("td", new XMLAttribute[] {
            new XMLAttribute("align", "center") }).indent();
        // by name
	page.printlnSTag("input", new XMLAttribute[] {
            new XMLAttribute("type", "radio"),
            new XMLAttribute("checked", "true"),
            new XMLAttribute("name", "searchKind"),
            new XMLAttribute("id", "byName"),
            new XMLAttribute("value", "byName")
        });
        page.println("By name");
        // by comment
	page.printlnSTag("input", new XMLAttribute[] {
            new XMLAttribute("type", "radio"),
            new XMLAttribute("name", "searchKind"),
            new XMLAttribute("id", "byComment"),
            new XMLAttribute("value", "byComment")
        });
        page.println("By comment");
        // by type
	page.printlnSTag("input", new XMLAttribute[] {
            new XMLAttribute("type", "radio"),
            new XMLAttribute("name", "searchKind"),
            new XMLAttribute("id", "byType"),
            new XMLAttribute("value", "byType")
        });
        page.println("By type").undent();
        page.printlnCTag("td").undent();
	page.printlnCTag("tr").undent();
	page.printlnCTag("table").undent();
	page.printlnCTag("form");
    }

    protected void addSearchSection() {
        addSearchSection(page);
    }

    /**
     * Writes the navigation bar to the current page.
     *
     * @param sym
     */
    protected void addNavigationBar(int navigationContext, Page page) {
	try {
	    String overviewLink = adaptURI(Location.mkURI(ROOT_PAGE), page.uri).toString();
	    String indexLink    = adaptURI(Location.mkURI(INDEX_PAGE), page.uri).toString();
	    String helpLink     = adaptURI(Location.mkURI(HELP_PAGE), page.uri).toString();

	    page.printlnOTag("table", ATTRS_NAVIGATION).indent();
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", ATTRS_NAVIGATION_LINKS).indent();
	    page.printlnOTag("table").indent();
	    page.printlnOTag("tr").indent();

	    // overview link
	    if (navigationContext == ROOT_NAV_CONTEXT)
		page.printlnTag("td", ATTRS_NAVIGATION_SELECTED, "Overview");
	    else {
		page.printOTag("td", ATTRS_NAVIGATION_ENABLED);
		page.printAhref(overviewLink, SELF_FRAME, "Overview");
		page.printlnCTag("td");
	    }
	    // index link
	    if (navigationContext == INDEX_NAV_CONTEXT)
		page.printlnTag("td", ATTRS_NAVIGATION_SELECTED, "Index");
	    else {
		page.printOTag("td", ATTRS_NAVIGATION_ENABLED);
		page.printAhref(indexLink, SELF_FRAME, "Index");
		page.printlnCTag("td");
	    }
	    // help link
	    if (navigationContext == HELP_NAV_CONTEXT)
		page.printlnTag("td", ATTRS_NAVIGATION_SELECTED, "Help");
	    else {
		page.printOTag("td", ATTRS_NAVIGATION_ENABLED);
		page.printAhref(helpLink, SELF_FRAME, "Help");
		page.printlnCTag("td");
	    }

	    page.undent();
	    page.printlnCTag("tr").undent();
	    page.printlnCTag("table").undent();
	    page.printlnCTag("td");

	    // product & version
	    page.printlnOTag("td", ATTRS_NAVIGATION_PRODUCT).indent();
	    addDocumentationTitle(new XMLAttribute[]{
                                      new XMLAttribute("class", "doctitle")}, page);
	    page.undent();
	    page.printlnCTag("td").undent();

	    page.printlnCTag("tr");

	    page.printlnOTag("tr").indent();
	    page.printlnTag("td", "&nbsp;").undent();
	    page.printlnCTag("tr").undent();
            if (launchServer) {
                page.printlnOTag("tr").indent();
                addSearchSection(page);
                page.undent();
                page.printlnCTag("tr");
            }
	    page.printlnCTag("table");
	} catch(Exception e) { throw Debug.abort(e); }
    }

    protected void addNavigationBar(int navigationContext) {
        addNavigationBar(navigationContext, page);
    }

    /**
     * Writes the validation bar to the current page.
     */
    protected void addValidationBar() {
        page.printlnOTag("div", ATTRS_VALIDATION);
        page.indent();
        page.printlnAhref(
			  "http://validator.w3.org/check/referer", SELF_FRAME,
			  "validate html");
        page.undent();
        page.printlnCTag("div");
    }

    /**
     * Writes the signature of the class or object to the current page.
     *
     * @param sym
     */
    protected void addTitle(Symbol sym) {
        if (sym.isRoot()) {
            page.printlnOTag("div", ATTRS_PAGE_TITLE).indent();
            page.println(doctitle.replaceAll("<.*>", " "));
            page.printlnSTag("br");
            page.println("API Specification").undent();
            page.printlnCTag("div");
            page.println("This document is the API specification for "
                + doctitle.replaceAll("<.*>", " ") + ".");
            page.printlnSTag("p");
        } else {
	    // in
	    page.print("in ");
	    printPath(sym.owner(), SELF_FRAME);

            // kind and name
	    page.printlnOTag("div", ATTRS_ENTITY).indent();
            page.print(symtab.getSymbolKeywordForDoc(sym) + " ");
	    page.printlnTag("span", ATTRS_ENTITY, sym.nameString()).undent();
	    page.printlnCTag("div");
	    page.printlnHLine();

	    // complete signature
	    // !!! page.println(printer().printTemplateHtmlSignature(sym, false).toString());
	    printTemplateHtmlSignature(sym, false);

	    // implementing classes or modules
	    // Maps classes to their direct implementing classes or modules
	    Map subs = ScalaSearch.subTemplates(root, isDocumented);

	    if (sym.isClass()) {
		List subList = (List) subs.get(sym);
		if (subList != null && subList.size() != 0) {
		    page.printlnOTag("dl").indent();
		    page.printlnOTag("dt");
		    page.printlnBold("Implementing classes or objects:");
		    page.printlnCTag("dt");
		    Iterator it = subList.iterator();
		    while (it.hasNext()) {
			Pair p = (Pair) it.next();
			Symbol sub = (Symbol) p.fst;
			Type tipe = (Type) p.snd;
			page.printlnOTag("dd");

			symtab.defString(sub, true /*addLink*/);
			if (sub.owner() != sym.owner()) {
                            page.print(" in ");
			    printPath(sub.owner(), SELF_FRAME);
                        }
			page.printlnCTag("dd");
		    }
                    page.undent();
		    page.printlnCTag("dl");
		}
	    }
 	}
    }

    /**
     * Writes a documentation comment to the current page.
     *
     * @param sym
     */
    protected void addDocumentationComment(Symbol sym) {
	Comment comment = getComment(sym);
	if (!comment.isEmpty()) {
	    page.printlnHLine();
	    addComments(comment);
	}
    }

    /**
     * Writes a sorted list of all members with a short summary
     * for each one.
     *
     * @param members
     * @param title
     */
    protected void addMemberSummary(Symbol[] members, String title) {
	if (members.length > 0) {
	    Symbol[] sortedMembers = new Symbol[members.length];
	    for (int i = 0; i < members.length; i++) {
		assert members[i] != null : "HA ENFIN !";
		sortedMembers[i] = members[i];
	    }
	    Arrays.sort(sortedMembers, ScalaSearch.symAlphaOrder);

	    // open table
	    page.printlnOTag("table", ATTRS_MEMBER).indent();

	    // title
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", ATTRS_TITLE_SUMMARY).indent();
	    page.println(title).undent();
	    page.printlnCTag("td").undent();
	    page.printlnCTag("tr");

	    // members
	    for (int i = 0; i < members.length; i++)
		addMemberSummary(sortedMembers[i]);

	    // close table
            page.undent();
	    page.printlnCTag("table");
	    page.printlnSTag("br");
	}
    }

    /**
     * Writes the summary of a member symbol to the current page.
     *
     * @param sym
     */
    protected void addMemberSummary(Symbol sym) {
	page.printlnOTag("tr").indent();

	// modifiers
        String mods = filterModifiers(sym);
	page.printlnOTag("td", ATTRS_MODIFIERS).indent();
	if (mods.length() > 0)
	    page.printlnTag("code", mods);
	else
	    page.printlnNbsp(1);
	page.undent();
	page.printlnCTag("td");

	// signature
	page.printlnOTag("td", ATTRS_SIGNATURE).indent();
	page.printOTag("code");
	symtab.defString(sym, true /*addLink*/);
	page.printlnCTag("code");

	// short description
	String firstSentence = firstSentence(getComment(sym));
	if (! firstSentence.equals("")) {
	    page.printlnSTag("br");
	    page.printNbsp(4);
	    page.println(firstSentence);
	}
	page.undent();
	page.printlnCTag("td").undent();
	page.printlnCTag("tr");
    }

    /**
     * Adds a list of all members with all details.
     *
     * @param members
     */
    protected void addMemberDetail(Symbol[] members, String title) {
	boolean first = true;
	for (int i = 0; i < members.length; i++) {
            Symbol sym = members[i];
	    if (!ScalaSearch.isContainer(sym)) {
		if (first) {
		    page.printlnOTag("table", ATTRS_MEMBER_DETAIL).indent();
                    page.printlnOTag("tr").indent();
                    page.printlnTag("td", ATTRS_MEMBER_TITLE, title).undent();
                    page.printlnCTag("tr").undent();
                    page.printlnCTag("table");
		    first = false;
		} else
		    page.printlnHLine();
		addMemberDetail(sym);
	    }
	}
    }

    /**
     * Writes the detail of a member symbol to the page, but create
     * instead a separate page for a class or an object.
     *
     * @param sym
     */
    protected void addMemberDetail(Symbol sym) {
	// title with label
        //	page.printlnAname(Page.asSeenFrom(Location.getURI(sym), uri).getFragment(), "");
	page.printlnAname(Location.getURI(sym).getFragment(), "");
	page.printTag("h3", sym.nameString());

	// signature
	page.printlnOTag("pre");
        String mods = filterModifiers(sym);
        //	String mods = Modifiers.Helper.toString(sym.flags);
	if (mods.length() > 0) page.print(mods + " ");
	symtab.printSignature(sym, false /*addLink*/);
	page.printlnCTag("pre");

	// comment
	addComments(getComment(sym));
    }

    /**
     * Add for each "strict" base type of this class or object symbol
     * the members that are inherited by this class or object.
     *
     * @param sym
     */
    protected void addInheritedMembers(Symbol sym, String inheritedMembers) {
        Symbol[] syms = ScalaSearch.collectMembers(sym);
	Pair grouped = ScalaSearch.groupSymbols(syms);
	Symbol[] owners = (Symbol[]) grouped.fst;
	Map/*<Symbol, Symbol[]>*/ group = (Map) grouped.snd;
	for (int i = 0; i < owners.length; i++) {
	    if (owners[i] != (sym.isModule() ? sym.moduleClass() : sym)) {
                page.printlnOTag("table", ATTRS_MEMBER).indent();

		// owner
                page.printlnOTag("tr").indent();
                page.printlnOTag("td", new XMLAttribute[]{
		    new XMLAttribute("class", "inherited-owner")}).indent();
                page.print(inheritedMembers + " inherited from ");
		printPath(owners[i], SELF_FRAME);
                page.undent();
                page.printlnCTag("td").undent();
                page.printlnCTag("tr");

		// members
                page.printlnOTag("tr").indent();
                page.printlnOTag("td", new XMLAttribute[]{
                   new XMLAttribute("class", "inherited-members")}).indent();
		Symbol[] members = (Symbol[]) group.get(owners[i]);
		for (int j = 0; j < members.length; j++) {
		    if (j > 0) page.print(", ");
		    symtab.printSymbol(members[j], true);
		}
                page.undent();
                page.printlnCTag("td").undent();
                page.printlnCTag("tr").undent();
                page.printlnCTag("table");
                page.printlnSTag("br");
	    }
	}
    }

    /**
     * Prints the signature of a class symbol.
     *
     * @param symbol
     * @param addLink
     */
    public void printTemplateHtmlSignature(Symbol symbol, boolean addLink) {
	// modifiers
        String mods = filterModifiers(symbol);
        //        String mods = Modifiers.Helper.toString(symbol.flags);
	page.printlnOTag("dl");
	page.printlnOTag("dt");
	symtab.print(mods).space();

        // kind
	String keyword = symtab.getSymbolKeywordForDoc(symbol);
        if (keyword != null) symtab.print(keyword).space();
        String inner = symtab.getSymbolInnerString(symbol);

        // name
	symtab.printDefinedSymbolName(symbol, addLink);
	if (symbol.isClass()) {
	    // type parameters
	    Symbol[] tparams = symbol.typeParams();
	    if (tparams.length != 0 || global.debug) {
		symtab.print('[');
		for (int i = 0; i < tparams.length; i++) {
		    if (i > 0) symtab.print(",");
		    symtab.printSignature(tparams[i], false);
		}
		symtab.print(']');
	    }
	    // value parameters
	    Symbol[] vparams = symbol.valueParams();
	    symtab.print('(');
	    for (int i = 0; i < vparams.length; i++) {
		if (i > 0) symtab.print(", ");
		if (vparams[i].isDefParameter()) symtab.print("def ");
		symtab.defString(vparams[i], false);
	    }
	    symtab.print(')');
	}

        // parents
        Type[] parts = (symbol.isModule() ? symbol.moduleClass() : symbol).parents();
        page.printlnCTag("dt");
        for (int i = 0; i < parts.length; i++) {
            page.printOTag("dd");
            symtab.print((i == 0) ? "extends " : "with ");
            symtab.printType(parts[i]);
	    page.printlnCTag("dd");
	}
	page.printCTag("dl");
    }

    /**
     * Creates the page describing the different frames.
     *
     * @param title The page title
     */
    class FramePromise extends Promise {

        public String name() { return FRAME_PAGE; }

        protected void computeIn(Writer writer) {
            createPrinters(writer, Location.makeURI(FRAME_PAGE), windowtitle, "");
            page.printHeader(ATTRS_META, getGenerator());

            page.printlnOTag("frameset", new XMLAttribute[] {
                new XMLAttribute("cols", "25%, 75%")}).indent();
            page.printlnOTag("frameset", new XMLAttribute[] {
                new XMLAttribute("rows", "50%, 50%")}).indent();

            page.printlnOTag("frame", new XMLAttribute[] {
                new XMLAttribute("src", adaptURI(Location.mkURI(PACKAGE_INDEX_PAGE), page.uri).toString()),
                new XMLAttribute("name", PACKAGES_FRAME)});
            page.printlnOTag("frame", new XMLAttribute[] {
                new XMLAttribute("src", adaptURI(Location.mkURI(PACKAGE_PAGE), page.uri).toString()),
                new XMLAttribute("name", CLASSES_FRAME)}).undent();
            page.printlnCTag("frameset");
            page.printlnOTag("frame", new XMLAttribute[] {
                new XMLAttribute("src", adaptURI(Location.mkURI(ROOT_PAGE), page.uri).toString()),
                new XMLAttribute("name", ROOT_FRAME)});

            page.printlnOTag("noframes").indent();
            page.printlnSTag("p");
            page.print("Here is the ");
            page.printAhref(adaptURI(Location.mkURI(ROOT_PAGE), page.uri).toString(),
                            "non-frame based version");
            page.println(" of the documentation.").undent();
            page.printlnCTag("noframes").undent();

            page.printlnCTag("frameset");
            page.printlnCTag("html");

            closePrinters();
        }
    }

    /**
     * Get the URL (as a string) of a resource located in the
     * directory "resources" relative to the classfile of this class.
     */
    protected String getResourceURL(String name) {
        String rsc = HTMLGenerator.class
            .getResource("resources/" + name)
            .toString();
        return rsc;
    }

    /**
     * Generates a resource file.
     *
     * @param name The name of the resource file
     */
    class ResourcePromise extends Promise {

        protected String name;

        protected String dir;

        ResourcePromise(String name, String dir) {
            this.name = name;
            this.dir = dir;
        }

        public String name() { return name; }

        protected void computeIn(Writer writer) {
            String rsrcName = "resources/" + name;
            Reader reader = new InputStreamReader(HTMLGenerator.class.getResourceAsStream(rsrcName));
            if (reader == null)
                throw Debug.abort("Resource file \"" + rsrcName + "\" not found");
            try {
                char[] buf = new char[1024];
                int len;
                while (true) {
                    len = reader.read(buf, 0, buf.length);
                    if (len <= 0) break;
                    writer.write(buf, 0, len);
                }

                reader.close();
                //                writer.close();
            } catch (IOException exception) {
                throw Debug.abort(exception); // !!! reporting an error would be wiser
            }
        }
    }

    private String removeHtmlSuffix(String url) {
	return url.substring(0, url.length() - 5);
    }

    /** Returns the summary page attached to a package symbol. */
    private String packageSummaryPage(Symbol sym) {
	if (sym.isRoot())
	    return PACKAGE_PAGE;
	else {
	    String packagePage = Location.getURI(sym).toString();
	    return removeHtmlSuffix(packagePage) + File.separator + PACKAGE_PAGE;
	}
    }

    /**
     * Writes a table containing a list of packages to the current page.
     *
     * @param syms The package list
     * @param title The title of the package list
     */
    private void printPackagesTable(Symbol[] syms, String title) {
        if (syms.length > 0) {
            page.printlnBold(title);
	    page.printlnOTag("table", ATTRS_LIST).indent();
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", new XMLAttribute[] {
                new XMLAttribute("style", "white-space:nowrap;")}).indent();
	    for (int i = 1; i < syms.length; i++) {
	        Symbol sym = syms[i];
                page.printAhref(
                    adaptURI(Location.mkURI(packageSummaryPage(sym)), page.uri).toString(),
                    CLASSES_FRAME,
		    removeHtmlSuffix(Location.getURI(sym).toString()));
	        page.printlnSTag("br");
	    }
            page.undent();
	    page.printlnCTag("td").undent();
	    page.printlnCTag("tr").undent();
	    page.printlnCTag("table");
            page.printlnSTag("p");
        }
    }

    /**
     * Writes a table containing a list of symbols to the current page.
     *
     * @param syms
     * @param title
     */
    private void addSymbolTable(Symbol[] syms, String title, boolean useFullName) {
        if (syms.length > 0) {
            page.printlnBold(title);
	    page.printlnOTag("table", ATTRS_LIST).indent();
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", new XMLAttribute[] {
                new XMLAttribute("style", "white-space:nowrap;")}).indent();
	    for (int i = 0; i < syms.length; i++) {
	        Symbol sym = syms[i];
                if (! sym.isRoot()) {
                    String name = sym.nameString();
                    if (sym.isPackage() || sym.isPackageClass())
                        page.printAhref(definitionURI(sym).toString(), CLASSES_FRAME, name);
                    else {
                        Symbol user = (useFullName) ? global.definitions.ROOT_CLASS : Symbol.NONE;
                        page.printAhref(definitionURI(sym).toString(), ROOT_FRAME, name);
                    }
	            page.printlnSTag("br");
                }
	    }
            page.undent();
	    page.printlnCTag("td").undent();
	    page.printlnCTag("tr").undent();
	    page.printlnCTag("table");
            page.printlnSTag("p");
        }
    }

    /**
     * Creates a page with the list of packages.
     *
     * @param title
     */
    class PackageIndexPromise extends Promise {

        public String name() { return PACKAGE_INDEX_PAGE; }

        protected void computeIn(Writer writer) {
            createPrinters(writer, Location.makeURI(PACKAGE_INDEX_PAGE), "List of packages", CLASSES_FRAME);
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();

            Symbol[] packages = ScalaSearch.getSortedPackageList(root, isDocumented);

            addDocumentationTitle(new XMLAttribute[]{
                new XMLAttribute("class", "doctitle-larger")});
            page.printAhref(adaptURI(Location.mkURI(PACKAGE_PAGE), page.uri).toString(),
                            CLASSES_FRAME, "All objects, traits and classes");
            page.printlnSTag("p");
            printPackagesTable(packages, "Packages");
            if (validate)
                addValidationBar();

            page.printFootpage();
            closePrinters();
        }
    }

    /**
     * Creates a page with a list of classes or objects.
     *
     * @param sym
     */
    class ContainerIndexPromise extends Promise {

        protected Symbol sym;

        ContainerIndexPromise(Symbol sym) {
            this.sym = sym;
        }

        public String name() { return packageSummaryPage(sym); }

        protected void computeIn(Writer writer) {
            createPrinters(writer, Location.makeURI(packageSummaryPage(sym)), Location.getName(sym), ROOT_FRAME);
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();

            page.printlnOTag("table", ATTRS_NAVIGATION).indent();
            page.printlnOTag("tr").indent();
            page.printlnOTag("td", ATTRS_NAVIGATION_LINKS).indent();
            printPath(sym, ROOT_FRAME);
            page.printlnCTag("td");
            page.printlnCTag("tr");
            page.printlnCTag("table");
            page.printlnSTag("p");

            String[] titles = new String[]{ "Objects", "Traits", "Classes" };
            if (sym.isRoot()) {
                Symbol[][] members = ScalaSearch.getSubContainerMembers(root, isDocumented);
                for (int i = 0; i < titles.length; i++)
                    addSymbolTable(members[i], "All " + titles[i], true);
            } else {
                Symbol[][] members = ScalaSearch.splitMembers(ScalaSearch.members(sym, isDocumented));
                for (int i = 0; i < titles.length; i++) {
                    Arrays.sort(members[i + 2], ScalaSearch.symAlphaOrder);
                    addSymbolTable(members[i + 2], titles[i], false);
                }
            }

            if (validate)
                addValidationBar();

            page.printFootpage();
            closePrinters();
        }
    }

    /**
     * Creates the index page.
     *
     * @param title The page title
     */
    class IndexPromise extends Promise {

        public String name() { return INDEX_PAGE; }

        protected void computeIn(Writer writer) {
            String title = "Scala Library Index";
            createPrinters(writer, Location.makeURI(INDEX_PAGE), title, SELF_FRAME);
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();

            addNavigationBar(INDEX_NAV_CONTEXT);
            page.printlnHLine();

            page.printlnOTag("table", ATTRS_MEMBER).indent();
            page.printlnOTag("tr").indent();
            page.printlnOTag("td", ATTRS_MEMBER_TITLE).indent();
            page.println("Index").undent();
            page.printlnCTag("td").undent();
            page.printlnCTag("tr").undent();
            page.printlnCTag("table");
            page.printlnSTag("br");

            Pair index = ScalaSearch.index(root, isDocumented);
            Character[] chars = (Character[]) index.fst;
            Map map = (Map) index.snd;
            for (int i  = 0; i < chars.length; i++)
                page.printlnAhref("#" + i, SELF_FRAME, HTMLPrinter.encode(chars[i]));
            page.printlnHLine();
            for (int i  = 0; i < chars.length; i++) {
                Character car = chars[i];
                page.printlnAname(String.valueOf(i), "");
                page.printlnOTag("h2");
                page.printBold(HTMLPrinter.encode(car));
                page.printlnCTag("h2");
                page.printlnOTag("dl").indent();
                Symbol[] syms = (Symbol[]) map.get(car);
                for (int j  = 0; j < syms.length; j++) {
                    page.printOTag("dt");
                    addIndexEntry(syms[j]);
                    page.printlnCTag("dt");
                    page.printlnTag("dd", firstSentence(getComment(syms[j])));
                }
                page.undent().printlnCTag("dl");
            }

            page.printlnHLine();
            addNavigationBar(INDEX_NAV_CONTEXT);
            if (validate)
                addValidationBar();

            page.printFootpage();
            closePrinters();
        }
    }

    /**
     * Creates the help page.
     *
     * @param title The page title
     */
    class HelpPromise extends Promise {

        public String name() { return HELP_PAGE; }

        protected void computeIn(Writer writer) {
            String title = "API Help";
            createPrinters(writer, Location.makeURI(HELP_PAGE), title, ROOT_PAGE);
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();

            addNavigationBar(HELP_NAV_CONTEXT);
            page.printlnHLine();

            XMLAttribute[] h3 = new XMLAttribute[]{
                new XMLAttribute("style", "margin:15px 0px 0px 0px; "
                                 + "font-size:large; font-weight: bold;")
            };
            XMLAttribute[] em = new XMLAttribute[]{
                new XMLAttribute("style", "margin:15px 0px 15px 0px; "
                                 + "font-size:small; font-style: italic;")
            };
            page.printlnTag("div", ATTRS_PAGE_TITLE, "How This API Document Is Organized");
            page.println("This API (Application Programming Interface) document "
                         + "has pages corresponding to the items in the navigation bar, "
                         + "described as follows.");

            page.printlnTag("div", h3, "Overview");
            page.printlnOTag("blockquote").indent();
            page.print("The ");
            page.printAhref(adaptURI(Location.mkURI(ROOT_PAGE), page.uri).toString(), SELF_FRAME, "Overview");
            page.println(" page is the front page of this API document and "
                         + "provides a list of all top-level packages, classes, traits "
                         + "and objects with a summary for each. "
                         + "This page can also contain an overall description of the "
                         + "set of packages.").undent();
            page.printlnCTag("blockquote");

            page.printlnTag("div", h3, "Package");
            page.printlnOTag("blockquote").indent();
            page.println("Each package has a page that contains a list of "
                         + "its objects, traits and classes, with a summary for each. "
                         + "This page can contain three categories:");
            page.printlnOTag("ul").indent();
            page.printlnTag("li", "Objects");
            page.printlnTag("li", "Traits");
            page.printlnTag("li", "Classes").undent();
            page.printlnCTag("ul").undent();
            page.printlnCTag("blockquote");

            page.printlnTag("div", h3, "Object/Trait/Class");
            page.printlnOTag("blockquote").indent();
            page.println("Each object, trait, class, nested object, nested "
                         + "trait and nested class has its own separate page. Each "
                         + "of these pages has three sections consisting of a object"
                         + "/trait/class description, summary tables, and detailed "
                         + "member descriptions:");
            page.printlnOTag("ul").indent();
            page.printlnTag("li", "Class inheritance diagram");
            page.printlnTag("li", "Direct Subclasses");
            page.printlnTag("li", "All Known Subinterfaces");
            page.printlnTag("li", "All Known Implementing Classes");
            page.printlnTag("li", "Class/interface declaration");
            page.printlnTag("li", "Class/interface description<p/>");
            page.printlnTag("li", "Nested Class Summary");
            page.printlnTag("li", "Field Summary");
            page.printlnTag("li", "Constructor Summary");
            page.printlnTag("li", "Method Summary<p/>");
            page.printlnTag("li", "Field Detail");
            page.printlnTag("li", "Constructor Detail");
            page.printlnTag("li", "Method Detail").undent();
            page.printlnCTag("ul").undent();
            page.println("Each summary entry contains the first sentence from "
                         + "the detailed description for that item. The summary entries "
                         + "are alphabetical, while the detailed descriptions are in "
                         + "the order they appear in the source code. This preserves "
                         + "the logical groupings established by the programmer.");
            page.printlnCTag("blockquote");


            page.printlnTag("div", h3, "Index");
            page.printlnOTag("blockquote").indent();
            page.print("The ");
            page.printAhref(adaptURI(Location.mkURI(INDEX_PAGE), page.uri).toString(), SELF_FRAME, "Index");
            page.print(" contains an alphabetic list of all classes, interfaces, "
                       + "constructors, methods, and fields.");
            page.printlnCTag("blockquote");

            if (launchServer) {
                page.printlnTag("div", h3, "Searching a definition");
                page.printlnOTag("blockquote").indent();
                page.printlnSTag("a",
                                 new XMLAttribute[] {
                                     new XMLAttribute("name", SEARCH_SECTION) });
                page.printlnOTag("p");
                page.println("At the top and and at the bottom of each page, there is a form that "
                             + "allows to search the definition of a <em>symbol</em> (field, method, "
                             + "package, object, type, trait or class).");
                page.printlnCTag("p");

                page.printlnOTag("p");
                page.println("There are three ways of specifying the symbols of interest:");
                page.printlnOTag("dl");
                page.printOTag("dt");
                page.printlnTag("b", "By name");
                page.printOTag("dd");
                page.println("The search string must be a ");
                page.printlnTag("a",
                                new XMLAttribute[] {
                                    new XMLAttribute("href",
                                                     "http://java.sun.com/j2se/1.4.2/docs/api/java/util/regex/Pattern.html") },
                                "regular expression");
                page.print(" that has to match a substring in the name of the searched symbol.");
                page.printOTag("dt");
                page.printlnTag("b", "By comment");
                page.printOTag("dd");
                page.println("The search string must be a ");
                page.printlnTag("a",
                                new XMLAttribute[] {
                                    new XMLAttribute("href",
                                                     "http://java.sun.com/j2se/1.4.2/docs/api/java/util/regex/Pattern.html") },
                                "regular expression");
                page.print(" that has to match a substring in the comments associated to the searched symbol.");
                page.printOTag("dt");
                page.printlnTag("b", "By type");
                page.printOTag("dd");
                page.println("The search string must represent a Scala type. Any string ");
                page.printlnTag("code", "S");
                page.println(" such that ");
                page.printlnTag("pre", "def foo S;");
                page.println(" is a valid function definition is accepted. Here are some examples:");
                page.printlnOTag("ul");
                page.printOTag("li");
                page.printlnTag("code", ": int => int");
                page.printOTag("li");
                page.printlnTag("code", "[a,b]: List[a] => (a => b) => List[b]");
                page.printOTag("li");
                page.printlnTag("code", "(x: int, y: int): unit");
                page.printlnCTag("ul");
                page.println("The searched symbols must conform to the entered type modulo ");
                page.printlnTag("a",
                                new XMLAttribute[] {
                                    new XMLAttribute("href",
                                                     "http://www.pps.jussieu.fr/~dicosmo/Publications/ISObook.html") },
                                "type isomorphism");
                page.println(". This concept allows to unify types that differ by their exact "
                             + "representation but not by their meaning.  The order of parameters is "
                             + "for instance irrelevant when looking for a function. Note finally that "
                             + "methods of classes are interpreted as functions that would take an "
                             + "extra argument of the type of the class.");
                page.printlnCTag("dl");
                page.printlnCTag("p");
                page.printlnCTag("blockquote");
            }

            page.printlnOTag("div", em);
            page.println("This help file applies to API documentation generated "
                         + "using the standard doclet.");
            page.printlnCTag("div");

            page.printlnHLine();
            addNavigationBar(HELP_NAV_CONTEXT);
            if (validate)
                addValidationBar();

            page.printFootpage();
            closePrinters();
        }
    }

    /**
     * Adds to the current page an hyperlinked path leading to a given
     * symbol (including itself).
     */
    protected void printPath(Symbol sym, String destinationFrame, Page page) {
	String name = removeHtmlSuffix(Location.getURI(sym).toString());
	if (isDocumented.apply(sym)) {
	    String target = definitionURI(sym, page).toString();
	    page.printlnAhref(target, destinationFrame, name);
	}
	else
	    page.println(name);
    }

    protected void printPath(Symbol sym, String destinationFrame) {
        printPath(sym, destinationFrame, page);
    }

    /**
     * Writes the string representation of a symbol entry in the index.
     *
     * @param symbol
     */
    protected void addIndexEntry(Symbol symbol, Page page, MySymbolTablePrinter symtab) {
	// kind
	String keyword = symtab.getSymbolKeywordForDoc(symbol);

        if (keyword != null) page.print(keyword).space();
	// name
	symtab.printDefinedSymbolName(symbol, true);
	// owner
	if (!symbol.isRoot()) {
	    page.print(" in ");
	    printPath(symbol.owner(), SELF_FRAME, page);
	}
    }

    protected void addIndexEntry(Symbol symbol) {
        addIndexEntry(symbol, page, symtab);
    }

    /**
     * Writes documentation comments to the current page.
     *
     * @param comment
     */
    protected void addComments(Comment comment) {
	if (!comment.isEmpty()) {
	    page.printlnOTag("dl").indent();
	    // text with inlined links
	    page.printlnTag("dd", inlineLinkTags(comment.holder, comment.text));
	    page.undent().printlnCTag("dl");

	    // tags
	    addTags(comment.tags);
	}
    }

    /** Inline all the {@link ...} tags inside the text.
     */
    protected String inlineLinkTags(Symbol holder, String text) {
	StringBuffer buff = new StringBuffer();
	Tag[] tags = Comment.makeTags(holder, text);
	for (int i = 0; i < tags.length; i++) {
	    if (tags[i].isText())
		buff.append(tags[i].text);
	    else if (tags[i].isReference())
		buff.append(inlineRefTag(tags[i]));
	}
	return buff.toString();
    }

    /**
     * Returns the first sentence of a documentation comment where all
     * links {@link ...} have been inlined.
     *
     * @param comment
     */
    protected String firstSentence(Comment comment) {
	return
	    inlineLinkTags(comment.holder, comment.firstSentence());
    }

    // TODO: remove this !
    private static String ahref(String dest, String target, String text) {
	return "<a href=\"" + dest + "\" target=\"" + target + "\">" +
	    text + "/a>";
    }

    /** Inline a @see documentation tag.
     */
    protected String inlineRefTag(Tag tag) {
	switch(Tag.parseReference(tag)) {
	case Bad(String ref):
	    return ref;
	case Url(String ref):
	    return ref;
	case Literal(String ref):
	    return ref;
	case Scala(String container, String member, String label):
	    Symbol sym = findSymbolFromString(tag.holder, container, member);
	    if (sym == Symbol.NONE) {
		System.err.println("Warning: not found " + tag);
		return tag.text;
	    }
	    else if (!isDocumented.apply(sym)) {
		System.err.println("Warning: not referenced " + tag);
		return tag.text;
	    }
	    else {
		String labl = label.equals("") ? sym.nameString() : label;
		return ahref(definitionURI(sym).toString(), ROOT_FRAME, labl);
	    }
	default:
	    throw Debug.abort("illegal case", tag);
	}
    }

    /**
     * Writes a set of Scaladoc tags to a page.
     *
     * @param tags
     */
    protected void addTags(Tag[] tags) {
	if (tags.length > 0) {
	    Tag returnTag = null;
	    Tag sinceTag = null;
	    Tag versionTag = null;
	    final List paramTagList = new LinkedList();;
	    final List seeTagList = new LinkedList();
	    final List throwsTagList = new LinkedList();
	    final List authorTagList = new LinkedList();
	    final List otherTagList = new LinkedList();

	    // partitioning the tags
	    for (int i = 0; i < tags.length; i++) {
		if ("@return".equals(tags[i].name))
		    returnTag = tags[i];
		else if ("@since".equals(tags[i].name))
		    sinceTag = tags[i];
		else if ("@version".equals(tags[i].name))
		    versionTag = tags[i];
		else if ("@param".equals(tags[i].name))
		    paramTagList.add(tags[i]);
		else if ("@see".equals(tags[i].name))
		    seeTagList.add(tags[i]);
		else if (tags[i].isException())
		    throwsTagList.add(tags[i]);
		else if ("@author".equals(tags[i].name))
		    authorTagList.add(tags[i]);
		else
		    otherTagList.add(tags[i]);
	    }

	    page.printlnOTag("dl");

	    // Author.
	    if (authorTagList.size() > 0) {
		addTagSection("Author");
		page.printlnOTag("dd").indent();
		Iterator it = authorTagList.iterator();
		Tag authorTag = (Tag) it.next();
		page.print(authorTag.text);
		while (it.hasNext()) {
		    authorTag = (Tag) it.next();
		    page.print(", " + authorTag.text);
		}
                page.println().undent();
		page.printlnCTag("dd");
	    }
	    // Since.
	    if (sinceTag != null) {
		addTagSection("Since");
		page.printlnTag("dd", sinceTag.text);
	    }
	    // Version.
	    if (versionTag != null) {
		addTagSection("Version");
		page.printlnTag("dd", versionTag.text);
	    }
	    // Parameters.
	    if (paramTagList.size() > 0) {
		addTagSection("Parameters");
		Iterator it = paramTagList.iterator();
		Tag paramTag = null;
		while (it.hasNext()) {
		    paramTag = (Tag) it.next();
		    Pair fields = Tag.split(paramTag.text);
		    String paramName = (String) fields.fst;
		    String paramDesc = (String) fields.snd;
		    page.printOTag("dd");
		    page.printTag("code", paramName);
                    page.println(" - ");
		    page.println(inlineLinkTags(paramTag.holder, paramDesc));
		    page.printlnCTag("dd");
		}
	    }
	    // Returns.
	    if (returnTag != null) {
		addTagSection("Returns");
		page.printlnTag("dd", returnTag.text);
	    }
	    // Throws.
	    if (throwsTagList.size() > 0) {
		addTagSection("Throws");
		Iterator it = throwsTagList.iterator();
		Tag throwsTag = null;
		while (it.hasNext()) {
		    throwsTag = (Tag) it.next();
		    Pair fields = Tag.split(throwsTag.text);
		    String exceptionName = (String) fields.fst;
		    String exceptionDesc = (String) fields.snd;
		    page.printOTag("dd");
		    page.printTag("code", exceptionName);
                    page.println(" - "); // TODO: hypertext link
		    page.println(inlineLinkTags(throwsTag.holder, exceptionDesc));
		    page.printlnCTag("dd");
		}
	    }
	    // See Also.
	    if (seeTagList.size() > 0) {
		addTagSection("See Also");
		page.printlnOTag("dd");
		Iterator it = seeTagList.iterator();
		Tag seetag = (Tag) it.next();
		page.println(inlineRefTag(seetag));
		while (it.hasNext()) {
		    seetag = (Tag) it.next();
		    page.print(", ");
		    page.println(inlineRefTag(seetag));
		}
		page.printlnCTag("dd");
	    }
	    // Others.
	    if (otherTagList.size() > 0) {
		Iterator it = otherTagList.iterator();
		while (it.hasNext())
		    addStandardTag((Tag) it.next());
	    }

	    page.printlnCTag("dl");
	}
    }

    /**
     * Returns the HTML representation of a documentation tag.
     *
     * @param tagName
     */
    protected void addTagSection(String tagName) {
	page.printOTag("dt", ATTRS_DOCTAG);
        page.printBold(tagName + ":");
        page.printlnCTag("dt");
    }

    /**
     * Returns the HTML representation of a standard documentation tag.
     *
     * @param tag
     */
    protected void addStandardTag(Tag tag) {
	String sectionName = "";
	if (tag.name.length() > 1) {
	    sectionName += Character.toUpperCase(tag.name.charAt(1));
	    if (tag.name.length() > 2)
		sectionName += tag.name.substring(2);
	}
	addTagSection(sectionName);
	page.printTag("dd", inlineLinkTags(tag.holder, tag.text));
    }

    /**
     * Returns the symbol contained in a specified class or object and
     * described by its label or its name. Return Symbol.NONE if not
     * found.
     *
     * @param context
     * @param classOrObject
     * @param label
     */
    protected Symbol findSymbolFromString(Symbol context, String classOrObject, String label) {
	/*
	String path;
	// absolute path
	if (classOrObject.startsWith(new Character(ScalaSearch.classChar).toString()) ||
	    classOrObject.startsWith(new Character(ScalaSearch.objectChar).toString()))
	    path = classOrObject;
	else // relative path
	    path = ScalaSearch.getOwnersString(context.owner()) + classOrObject;
	path = path.substring(1);
	Symbol sym = ScalaSearch.lookup(global.definitions.ROOT_CLASS, path);
	if (sym == Symbol.NONE)
	    return Symbol.NONE;
	else {
	    if (label == null)
		return sym;
	    else {
		// look for a member in the scope that has a tag
		// @label with the given label
		Scope scope = sym.moduleClass().members();
		SymbolIterator it  = scope.iterator(true);
		while (it.hasNext()) {
		    Symbol member = (Symbol) it.next();
		    Tag[] tags = getComment(member).tags;
		    for (int i = 0; i < tags.length; i++)
			if ("@label".equals(tags[i].name) &&
			    label.equals(tags[i].text))
			    return member;
		}
		// look for the first term with label as name
		return sym.moduleClass().lookup(Name.fromString(label).toTermName());
	    }
	}
	*/
	return Symbol.NONE;
    }

    protected void addCategory(Symbol[] symbols, String title, Page page, MySymbolTablePrinter symtab) {
        if (symbols.length > 0) {
            page.printlnTag("h3", title);
            page.printlnOTag("dl").indent();
            for(int i = 0; i < symbols.length; i++) {
                page.printOTag("dt");
                addIndexEntry(symbols[i], page, symtab);
                page.printlnCTag("dt");
                page.printlnTag("dd", firstSentence(getComment(symbols[i])));
            }
            page.undent().printlnCTag("dl");
        }
    }

    protected void addFoundSymbols(List symbols, Page page, MySymbolTablePrinter symtab) {
        // partition and sort
        List fields = new LinkedList();
        List modules = new LinkedList();
        List types = new LinkedList();
        ScalaSearch.categorizeSymbols(symbols, fields, modules, types);
        Symbol[] sortedFields = ScalaSearch.sortList(fields);
        Symbol[] sortedModules = ScalaSearch.sortList(modules);
        Symbol[] sortedTypes = ScalaSearch.sortList(types);

        addCategory(sortedFields, "val/def", page, symtab);
        addCategory(sortedModules, "package/object", page, symtab);
        addCategory(sortedTypes, "type/trait/class", page, symtab);
    }

    protected void addResultNumber(int number, Page page) {
        page.printOTag("p");
        page.printOTag("b");
        page.print("" + number + " result(s).");
        page.printCTag("b");
        page.printCTag("p");
    }

    public static String SEARCH_SERVLET_NAME = "scaladocServlet";
    public static String PAGE_SERVLET_NAME = "pageServlet";

    public class SearchServlet extends Servlet {

        public String name() {
            return SEARCH_SERVLET_NAME;
        }

        public synchronized void apply(Map req, Writer out) {
            // create page
            String pagename = "search-page";
            URI uri = Location.makeURI(pagename + ".html");
            String destinationFrame = SELF_FRAME;
            String title = pagename;

            final Page page = new Page(out, uri, destinationFrame,
                                       title, representation,
                                       HTMLGenerator.this.adaptURI(Location.mkURI(HTMLPrinter.DEFAULT_STYLESHEET),
                                                                   uri).toString(),
                                       HTMLGenerator.this.adaptURI(Location.mkURI(HTMLPrinter.DEFAULT_JAVASCRIPT),
                                                                   uri).toString());
            page.open();
            page.printHeader(ATTRS_META, getGenerator());
            page.printOpenBody();
            addNavigationBar(CONTAINER_NAV_CONTEXT, page);
            page.printlnHLine();

            // create symbol printer
            final MySymbolTablePrinter symtab =
                SymbolTablePrinterFactory.makeHTML(HTMLGenerator.this, page, isDocumented);

            // analyze the request
            String searchKind = (String) req.get("searchKind");
            String searchString = (String) req.get("searchString");

            // search summary
            String searchKindSummary = null;
            if (searchKind.equals("byName"))
                searchKindSummary = "in defined names";
            else if (searchKind.equals("byComment"))
                searchKindSummary = "in comments";
            else if (searchKind.equals("byType"))
                searchKindSummary = "by type";
            String searchSummary =
                "Scaladoc searched for the string (" + searchString + ") " + searchKindSummary + ".";
            page.printOTag("p");
            page.printOTag("b");
            page.print(searchSummary);
            page.printCTag("b");
            page.printCTag("p");

            // Search by name.
            if (searchKind.equals("byName")) {
                String regexp = searchString;
                final Pattern p = Pattern.compile(regexp);

                final List found = new LinkedList();
                //collect
                ScalaSearch.foreach(global.definitions.ROOT_CLASS,
                                    new ScalaSearch.SymFun() {
                                        public void apply(Symbol sym) {
                                            String name = sym.nameString();
                                            Matcher m = p.matcher(name);
                                            if (m.find())
                                                found.add(sym);
                                        }
                                    },
                                    isDocumented);
                addResultNumber(found.size(), page);
                addFoundSymbols(found, page, symtab);
            }
            // Search by comment.
            else if (searchKind.equals("byComment")) {
                String regexp = searchString;
                final Pattern p = Pattern.compile(regexp);

                final List found = new LinkedList();
                ScalaSearch.foreach(global.definitions.ROOT_CLASS,
                                    new ScalaSearch.SymFun() {
                                        public void apply(Symbol sym) {
                                            Pair c = (Pair) global.mapSymbolComment.get(sym);
                                            if (c != null) {
                                                String comment = (String) c.fst;
                                                Matcher m = p.matcher(comment);
                                                if (m.find())
                                                    found.add(sym);
                                            }
                                        }
                                    },
                                    isDocumented);
                addResultNumber(found.size(), page);
                addFoundSymbols(found, page, symtab);
            }
            // Search by type.
            else if (searchKind.equals("byType")) {
                Type t = ScalaSearch.typeOfString(searchString, global);
                TypeIsomorphism ml = newTypeIso(global);

                List found = new LinkedList();
                Map searchResults = new HashMap();
                Iterator it = ml.searchType(t, isDocumented);
                while (it.hasNext()) {
                    SearchResult result = (SearchResult) it.next();
                    found.add(result.symbol);
                    searchResults.put(result.symbol, result);
                }
                if (t == Type.NoType) {
                    page.printOTag("p");
                    page.println("Scaladoc could not recognize your search string as a type, "
                                 + "see the ");
                    page.printlnTag("a",
                                    new XMLAttribute[] {
                                        new XMLAttribute("href", HELP_PAGE + "#" + SEARCH_SECTION) },
                                    "help page");
                    page.println(" to know which syntax to use.");
                    page.printCTag("p");
                }
                else {
                    page.printOTag("p");
                    page.print("You are searching for symbols with type: ");
                    page.print(t.toString());
                    page.printCTag("p");

                    addResultNumber(found.size(), page);

                    Symbol[] sortedSymbols = ScalaSearch.sortList(found);

                    for(int i = 0; i < sortedSymbols.length; i++) {
                        Symbol sym = sortedSymbols[i];
                        SearchResult result = (SearchResult) searchResults.get(sym);
                        Type adaptedType = result.getType;
                        page.printOTag("dt");
                        //page.printOTag("code");
                        symtab.printShortSignature(sym, true);
                        symtab.printSeqType(sym, adaptedType, symtab.getSymbolInnerString(sym));
                        page.printlnCTag("dt");
                        if (!sym.isRoot()) {
                            page.printOTag("dd");
                            page.print("in ");
                            printPath(sym.owner(), SELF_FRAME, page);
                            if (result.isInClass && (result.tparams.length > 0)) {
                                page.print("[");
                                for(int j = 0; j < result.tparams.length; j++) {
                                    if (j != 0) page.print(",");
                                    page.print(result.tparams[j].nameString());
                                }
                                page.print("]");
                            }
                            page.printCTag("dd");
                        }
                        String firstSent = firstSentence(getComment(sym));
                        if (!firstSent.equals(""))
                            page.printlnTag("dd", firstSent);
                        //page.printlnCTag("code");
                    }
                }
            }
            // close page
            page.printlnHLine();
            addNavigationBar(CONTAINER_NAV_CONTEXT, page);
            page.printFootpage();
            // page.close();
            // already done by the HTTP server when closing connection
        }
    }

    /**
     * A servlet that accepts requests for web pages registered in the
     * hastable <code>promises</code>.
     */
    public class PageServlet extends Servlet {

        public String name() {
            return PAGE_SERVLET_NAME;
        }

        public synchronized void apply(Map req, Writer out) {
            // analyze the request
            String pageName = (String) req.get("page");
            Promise promise = (Promise) promises.get(pageName);
            promise.writeOn(out);
        }
    }
}

public class SearchResult {
    Symbol symbol;
    Type getType;
    boolean isInClass;
    Symbol[] tparams;
    public SearchResult(Symbol symbol,
                        Type getType,
                        boolean isInClass,
                        Symbol[] tparams) {
        this.symbol = symbol;
        this.getType = getType;
        this.isInClass = isInClass;
        this.tparams = tparams;
    }
}

/**
 * This class represents a web page not yet printed.
 */
public abstract class Promise {

    protected StringWriter cache = null;

    /** Name of the web page. */
    public abstract String name();

    /**
     * Print the web page on the given writer. Do not forget to close
     * the writer afterwards. Cache the result for further reuse.
     */
    public void writeOn(Writer writer) {
        if (cache == null) {
            cache = new StringWriter();
            computeIn(cache);
            try {
                cache.close();
            } catch(IOException e) { e.printStackTrace(); }
        }
        try {
            writer.write(cache.toString());
        } catch(IOException e) { e.printStackTrace(); }
    }

    /** Print the web page on the given writer. */
    protected abstract void computeIn(Writer writer);
}
