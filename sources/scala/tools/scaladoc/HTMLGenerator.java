/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.io.BufferedWriter;
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

import ch.epfl.lamp.util.XMLAttribute;
import ch.epfl.lamp.util.HTMLPrinter;
import ch.epfl.lamp.util.HTMLRepresentation;
import ch.epfl.lamp.util.Pair;
import ch.epfl.lamp.util.Position;
import ch.epfl.lamp.util.XHTMLPrinter;

import scalac.Global;
import scalac.ast.Tree;
import scalac.ast.Tree.AbsTypeDef;
import scalac.ast.Tree.Template;
import scalac.ast.Tree.ValDef;
import scalac.symtab.Kinds;
import scalac.symtab.Modifiers;
import scalac.symtab.NoSymbol;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.Type.*;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Strings;

/**
 * The class <code>HTMLGenerator</code> generates
 * the HTML documentation for a given Scala tree.
 */
public class HTMLGenerator {

    public static final String PRODUCT =
        System.getProperty("scala.product", "scaladoc");
    public static final String VERSION =
        System.getProperty("scala.version", "unknown version");

    /*
     * Names of predefined page names.
     */
    protected final String FRAME_PAGE            = "index.html";
    protected final String ROOT_PAGE             = Location.ROOT_NAME + ".html";
    protected final String PACKAGE_LIST_PAGE     = "package-list-page.html";
    protected final String HELP_PAGE             = "help-page.html";
    protected final String INDEX_PAGE            = "index-page.html";
    protected final String PACKAGE_PAGE          = "package-page.html";

    /*
     * Names of frames.
     */
    protected final String ROOT_FRAME     = "rootFrame";
    protected final String PACKAGES_FRAME = "packagesFrame";
    protected final String CLASSES_FRAME  = "classesFrame";

    /*
     * HTML meta information.
     */
    protected final String GENERATOR = PRODUCT + " (" + VERSION + ")";
    protected final SimpleDateFormat df = new SimpleDateFormat("EEE MMM d HH:mm:ss z yyyy");

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

    protected final XMLAttribute[] ATTRS_META =
        new XMLAttribute[]{ new XMLAttribute("generator", GENERATOR) };

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

    /** The unique documented tree.
     */
    protected final Tree tree;

    /** Global compiler environment.
     */
    protected final Global global;

    /** Maps classes to their direct implementing classes or modules.
     */
    protected final Map subs;

    /** Directory where to put generated HTML pages.
     */
    protected File directory;

    /** Comments associated with symbols.
     */
    protected Map/*<Symbol, Comment>*/ comments = new HashMap();

    /** The underlying HTML printer.
     */
    public HTMLPrinter page;

    /** Symbols defined in the syntactic tree.
     */
    protected TreeSymbols treeSymbols;

    /** The current URI.
     */
    protected URI uri;

    /**
     * The underlying symbol table printer.
     */
    protected SymbolTablePrinter symtab;

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

    /**
     * HTML pages may be generated recursively,
     * so we need to save active printers.
     */
    protected final Stack stack = new Stack();

    private final Symbol JAVALANG; // !!! remove ?
    private final Type ROOT_TYPE; // !!! remove ?

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

    /**
     * Creates a new instance.
     *
     * @param tree
     * @param global
     */
    protected HTMLGenerator(Tree tree, Global global) {
	this.tree = tree;
	this.global = global;
	this.subs = ScalaSearch.subTemplates(tree);
	this.treeSymbols = new TreeSymbols(tree);
	try {  this.uri = new URI("."); } catch(Exception e) {}

        this.JAVALANG = global.definitions.getClass(Names.java_lang);
        this.ROOT_TYPE = global.definitions.ROOT.thisType();
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
    }

    /**
     * Open a new documentation page and make it the current page.
     * @param uri   URL of the page
     * @param title Title of the page
     */
    protected void openPage(URI uri, String title) {
	try {
	    stack.push(page);
	    stack.push(symtab);
	    stack.push(uri);

	    this.uri = uri;
	    File f = new File(directory, uri.toString());
	    f.getParentFile().mkdirs();

	    BufferedWriter out = new BufferedWriter(new FileWriter(f));
	    String stylesheetPath = Location.asSeenFrom(new URI(stylesheet), uri).toString();
	    if (representation.isXHTMLType())
		page = new XHTMLPrinter(out, title, representation, stylesheetPath);
	    else
		page = new HTMLPrinter(out, title, representation, stylesheetPath);
	    symtab = new SymbolTablePrinter(this, page.getCodePrinter());
	} catch(Exception e) { throw Debug.abort(e); }
    }

    /**
     * Close the current page.
     */
    protected void closePage() {
	try {
	    page.getCodePrinter().getWriter().close();
            uri = (URI) stack.pop();
            symtab = (SymbolTablePrinter) stack.pop();
            page = (HTMLPrinter) stack.pop();
	} catch (IOException exception) {
	    throw Debug.abort(exception); // !!! reporting an error would be wiser
	}
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
     * Return the URL of the definition of the given symbol relative
     * to the current page.
     */
    public String definitionURL(Symbol sym) {
	return Location.asSeenFrom(Location.getURI(sym), uri).toString();
    }

    /**
     * Generates the HTML pages.
     */
    protected void apply() {
        if (! checkOutpath())
            return;

        // page with list of packages
	createPackageIndexPage();

	// class and object pages
	createPages(tree);

	if (!noindex) {
            // page with index of Scala documented entities.
	    createIndexPage();
        }

        createHelpPage();

	// page with list of objects and classes.
	createContainerIndexPage(tree);

	// frame description page
	createFramePage();

        // style sheet
        createResource(HTMLPrinter.DEFAULT_STYLESHEET);

        // script
        createResource(HTMLPrinter.DEFAULT_JAVASCRIPT);
    }

    /**
     * Main function.
     */
    public static void apply(Tree tree, Global global) {
	new HTMLGenerator(tree, global).apply();
    }

    /**
     * Returns the comment associated with a given symbol.
     *
     * @param sym
     */
    protected Comment getComment(Symbol sym) {
	Comment comment = (Comment) comments.get(sym);
	if (comment == null) {
	    String s = (String) global.mapSymbolComment.get(sym);
	    comment = new Comment(sym, s);
	    comments.put(sym, comment);
	}
	return comment;
    }

    /**
     * Returns members of a class or object definition tree.
     *
     * @param tree
     */
    protected Tree[][] members(Tree tree) {
	switch (tree) {
	case ClassDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams,
		      Tree tpe, Template impl):
            return members(impl.body);
	case ModuleDef(int mods, Name name, Tree tpe, Template impl):
            return members(impl.body);
	default:
	    throw Debug.abort("illegal tree", tree);
	}
    }

    private Tree[][] members(Tree[] trees) {
	List fields = new LinkedList();
	List methods = new LinkedList();
        List objects = new LinkedList();
        List traits = new LinkedList();
        List classes = new LinkedList();
	List packages = new LinkedList();
        for (int i = 0; i < trees.length; i++) {
	    Symbol sym = trees[i].symbol();
	    if (sym.isTrait()) traits.add(trees[i]);
	    else if (sym.isClass()) classes.add(trees[i]);
	    else if (sym.isPackage()) packages.add(trees[i]);
	    else if (sym.isModule()) objects.add(trees[i]);
	    else if (sym.isMethod()) methods.add(trees[i]);
	    else fields.add(trees[i]);
        }
        return new Tree[][] {
	    (Tree[]) fields.toArray(new Tree[fields.size()]),
	    (Tree[]) methods.toArray(new Tree[fields.size()]),
	    (Tree[]) objects.toArray(new Tree[objects.size()]),
	    (Tree[]) traits.toArray(new Tree[traits.size()]),
	    (Tree[]) classes.toArray(new Tree[classes.size()]),
	    (Tree[]) packages.toArray(new Tree[packages.size()])
	};
    }

    protected String getGenerator() {
    	return "Generated by " + GENERATOR + " on " + df.format(new Date());
    }

    /**
     * Generates a HTML page for a class or object definition as well
     * as pages for every inner class or object.
     *
     * @param tree
     */
    protected void createPages(Tree tree) {
        Symbol sym = tree.symbol();
	String title = Location.getName(sym);
        openPage(Location.getURI(sym), title);

        page.printHeader(ATTRS_META, getGenerator());
        String windowTitle = title + " (" + doctitle.replaceAll("<.*>", " ") + ")";
        page.printOpenBody(new XMLAttribute[]{
            new XMLAttribute("onload", "setWindowTitle('" + windowTitle + "');")
        });
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
        Tree[][] members = members(tree);
        for (int i = 0; i < titles.length; i++) {
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

        closePage();
    }

    /**
     * Returns the string representation of the kind of a symbol.
     *
     * @param sym
     */
    protected String kind(Symbol sym) {
	return symtab.getSymbolKeyword(sym);
    }

    /**
     * Writes the product name and version to the current page.
     *
     * @param attrs
     */
    protected void addDocumentationTitle(XMLAttribute[] attrs) {
        page.printlnOTag("div", attrs).indent();
        page.println(doctitle).undent();
        page.printlnCTag("div");
    }

    /**
     * Writes the navigation bar to the current page.
     *
     * @param sym
     */
    protected void addNavigationBar(int navigationContext) {
	try {
	    String overviewLink = Location.asSeenFrom(new URI(ROOT_PAGE), uri).toString();
	    String indexLink = Location.asSeenFrom(new URI(INDEX_PAGE), uri).toString();
	    String helpLink = Location.asSeenFrom(new URI(HELP_PAGE), uri).toString();

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
		page.printAhref(overviewLink, ROOT_FRAME, "Overview");
		page.printlnCTag("td");
	    }
	    // index link
	    if (navigationContext == INDEX_NAV_CONTEXT)
		page.printlnTag("td", ATTRS_NAVIGATION_SELECTED, "Index");
	    else {
		page.printOTag("td", ATTRS_NAVIGATION_ENABLED);
		page.printAhref(indexLink, ROOT_FRAME, "Index");
		page.printlnCTag("td");
	    }
	    // help link
	    if (navigationContext == HELP_NAV_CONTEXT)
		page.printlnTag("td", ATTRS_NAVIGATION_SELECTED, "Help");
	    else {
		page.printOTag("td", ATTRS_NAVIGATION_ENABLED);
		page.printAhref(helpLink, ROOT_FRAME, "Help");
		page.printlnCTag("td");
	    }

	    page.undent();
	    page.printlnCTag("tr").undent();
	    page.printlnCTag("table").undent();
	    page.printlnCTag("td");

	    // product & version
	    page.printlnOTag("td", ATTRS_NAVIGATION_PRODUCT).indent();
	    addDocumentationTitle(new XMLAttribute[]{
		new XMLAttribute("class", "doctitle")});
	    page.undent();
	    page.printlnCTag("td").undent();

	    page.printlnCTag("tr");

	    page.printlnOTag("tr").indent();
	    page.printlnTag("td", "&nbsp;").undent();
	    page.printlnCTag("tr").undent();
	    page.printlnCTag("table");
	} catch(Exception e) { throw Debug.abort(e); }
    }

    /**
     * Writes the validation bar to the current page.
     */
    protected void addValidationBar() {
        page.printlnOTag("div", ATTRS_VALIDATION);
        page.indent();
        page.printlnAhref(
			  "http://validator.w3.org/check/referer", ROOT_FRAME,
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
	    printPath(sym.owner());

            // kind and name
	    page.printlnOTag("div", ATTRS_ENTITY).indent();
	    if (sym.isPackage())
		page.print("package ");
	    else
		page.print(kind(sym) + " ");
	    page.printlnTag("span", ATTRS_ENTITY, sym.nameString()).undent();
	    page.printlnCTag("div");
	    page.printlnHLine();

	    // complete signature
	    // !!! page.println(printer().printTemplateHtmlSignature(sym, false).toString());
	    symtab.printTemplateHtmlSignature(sym, false);

	    // implementing classes or modules
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

			defString(sub, true /*addLink*/);
			if (sub.owner() != sym.owner()) {
                            page.print(" in ");
			    printPath(sub.owner());
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
    protected void addMemberSummary(Tree[] members, String title) {
	if (members.length > 0) {
	    Tree[] sortedMembers = new Tree[members.length];
	    for (int i = 0; i < members.length; i++)
		sortedMembers[i] = members[i];
	    Arrays.sort(sortedMembers, ScalaSearch.alphaOrder);

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
		addMemberSummary(sortedMembers[i].symbol());

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
	if (treeSymbols.contains(sym)) {
	    page.printlnOTag("tr").indent();

	    // modifiers
	    String mods = Modifiers.Helper.toString(sym.flags);
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
	    defString(sym, true /*addLink*/);
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
    }

    /**
     * Adds a list of all members with all details.
     *
     * @param members
     */
    protected void addMemberDetail(Tree[] members, String title) {
	boolean first = true;
	for (int i = 0; i < members.length; i++) {
            Symbol sym = members[i].symbol();
	    if (sym.isRoot() || sym.isClass() || sym.isModule() || sym.isPackage()) {
		createPages(members[i]);
		if (sym.isPackage())
		    createContainerIndexPage(members[i]);
	    }
	    else {
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
	if (treeSymbols.contains(sym)) {
	    // title with label
	    page.printlnAname(Location.asSeenFrom(Location.getURI(sym), uri).getFragment(), "");
            page.printTag("h3", sym.nameString());

            // signature
	    page.printlnOTag("pre");
	    String mods = Modifiers.Helper.toString(sym.flags);
	    if (mods.length() > 0) page.print(mods + " ");
	    symtab.printSignature(sym, false /*addLink*/);
	    page.printlnCTag("pre");

            // comment
            addComments(getComment(sym));
	}
    }

    /**
     * Add for each "strict" base type of this class or object symbol
     * the members that are inherited by this class or object.
     *
     * @param sym
     */
    protected void addInheritedMembers(Symbol sym, String inheritedMembers) {
	Symbol[] syms = ScalaSearch.findMembers(sym);
	Pair grouped = ScalaSearch.groupSymbols(syms);
	Symbol[] owners = (Symbol[]) grouped.fst;
	Map/*<Symbol, Symbol[]>*/ group = (Map) grouped.snd;
	for (int i = 0; i < owners.length; i++) {
	    if (owners[i] != sym.moduleClass()) {
                page.printlnOTag("table", ATTRS_MEMBER).indent();

		// owner
                page.printlnOTag("tr").indent();
                page.printlnOTag("td", new XMLAttribute[]{
		    new XMLAttribute("class", "inherited-owner")}).indent();
                page.print(inheritedMembers + " inherited from ");
		printPath(owners[i]);
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
		    printSymbol(members[j], true);
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
     * Writes the string representation of the signature of a definition.
     *
     * @param sym
     * @param addLink Generates an hypertext reference if the parameter
     *        <code>addLink</code> is <code>true</code>
     */
    void defString(Symbol sym, boolean addLink) {
	if (sym.isRoot())
	    page.print("Root class");
	else if (sym.isClass() || sym.isModule())
	    symtab.printTemplateSignature(sym, addLink);
	else if (sym.isType() && !sym.isParameter())
	    symtab.printShortSignature(sym, addLink);
	else
	    symtab.printSignature(sym, addLink);
    }

    /**
     * Removes the longest prefix of this type which corresponds to a
     * nested of class and object definitions. The idea is that this
     * prefix is redondant and can be recovered directly from the tree
     * itself.
     *
     * @param prefix
     */
    protected Type cleanPrefix(Type prefix) {
	if (prefix == null) return null;
	if (prefix.symbol().kind == Kinds.NONE) return null;
	if (prefix.symbol().isRoot()) return null;

	// Next line should be removed in theory.
        if (prefix.symbol().moduleClass() == JAVALANG)
            return null;

	switch(prefix) {
	case ThisType(Symbol sym):
	    if (sym.isPackage() && treeSymbols.contains(sym.module()))
		return null;
	    else if (treeSymbols.contains(sym))
		return null;
	    else
		return prefix;
	case TypeRef(Type pre, Symbol sym, Type[] args):
	    Type pre1 = cleanPrefix(pre);
	    if (pre1 == null && args.length == 0 && treeSymbols.contains(sym))
		return null;
	    else {
		pre1 = pre1 == null ? ROOT_TYPE : pre1;
		return Type.typeRef(pre1, sym, args);
	    }
	case SingleType(Type pre, Symbol sym):
	    Type pre1 = cleanPrefix(pre);
	    if (pre1 == null) {
		if (sym.isClass() || sym.isModule())
		    if (treeSymbols.contains(sym)) {
			return null;
		    }
		    else
			return Type.singleType(ROOT_TYPE, sym);
		else
		    return Type.singleType(ROOT_TYPE, sym);
	    }
	    else
		return Type.singleType(pre1, sym);
	default:
	    return prefix;
	}
    }

    protected URI mkURI(String uri) {
	try {
	    return new URI(uri);
	} catch(Exception e) { throw Debug.abort(e); }
    }

    /**
     * Creates the page describing the different frames.
     *
     * @param title The page title
     */
    protected void createFramePage() {
        openPage(mkURI(FRAME_PAGE), windowtitle);

        page.printHeader(ATTRS_META, getGenerator());
	page.printlnOTag("frameset", new XMLAttribute[] {
            new XMLAttribute("cols", "25%, 75%")}).indent();
	page.printlnOTag("frameset", new XMLAttribute[] {
            new XMLAttribute("rows", "50%, 50%")}).indent();

	page.printlnOTag("frame", new XMLAttribute[] {
            new XMLAttribute("src", PACKAGE_LIST_PAGE),
            new XMLAttribute("name", PACKAGES_FRAME)});
	page.printlnOTag("frame", new XMLAttribute[] {
            new XMLAttribute("src", PACKAGE_PAGE),
            new XMLAttribute("name", CLASSES_FRAME)}).undent();
	page.printlnCTag("frameset");
	page.printlnOTag("frame", new XMLAttribute[] {
            new XMLAttribute("src", ROOT_PAGE),
            new XMLAttribute("name", ROOT_FRAME)});

        page.printlnOTag("noframes").indent();
        page.printlnSTag("p");
        page.print("Here is the ");
        page.printAhref(ROOT_PAGE, "non-frame based version");
        page.println(" of the documentation.").undent();
        page.printlnCTag("noframes").undent();

        page.printlnCTag("frameset");
        page.printlnCTag("html");

        closePage();
    }

    /**
     * Generates a resource file.
     *
     * @param name The name of the resource file
     */
    protected void createResource(String name) {
        String rsrcName = "resources/" + name;
        InputStream in = HTMLGenerator.class.getResourceAsStream(rsrcName);
        if (in == null)
	    throw Debug.abort("Resource file \"" + rsrcName + "\" not found");
        try {
            FileOutputStream out = new FileOutputStream(
                directory.getPath() + File.separator + name);

            byte[] buf = new byte[1024];
            int len;
            while (true) {
                len = in.read(buf, 0, buf.length);
                if (len <= 0) break;
               out.write(buf, 0, len);
            }

            in.close();
            out.close();
	} catch (IOException exception) {
	    throw Debug.abort(exception); // !!! reporting an error would be wiser
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
     * @param trees The package list
     * @param title The title of the package list
     */
    private void printPackagesTable(Tree[] trees, String title) {
        if (trees.length > 0) {
            page.printlnBold(title);
	    page.printlnOTag("table", ATTRS_LIST).indent();
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", new XMLAttribute[] {
                new XMLAttribute("style", "white-space:nowrap;")}).indent();
	    for (int i = 1; i < trees.length; i++) {
	        Symbol sym = trees[i].symbol();
                page.printAhref(
                    packageSummaryPage(sym),
                    CLASSES_FRAME,
		    removeHtmlSuffix(Location.getURI(sym).toString()));
		//                    sym.fullNameString());
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
     * Writes a table containing a list of trees to the current page.
     *
     * @param trees
     * @param title
     */
    private void addTreeTable(Tree[] trees, String title, boolean useFullName) {
        if (trees.length > 0) {
            page.printlnBold(title);
	    page.printlnOTag("table", ATTRS_LIST).indent();
	    page.printlnOTag("tr").indent();
	    page.printlnOTag("td", new XMLAttribute[] {
                new XMLAttribute("style", "white-space:nowrap;")}).indent();
	    for (int i = 0; i < trees.length; i++) {
	        Symbol sym = trees[i].symbol();
                if (! sym.isRoot()) {
                    String name = sym.nameString();
                    if (sym.isPackage())
                        page.printAhref(definitionURL(sym), CLASSES_FRAME, name);
                    else {
                        Symbol user = (useFullName) ? global.definitions.ROOT : Symbol.NONE;
                        page.printAhref(definitionURL(sym), ROOT_FRAME, name);
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
     * Creates a page with a list of packages.
     *
     * @param title
     */
    protected void createPackageIndexPage() {
	openPage(mkURI(PACKAGE_LIST_PAGE), "List of packages");

        Tree[] packages = ScalaSearch.getSortedPackageList(tree);

        page.printHeader(ATTRS_META, getGenerator());
	page.printOpenBody();
        addDocumentationTitle(new XMLAttribute[]{
            new XMLAttribute("class", "doctitle-larger")});
        page.printAhref(PACKAGE_PAGE, CLASSES_FRAME, "All objects, traits and classes");
        page.printlnSTag("p");
        printPackagesTable(packages, "Packages");
        if (validate)
            addValidationBar();
	page.printFootpage();

	closePage();
    }

    /**
     * Creates a page with a list of classes or objects.
     *
     * @param tree
     */
    protected void createContainerIndexPage(Tree tree) {
        Symbol sym = tree.symbol();
        openPage(mkURI(packageSummaryPage(sym)), Location.getName(sym));
        page.printHeader(ATTRS_META, getGenerator());
	page.printOpenBody();

	page.printlnOTag("table", ATTRS_NAVIGATION).indent();
	page.printlnOTag("tr").indent();
	page.printlnOTag("td", ATTRS_NAVIGATION_LINKS).indent();

	printPath(sym);
	//            page.printlnAhref(definitionURL(sym), ROOT_FRAME, sym.fullNameString());

	page.printlnCTag("td");
	page.printlnCTag("tr");
	page.printlnCTag("table");
	page.printlnSTag("p");


        String[] titles = new String[]{ "Objects", "Traits", "Classes" };
        if (sym.isRoot()) {
            Tree[][] members = ScalaSearch.getSortedPackageMemberList(tree);
            for (int i = 0; i < titles.length; i++)
                addTreeTable(members[i], "All " + titles[i], true);
        } else {
            Tree[][] members = members(tree);
            for (int i = 0; i < titles.length; i++)
                addTreeTable(members[i + 2], titles[i], false);
        }

        if (validate)
            addValidationBar();
        page.printFootpage();

        closePage();
    }

    /**
     * Creates the index page.
     *
     * @param title The page title
     */
    protected void createIndexPage() {
	String title = "Scala Library Index";
	openPage(mkURI(INDEX_PAGE), title);

        page.printHeader(ATTRS_META, getGenerator());
        String windowTitle = title + " (" + doctitle.replaceAll("<.*>", " ") + ")";
        page.printOpenBody(new XMLAttribute[]{
            new XMLAttribute("onload", "setWindowTitle('" + windowTitle + "');")
        });
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

	Pair index = ScalaSearch.index(tree);
	Character[] chars = (Character[]) index.fst;
	Map map = (Map) index.snd;
	for (int i  = 0; i < chars.length; i++)
	    page.printlnAhref("#" + i, ROOT_FRAME, HTMLPrinter.encode(chars[i]));
	page.printlnHLine();
	for (int i  = 0; i < chars.length; i++) {
	    Character car = chars[i];
	    page.printlnAname(String.valueOf(i), "");
	    page.printlnOTag("h2");
            page.printBold(HTMLPrinter.encode(car));
            page.printlnCTag("h2");
	    page.printlnOTag("dl").indent();
	    Tree[] trees = (Tree[]) map.get(car);
	    for (int j  = 0; j < trees.length; j++) {
		page.printOTag("dt");
                addIndexEntry(trees[j].symbol());
                page.printlnCTag("dt");
		page.printlnTag("dd", firstSentence(getComment(trees[j].symbol())));
	    }
            page.undent().printlnCTag("dl");
	}

        page.printlnHLine();
        addNavigationBar(INDEX_NAV_CONTEXT);
        if (validate)
            addValidationBar();
        page.printFootpage();

        closePage();
    }

    /**
     * Creates the help page.
     *
     * @param title The page title
     */
    protected void createHelpPage() {
	String title = "API Help";
	openPage(mkURI(HELP_PAGE), title);

        page.printHeader(ATTRS_META, getGenerator());
        String windowTitle = title + " (" + doctitle.replaceAll("<.*>", " ") + ")";
        page.printOpenBody(new XMLAttribute[]{
            new XMLAttribute("onload", "setWindowTitle('" + windowTitle + "');")
        });
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
        page.printAhref(ROOT_PAGE, ROOT_FRAME, "Overview");
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
        page.printAhref(INDEX_PAGE, ROOT_FRAME, "Index");
        page.print(" contains an alphabetic list of all classes, interfaces, "
            + "constructors, methods, and fields.");
        page.printlnCTag("blockquote");

        page.printlnOTag("div", em);
        page.println("This help file applies to API documentation generated "
            + "using the standard doclet.");
        page.printlnCTag("div");

        page.printlnHLine();
        addNavigationBar(HELP_NAV_CONTEXT);
        if (validate)
            addValidationBar();
        page.printFootpage();

        closePage();
    }

    /**
     * Adds to the current page an hyperlinked path leading to a given
     * symbol (including itself).
     */
    protected void printPath(Symbol sym) {
	String name = removeHtmlSuffix(Location.getURI(sym).toString());
	if (treeSymbols.contains(sym)) {
	    String target = definitionURL(sym);
	    page.printlnAhref(target, ROOT_FRAME, name);
	}
	else
	    page.println(name);
    }

    /**
     * Adds to the current page an hyperlink leading to a given
     * symbol.
     */
    protected void printSymbol(Symbol sym, boolean addLink) {
	String name = sym.nameString();
	if (global.debug) name = sym.name.toString();
	if (treeSymbols.contains(sym))
	    if (addLink)
		page.printAhref(definitionURL(sym), ROOT_FRAME, name);
	    else {
		page.printOTag("em");
		page.print(name);
		page.printCTag("em");
	    }
	else
	    page.print(name);
    }

    /**
     * Writes the string representation of a symbol entry in the index.
     *
     * @param symbol
     */
    protected void addIndexEntry(Symbol symbol) {
	// kind
	String keyword = symtab.getSymbolKeyword(symbol);
        if (keyword != null) page.print(keyword).space();
	// name
	symtab.printDefinedSymbolName(symbol, true);
	// owner
	if (!symbol.isRoot()) {
	    page.print(" in ");
	    printPath(symbol.owner());
	}
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
		System.err.println("Warning: Scaladoc: not found: " + tag);
		return tag.text;
	    }
	    else if (!treeSymbols.contains(sym)) {
		System.err.println("Warning: Scaladoc: not referenced: " + tag);
		return tag.text;
	    }
	    else {
		String labl = label.equals("") ? sym.nameString() : label;
		return ahref(definitionURL(sym), ROOT_FRAME, labl);
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
}
