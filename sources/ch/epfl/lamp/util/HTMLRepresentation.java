/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

/**
 * This class contains properties of HTML document representation
 * (see http://www.w3.org/TR/REC-html40/charset.html).
 */
public class HTMLRepresentation {

    public static final String DEFAULT_DOCTYPE     = "HTML 4.01 Transitional";
    public static final String DEFAULT_DOCENCODING = "iso-8859-1";
    public static final String DEFAULT_DOCLANGUAGE = "EN";

    //########################################################################
    // Private Fields

    /** The document type */
    private final String doctype;

    /** The document character encoding */
    private final String docencoding;

    /** The document language */
    private final String language;

    //########################################################################
    // Public Constructors

    /** Creates a new instance */
    public HTMLRepresentation(String doctype, String docencoding, String language) {
        this.doctype = doctype;
        this.docencoding = docencoding;
        this.language = language;
    }

    /** Creates a new instance */
    public HTMLRepresentation(String doctype, String docencoding) {
        this(doctype, docencoding, DEFAULT_DOCLANGUAGE);
    }

    /** Creates a new instance */
    public HTMLRepresentation(String doctype) {
        this(doctype, DEFAULT_DOCENCODING, DEFAULT_DOCLANGUAGE);
    }

    /** Creates a new instance */
    public HTMLRepresentation() {
        this(DEFAULT_DOCTYPE, DEFAULT_DOCENCODING, DEFAULT_DOCLANGUAGE);
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /**
     * Returns the underlying document type.
     */
    public String getType() {
        return doctype;
    }

    /**
     * Returns the underlying character encoding.
     */
    public String getEncoding() {
        return docencoding;
    }

    /**
     * Returns the underlying document language.
     *
     * @ return the language name of the underlying document
     */
    public String getLanguage() {
        return language;
    }

    /**
     * Returns <code>true</code> if the document type is HTML.
     */
    public boolean isHTMLType() {
        return doctype.toLowerCase().matches("^html\\p{Space}\\d\\.\\d.*");
    }

    /**
     * Returns <code>true</code> if the document type is XHTML.
     */
    public boolean isXHTMLType() {
        return doctype.toLowerCase().matches("^xhtml\\p{Space}\\d\\.\\d.*");
    }

    //########################################################################
}
