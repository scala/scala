/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

/** This class contains properties of HTML document representation
 *  (see http://www.w3.org/TR/REC-html40/charset.html).
 */
public class HTMLRepresentation {

    //########################################################################
    // Private Fields

    /** The document type */
    private final String type;

    /** The document character encoding */
    private final String encoding;

    /** The document language */
    private final String language;

    //########################################################################
    // Public Constructors

    /** Creates a new instance */
    public HTMLRepresentation(String type, String encoding, String language) {
        this.type = type;
        this.encoding = encoding;
        this.language = language;
    }

    /** Creates a new instance */
    public HTMLRepresentation(String type, String encoding) {
        this(type, encoding, "EN");
    }

    /** Creates a new instance */
    public HTMLRepresentation(String type) {
        this(type, "iso-8859-1", "EN");
    }

    /** Creates a new instance */
    public HTMLRepresentation() {
        this("HTML 4.01 Transitional", "iso-8859-1", "EN");
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /** Returns the underlying document type. */
    public String getType() {
        return type;
    }

    /** Returns the underlying character encoding. */
    public String getEncoding() {
        return encoding;
    }

    /** Returns the underlying character encoding. */
    public String getLanguage() {
        return language;
    }

}