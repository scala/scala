/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab;

import scalac.Global;
import scalac.util.NameTransformer;

/**
 * This class provides methods to turn symbol names into strings.
 *
 * There are several kinds of owner/symbol relationships:
 *  - root/symbol
 *  - package/symbol
 *  - class/class
 *  - class/member
 *  - owner/parameter
 *  - none/symbol
 *  - error/symbol
 *  - other relationships
 *
 * The separator to use for each of these relationships are specified
 * independently. When a separator is set to '\0' the corresponding
 * owners are dropped from the resulting string. ROOT, NONE and ERROR
 * owners are also dropped. The choice of the separator to use is done
 * by the method "getOwnerSymbolSeparator(Symbol)"
 *
 * Symbol names are usually encoded. There is support to decode these
 * names.
 *
 * Each symbol has an unique identifier. There is support to print
 * these unique identifiers.
 */
public class SymbolNameWriter {

    //########################################################################
    // Private Fields

    /** The string buffer */
    private StringBuffer buffer;

    /** The root/symbol separator */
    private char root;

    /** The package/symbol separator */
    private char peckage;

    /** The class/class separator */
    private char clasz;

    /** The class/member separator */
    private char member;

    /** The owner/parameter separator */
    private char parameter;

    /** The none/symbol separator */
    private char none;

    /** The error/symbol separator */
    private char error;

    /** The other relationships separator */
    private char other;

    /** The name decoding status */
    private boolean decode;

    /** The symbol/unique-identifier separator */
    private char unique;

    /** The number of pending characters */
    private int pending;

    /** The prefix for the current operation (null if none) */
    private String prefix;

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance with an empty string buffer. This is
     * equivalent to calling "SymbolStringifier(null)".
     */
    public SymbolNameWriter() {
        this(null);
    }

    /**
     * Initializes this instance with given string buffer. All
     * separators are set to '.' excepted the root/symbol separator
     * which is set to '\0'. The symbol name decoding is disabled. The
     * unique identifier printing is disabled.
     */
    public SymbolNameWriter(StringBuffer buffer) {
        setStringBuffer(buffer).setAllSeparators('.').setRootSeparator('\0');
    }

    //########################################################################
    // Public Method - Configuration operations

    /** Sets the string buffer. */
    public SymbolNameWriter setStringBuffer(StringBuffer buffer) {
        this.buffer = buffer;
        return this;
    }

    /** Sets all separators. */
    public SymbolNameWriter setAllSeparators(char separator) {
        setRootSeparator(separator);
        setPackageSeparator(separator);
        setClassSeparator(separator);
        setMemberSeparator(separator);
        setParameterSeparator(separator);
        setNoneSeparator(separator);
        setErrorSeparator(separator);
        setOtherSeparator(separator);
        return this;
    }

    /** Sets the root/symbol separator. */
    public SymbolNameWriter setRootSeparator(char separator) {
        this.root = separator;
        return this;
    }

    /** Sets the package/symbol separator. */
    public SymbolNameWriter setPackageSeparator(char separator) {
        this.peckage = separator;
        return this;
    }

    /** Sets the class/class separator. */
    public SymbolNameWriter setClassSeparator(char separator) {
        this.clasz = separator;
        return this;
    }

    /** Sets the class/member separator. */
    public SymbolNameWriter setMemberSeparator(char separator) {
        this.member = separator;
        return this;
    }

    /** Sets the owner/parameter separator. */
    public SymbolNameWriter setParameterSeparator(char separator) {
        this.parameter = separator;
        return this;
    }

    /** Sets the none/symbol separator. */
    public SymbolNameWriter setNoneSeparator(char separator) {
        this.none = separator;
        return this;
    }

    /** Sets the error/symbol separator. */
    public SymbolNameWriter setErrorSeparator(char separator) {
        this.error = separator;
        return this;
    }

    /** Sets the other relationships separator. */
    public SymbolNameWriter setOtherSeparator(char separator) {
        this.other = separator;
        return this;
    }

    /** Sets the name decoding status. */
    public SymbolNameWriter setNameDecoding(boolean decode) {
        this.decode = decode;
        return this;
    }

    /** Sets the symbol/unique-identifier separator. */
    public SymbolNameWriter setUniqueSeparator(char separator) {
        this.unique = separator;
        return this;
    }

    //########################################################################
    // Public Method - To string operations

    /** Returns the string formed by the symbol. */
    public String toString(Symbol symbol) {
        return appendSymbol(symbol).toString();
    }

    /** Returns the string formed by the prefix and symbol. */
    public String toString(String prefix, Symbol symbol) {
        return appendSymbol(prefix, symbol).toString();
    }

    /** Returns the string formed by the symbol and suffix. */
    public String toString(Symbol symbol, String suffix) {
        return appendSymbol(symbol, suffix).toString();
    }

    /** Returns the string formed by the prefix, symbol and suffix. */
    public String toString(String prefix, Symbol symbol, String suffix) {
        return appendSymbol(prefix, symbol, suffix).toString();
    }

    //########################################################################
    // Public Method - Append operations

    /** Appends given symbol. */
    public StringBuffer appendSymbol(Symbol symbol) {
        String name = getSymbolName(symbol);
        this.pending += name.length();
        char separator = getSymbolSeparator(symbol);
        return appendPrefix(symbol.owner(), separator).append(name);;
    }

    /** Appends given prefix and symbol. */
    public StringBuffer appendSymbol(String prefix, Symbol symbol) {
        assert this.prefix == null && prefix != null;
        this.prefix = prefix;
        return appendSymbol(symbol);
    }

    /** Appends given symbol and suffix. */
    public StringBuffer appendSymbol(Symbol symbol, String suffix) {
        this.pending += suffix.length();
        return appendSymbol(symbol).append(suffix);
    }

    /** Appends given prefix, symbol and suffix. */
    public StringBuffer appendSymbol(String prefix, Symbol symbol,
        String suffix)
    {
        assert this.prefix == null && prefix != null;
        this.prefix = prefix;
        this.pending += suffix.length();
        return appendSymbol(symbol).append(suffix);
    }

    /** Appends prefix formed by given owner and separator. */
    public StringBuffer appendPrefix(Symbol owner, char separator) {
        if (separator == 0) return getStringBuffer();
        this.pending += 1;
        return appendSymbol(owner).append(separator);
    }

    //########################################################################
    // Public Method - Query operations

    /** Returns the name to use for given symbol. */
    public String getSymbolName(Symbol symbol) {
        String name = symbol.name.toString();
        if (decode) name = NameTransformer.decode(name);
        if (unique != 0) name =name+unique+Global.instance.uniqueID.id(symbol);
        return name;
    }

    /** Returns the owner/symbol separator to use for given symbol. */
    public char getSymbolSeparator(Symbol symbol) {
        if (symbol.isRoot() || symbol.isNone() || symbol.isError()) return 0;
        if (symbol.isParameter()) return parameter;
        Symbol owner = symbol.owner();
        if (owner.isRoot()) return root;
        if (owner.isNone()) return none;
        if (owner.isError()) return error;
        if (owner.isPackageClass()) return peckage;
        if (owner.isClass()) return symbol.isClass() ? clasz : member;
        return other;
    }

    /** Returns the string buffer. */
    public StringBuffer getStringBuffer() {
        if (prefix != null) pending += prefix.length();
        if (buffer == null) {
            this.buffer = new StringBuffer(pending);
        } else {
            buffer.ensureCapacity(buffer.length() + pending);
        }
        if (prefix != null) buffer.append(prefix);
        this.pending = 0;
        this.prefix = null;
        return buffer;
    }

    /** Returns the content of the string buffer. */
    public String toString() {
        return buffer == null ? "" : buffer.toString();
    }

    //########################################################################
}
