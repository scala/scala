/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import java.util.Map;
import java.util.LinkedHashMap;

import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

/** This class represents an attributed class. */
public class AClass extends ARepository {

    //########################################################################
    // Private Fields

    /** The class symbol */
    private final Symbol symbol;

    /** The symbol to field map */
    private final Map/*<Symbol,AField>*/ fields;

    /** The symbol to method map */
    private final Map/*Symbol,AMethod*/ methods;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AClass(Symbol symbol) {
        this.symbol = symbol;
        this.fields = new LinkedHashMap();
        this.methods = new LinkedHashMap();
    }

    //########################################################################
    // Public Methods

    /** Returns the symbol of this class. */
    public Symbol symbol() {
        return symbol;
    }

    /** Is this class public? */
    public boolean isPublic() {
        return symbol().isPublic();
    }

    /** Is this class private? */
    public boolean isPrivate() {
        return symbol().isPrivate();
    }

    /** Is this class protected? */
    public boolean isProtected() {
        return symbol().isProtected();
    }

    /** Is this class final? */
    public boolean isFinal() {
        return false; // !!!
    }

    /** Is this class abstract? */
    public boolean isAbstract() {
        return symbol.isAbstractClass();
    }

    /** Is this class an interface? */
    public boolean isInterface() {
        return symbol.isInterface();
    }

    /** Is this class deprecated? */
    public boolean isDeprecated() {
        return false; // !!!
    }

    /** Is this class synthetic? */
    public boolean isSynthetic() {
        return symbol().isSynthetic();
    }

    /** Adds the given field to this class. */
    public void addField(AField field) {
        assert !fields.containsKey(field.symbol()): field;
        fields.put(field.symbol(), field);
    }

    /** Adds the given method to this class. */
    public void addMethod(AMethod method) {
        assert !methods.containsKey(method.symbol()): method;
        methods.put(method.symbol(), method);
    }

    /** Returns the fields of this class. */
    public AField[] fields() {
        return (AField[])fields.values().toArray(new AField[fields.size()]);
    }

    /** Returns the methods of this class. */
    public AMethod[] methods() {
        return(AMethod[])methods.values().toArray(new AMethod[methods.size()]);
    }

    /** Returns the type parameters of this class. */
    public Symbol[] tparams() {
        return symbol.typeParams();
    }

    /** Returns the value parameters of this class. */
    public Symbol[] vparams() {
        return symbol.valueParams();
    }

    /** Returns the parent types of this class. */
    public Type[] parents() {
        switch (symbol.info()) {
        case CompoundType(Type[] parts, _):
            return parts;
        default:
            throw Debug.abort("illegal case", symbol.info());
        }
    }

    /** Returns a string representation of this class. */
    public String toString() {
        return new ATreePrinter().printClass(this).toString();
    }

    //########################################################################
}
