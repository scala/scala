/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** This class represents attributed code. */
public class ACode {

    //########################################################################
    // Public Cases

    public case Void;
    public case This(Symbol clasz);
    public case Constant(AConstant constant);
    public case Load(ALocation location);
    public case Store(ALocation location, ACode value);
    public case Apply(AFunction function, Type[] targs, ACode[] vargs);
    public case IsAs(ACode value, Type type, boolean cast);
    public case If(ACode test, ACode success, ACode failure);
    public case Switch(ACode test, int[][] tags, ACode[] bodies, ACode other);
    public case Synchronized(ACode lock, ACode value);
    public case Block(Symbol[] locals, ACode[] statements, ACode value);
    public case Label(Symbol label, Symbol[] locals, ACode value);
    public case Goto(Symbol label, ACode[] vargs);
    public case Return(Symbol function, ACode value);
    public case Throw(ACode value);
    public case Drop(ACode value, Type type);

    //########################################################################
    // Public Fields

    /** The source file position */
    public int pos;

    //########################################################################
    // Public Methods

    /** Returns a string representation of this code. */
    public String toString() {
        return new ATreePrinter().printCode(this).toString();
    }

    //########################################################################
}
