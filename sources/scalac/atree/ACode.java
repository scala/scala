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

    // jvm  : -
    public case Void;

    // jvm  : aload_0
    public case This(Symbol clasz);

    // jvm  : {b, s}ipush, ldc{ ,_w, 2_w}, aconst_null
    // jvm  : iconst_{m1, 2, 3, 4, 5}, {i, l, f, d}const_{0, 1}, fconst_2
    public case Constant(AConstant constant);

    // jvm  : get{static, field}
    // jvm  : {i, l, f, d, a}load{, _0, _1, _2, _3}
    // jvm  : {i, l, f, d, a, b, c, s}aload
    public case Load(ALocation location);

    // jvm  : put{static, field}
    // jvm  : {i, l, f, d, a}store{, _0, _1, _2, _3}
    // jvm  : {i, l, f, d, a, b, c, s}store
    public case Store(ALocation location, ACode value);

    // jvm  : new, invoke{static, virtual, interface, special}, {, a}newarray
    // jvm  : <see also in APrimitive>
    public case Apply(AFunction function, Type[] targs, ACode[] vargs);

    // jvm  : instanceof, checkcast
    public case IsAs(ACode value, Type type, boolean cast);

    // jvm  : -
    public case If(ACode test, ACode success, ACode failure);

    // jvm  : {tables, lookup}switch
    public case Switch(ACode test, int[][] tags, ACode[] bodies, ACode other);

    // jvm  : monitor{enter, exit}
    public case Synchronized(ACode lock, ACode value);

    // jvm  : -
    public case Block(Symbol[] locals, ACode[] statements, ACode value);

    // jvm  : -
    public case Label(Symbol label, Symbol[] locals, ACode value);

    // jvm  : goto, goto_w
    public case Goto(Symbol label, ACode[] vargs);

    // jvm  : {i, l, f, d, a, }return
    public case Return(Symbol function, ACode value);

    // jvm  : athrow
    public case Throw(ACode value);

    // jvm  : pop, pop2
    public case Drop(ACode value, Type type);

    // jvm  : nop, dup{, _x1, _x2, 2, 2_x1, 2_x2}, swap
    // jvm  : multianewarray, iinc, jsr{, _w}, ret, wide
    // NOT MAPPED

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
