/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

/** This class represents a primitive operation. */
public class APrimitive {

    //########################################################################
    // Public Cases

    // type : (type) => type
    // range: type <- { BOOL, Ix, Ux, Rx }
    // jvm  : {i, l, f, d}neg
    public case Negation(ATypeKind kind);

    // type : zero ? (type) => BOOL : (type,type) => BOOL
    // range: type <- { BOOL, Ix, Ux, Rx, REF }
    // jvm  : if{eq, ne, lt, ge, le, gt}, if{null, nonnull}
    //        if_icmp{eq, ne, lt, ge, le, gt}, if_acmp{eq,ne}
    public case Test(ATestOp op, ATypeKind kind, boolean zero);

    // type : (type,type) => I4
    // range: type <- { Ix, Ux, Rx }
    // jvm  : lcmp, {f, d}cmp{l, g}
    public case Comparison(AComparisonOp op, ATypeKind kind);

    // type : (type,type) => type
    // range: type <- { Ix, Ux, Rx }
    // jvm  : {i, l, f, d}{add, sub, mul, div, rem}
    public case Arithmetic(AArithmeticOp op, ATypeKind kind);

    // type : (type,type) => type
    // range: type <- { BOOL, Ix, Ux }
    // jvm  : {i, l}{and, or, xor}
    public case Logical(ALogicalOp op, ATypeKind kind);

    // type : (type,I4) => type
    // range: type <- { Ix, Ux }
    // jvm  : {i, l}{shl, ushl, shr}
    public case Shift(AShiftOp op, ATypeKind kind);

    // type : (src) => dst
    // range: src,dst <- { Ix, Ux, Rx }
    // jvm  : i2{l, f, d}, l2{i, f, d}, f2{i, l, d}, d2{i, l, f}, i2{b, c, s}
    public case Conversion(ATypeKind src, ATypeKind dst);

    // type : (Array[REF]) => I4
    // range: type <- { BOOL, Ix, Ux, Rx, REF }
    // jvm  : arraylength
    public case ArrayLength(ATypeKind kind);

    // type : (lf,rg) => STR
    // range: lf,rg <- { BOOL, Ix, Ux, Rx, REF, STR }
    // jvm  : -
    public case StringConcat(ATypeKind lf, ATypeKind rg);

    //########################################################################
    // Public Methods

    /** Returns a string representation of this primitive. */
    public String toString() {
        return new ATreePrinter().printPrimitive(this).toString();
    }

    //########################################################################
}
