/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** This class implements an attributed code factory. */
public class ACodeFactory {

    //########################################################################
    // Public Fields

    /** The unique Void node */
    public final ACode Void = ACode.Void;

    //########################################################################
    // Public Methods

    /** Creates a This node. */
    public ACode This(Tree t, Symbol clasz) {
        ACode.This code = ACode.This(clasz);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Constant node. */
    public ACode Constant(Tree t, AConstant constant) {
        ACode.Constant code = ACode.Constant(constant);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Load node. */
    public ACode Load(Tree t, ALocation location) {
        ACode.Load code = ACode.Load(location);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Store node. */
    public ACode Store(Tree t, ALocation location, ACode value) {
        ACode.Store code = ACode.Store(location, value);
        code.pos = t.pos;
        return code;
    }

    /** Creates an Apply node. */
    public ACode Apply(Tree t, AFunction function, Type[] targs,ACode[] vargs){
        ACode.Apply code = ACode.Apply(function, targs, vargs);
        code.pos = t.pos;
        return code;
    }

    /** Creates an IsAs node. */
    public ACode IsAs(Tree t, ACode value, Type type, boolean cast) {
        ACode.IsAs code = ACode.IsAs(value, type, cast);
        code.pos = t.pos;
        return code;
    }

    /** Creates an If node. */
    public ACode If(Tree t, ACode test, ACode success, ACode failure) {
        ACode.If code = ACode.If(test, success, failure);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Switch node. */
    public ACode Switch(Tree t, ACode test, int[][] tags, ACode[] bodies,
        ACode other)
    {
        ACode.Switch code = ACode.Switch(test, tags, bodies, other);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Synchronized node. */
    public ACode Synchronized(Tree t, ACode lock, ACode value) {
        ACode.Synchronized code = ACode.Synchronized(lock, value);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Block node. */
    public ACode Block(Tree t, Symbol[] locals,ACode[] statements,ACode value){
        ACode.Block code = ACode.Block(locals, statements, value);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Label node. */
    public ACode Label(Tree t, Symbol label, Symbol[] locals, ACode value) {
        ACode.Label code = ACode.Label(label, locals, value);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Goto node. */
    public ACode Goto(Tree t, Symbol label, ACode[] vargs) {
        ACode.Goto code = ACode.Goto(label, vargs);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Return node. */
    public ACode Return(Tree t, Symbol function, ACode value) {
        ACode.Return code = ACode.Return(function, value);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Throw node. */
    public ACode Throw(Tree t, ACode value) {
        ACode.Throw code = ACode.Throw(value);
        code.pos = t.pos;
        return code;
    }

    /** Creates a Drop node. */
    public ACode Drop(Tree t, ACode value, Type type) {
        ACode.Drop code = ACode.Drop(value, type);
        code.pos = t.pos;
        return code;
    }

    //########################################################################
}
