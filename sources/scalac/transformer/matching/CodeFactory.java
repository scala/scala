/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;

import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;

class CodeFactory extends PatternTool {

     public CodeFactory( Unit unit, Infer infer ) {
            super( unit, infer );
     }

    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
        //return sym.typeAt(unit.global.ANALYZER_PHASE.id);
    }

    /** return the analyzed type
     */
    public Type typeOf0(Symbol sym) {
        return sym.typeAt(unit.global.PHASE.ANALYZER.id);
    }


    protected Tree Block(int pos, Tree[] ts, Type tpe) {
        if (ts.length == 1)
            return ts[0];
        else if (ts.length > 1)
            switch (ts[ts.length - 1]) {
                case Block(Tree[] ts0):
                    Tree[] ts1 = new Tree[ts0.length + ts.length - 1];
                    System.arraycopy(ts, 0, ts1, 0, ts.length - 1);
                    System.arraycopy(ts0, 0, ts1, ts.length - 1, ts0.length);
                    return Block(pos, ts1, tpe);
            }
        return make.Block(pos, ts).setType(tpe);
    }

    /* // unused
    protected Tree Negate(Tree tree) {
        switch (tree) {
            case Literal(Object value):
                return gen.mkBooleanLit(tree.pos, !((Boolean)value).booleanValue());
        }
        return make.Apply(
                tree.pos,
                gen.Select(tree, NOT_N),
                Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
    }
    */
    protected Tree And(Tree left, Tree right) {
        switch (left) {
            case Literal(Object value):
                return ((Boolean)value).booleanValue() ? right : left;
        }
        switch (right) {
            case Literal(Object value):
                if (((Boolean)value).booleanValue()) return left;
        }
        Symbol fun = left.type.lookup(AND_N);
        return make.Apply(
                left.pos,
                make.Select(
                    left.pos,
                    left,
                    AND_N).setType(typeOf(fun)).setSymbol(fun),
                new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree Or(Tree left, Tree right) {
        switch (left) {
            case Literal(Object value):
                return ((Boolean)value).booleanValue() ? left : right;
        }
        switch (right) {
            case Literal(Object value):
                if (!((Boolean)value).booleanValue()) return left;
        }
        Symbol fun = left.type.lookup(OR_N);
        return make.Apply(
                left.pos,
                make.Select(
                    left.pos,
                    left,
                    OR_N).setType(typeOf(fun)).setSymbol(fun),
                new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree Is(Tree tree, Type type) {
        return
            make.Apply(
                tree.pos,
                make.TypeApply(
                    tree.pos,
                    make.Select(
                        tree.pos,
                        tree,
                        defs.IS.name).setType(typeOf(defs.IS)).setSymbol(defs.IS),
                    new Tree[]{gen.mkType(tree.pos, type)})
                .setType(Type.MethodType(Symbol.EMPTY_ARRAY, defs.BOOLEAN_TYPE)),
                Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree As(Tree tree, Type type) {
        return
            make.Apply(
                tree.pos,
                make.TypeApply(
                    tree.pos,
                    make.Select(
                        tree.pos,
                        tree,
                        defs.AS.name).setType(typeOf(defs.AS)).setSymbol(defs.AS),
                    new Tree[]{gen.mkType(tree.pos, type)})
                .setType(Type.MethodType(Symbol.EMPTY_ARRAY, type)),
                Tree.EMPTY_ARRAY).setType(type);
    }

    protected Tree Equals(Tree left, Tree right) {
        Symbol fun = left.type.lookup(EQUALS_N);
        switch (typeOf(fun)) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                //System.out.println("**** " + left.type);
                Tree t = make.Select(left.pos, left, EQUALS_N);
                //for (int i = 0; i < alttypes.length; i++)
                //    System.out.println(alts[i] + ": " + alttypes[i]);
                infer.methodAlternative(t, alts, alttypes,
                    new Type[]{right.type}, defs.BOOLEAN_TYPE);
                return make.Apply(left.pos, t, new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
            default:
                //System.out.println("#### " + left.type + ": " + fun);
                return make.Apply(
                    left.pos,
                    make.Select(
                        left.pos,
                        left,
                        EQUALS_N).setType(typeOf(fun)).setSymbol(fun),
                    new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
        }
    }

    protected Tree ThrowMatchError(int pos, Type type) {
        Symbol matchErrorModule = defs.SCALA.members().lookup(MATCHERROR_N);
        outer: switch (typeOf(matchErrorModule)) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                for (int i = 0; i < alts.length; i++)
                    switch (alttypes[i]) {
                        case TypeRef(_, _, _):
                            matchErrorModule = alts[i];
                            break outer;
                    }
        }
        Symbol failMethod = typeOf(matchErrorModule).lookup(FAIL_N);
        return
        make.Apply(
            pos,
            make.TypeApply(
               pos,
               make.Select(
                pos,
                make.Select(
                    pos,
                    make.Ident(pos, Names.scala).setType(typeOf(defs.SCALA)).setSymbol(defs.SCALA),
                    MATCHERROR_N)
                   .setSymbol(matchErrorModule)
                   .setType(typeOf(matchErrorModule)),
                FAIL_N).setType(typeOf(failMethod)).setSymbol(failMethod),
                new Tree[]{gen.mkType(pos, type)})
              .setType(((Type.PolyType) typeOf(failMethod)).result.subst(
                    typeOf(failMethod).typeParams(),
                    new Type[]{type})),
            new Tree[]{
                make.Literal(pos, unit.toString()).setType(defs.STRING_TYPE),
                make.Literal(pos, new Integer(Position.line(pos))).setType(defs.INT_TYPE)
            }).setType(type);
    }

}
