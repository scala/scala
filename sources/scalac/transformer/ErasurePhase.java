/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ErasurePhase.java,v 1.13 2002/11/14 15:58:22 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.backend.Primitive;
import scalac.backend.Primitives;
import scalac.checkers.Checker;
import scalac.checkers.CheckSymbols;
import scalac.checkers.CheckTypes;
import scalac.symtab.Definitions;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Debug;

public class ErasurePhase extends Phase {

    //########################################################################
    // Private Fields

    private final Definitions definitions;
    private final Primitives primitives;
    private final Erasure erasure;

    //########################################################################
    // Public Constructors

    public ErasurePhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        this.definitions = global.definitions;
        this.primitives = global.primitives;
        this.erasure = new Erasure(global);
    }

    //########################################################################
    // Public Methods

    public void apply(CompilationUnit[] units) {
        erasure.apply(units);
    }

    public Type transformInfo(Symbol sym, Type tp) {
        if (sym.isConstructor() && sym.constructorClass().isSubClass(definitions.ANYVAL_CLASS)) return tp;
        if (sym.isClass()) {
            if (sym == definitions.ANY_CLASS) return tp;
            if (sym.isJava() && sym.isModuleClass()) return tp;
            if (sym.isSubClass(definitions.ANYVAL_CLASS))
                if (sym != definitions.ANYVAL_CLASS) return tp;
            switch (tp) {
            case CompoundType(Type[] parents, Scope members):
                assert parents.length != 0: Debug.show(sym) + " -- " + tp;
                if (sym.isInterface()) {
                    Symbol superclass = parents[0].symbol();
                    if (superclass.isJava() && !superclass.isInterface()) {
                        parents = Type.cloneArray(parents);
                        parents[0] = definitions.ANYREF_TYPE();
                        tp = Type.compoundType(parents, members, sym);
                    }
                }
                return Type.erasureMap.map(tp);
            default:
                throw Debug.abort("illegal case", tp);
            }
        }
        if (sym.isTerm() && sym.isParameter()) {
            if (primitives.getPrimitive(sym.owner()) == Primitive.BOX)
                return eraseUnboxMethodType(tp);
            if (primitives.getPrimitive(sym.owner()) == Primitive.UNBOX)
                return eraseBoxMethodType(tp);
        }
        if (sym.isType()) return tp;
        if (sym.isThisSym()) return sym.owner().nextType();
        // if (sym == definitions.NULL) return tp.resultType().erasure();
        if (global.target == global.TARGET_INT && sym ==primitives.NEW_OARRAY){
            // !!! hack for interpreter
            Name name = Name.fromString("element").toTypeName();
            Symbol tparam = sym.newTParam(sym.pos, 0, name, definitions.ANY_TYPE());
            return Type.PolyType(new Symbol[]{tparam}, tp);
        }
        switch (primitives.getPrimitive(sym)) {
        case Primitive.IS : return Type.PolyType(tp.typeParams(), Type.MethodType(tp.valueParams(), tp.resultType().erasure()));
        case Primitive.AS : return tp;
        case Primitive.BOX: return eraseBoxMethodType(tp);
        case Primitive.UNBOX: return eraseUnboxMethodType(tp);
        default           : return tp.erasure();
        }
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
        };
    }

    //########################################################################
    // Private Methods

    private Type eraseBoxMethodType(Type type) {
        switch (type) {
        case PolyType(_, Type result):
            return eraseBoxMethodType(result);
        case MethodType(Symbol[] params, Type result):
            return Type.MethodType(params, eraseBoxMethodType(result));
        case TypeRef(Type prefix, Symbol clasz, Type[] args):
            return Type.typeRef(prefix, clasz, Type.EMPTY_ARRAY);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    private Type eraseUnboxMethodType(Type type) {
        switch (type) {
        case PolyType(_, Type result):
            return eraseUnboxMethodType(result);
        case MethodType(Symbol[] params, Type result):
            return Type.MethodType(params, eraseUnboxMethodType(result));
        case TypeRef(_, Symbol clasz, Type[] args):
            if (clasz == definitions.ARRAY_CLASS) {
                Symbol element = args[0].symbol();
                if (element.isAbstractType())
                    if (element.info().symbol() == definitions.ANY_CLASS)
                        return definitions.ANYREF_CLASS.nextType();
            }
            return type.fullErasure();
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    //########################################################################
}
