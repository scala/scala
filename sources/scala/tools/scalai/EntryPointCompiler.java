/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: EntryPointCompiler.java,v 1.1 2002/08/30 14:28:25 paltherr Exp $
// $Id$

package scalai;

import scalac.Global;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.parser.Sourcefile;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Position;

import scalac.symtab.TermSymbol;

public class EntryPointCompiler {

    //########################################################################
    // Private Fields

    private final String product;
    private final Global global;
    private final Type MAIN_TYPE; // !!! move elsewhere

    //########################################################################
    // Public Constructors

    public EntryPointCompiler(String product, Global global) {
        this.product = product;
        this.global = global;
        Name ARGS_N = Name.fromString("args");
        Symbol args = new TermSymbol(0, ARGS_N, null, 0);
        args.setInfo(global.definitions.arrayType(global.definitions.STRING_TYPE));
        this.MAIN_TYPE = Type.MethodType(new Symbol[] {args}, global.definitions.UNIT_TYPE).erasure();
    }

    //########################################################################
    // Public Methods

    public void compile(String main, String[] args) {
        String names = main.replace('/', '.') + (main.length() > 0 ? "." : "");
        if (names.length() > 0 && names.charAt(0) == '.') {
            error("illegal module name '" + main + "'");
            return;
        }

        Symbol module = global.definitions.ROOT;
        for (int i = 0, j; (j = names.indexOf('.', i)) >= 0; i = j + 1) {
            Name name = Name.fromString(names.substring(i, j));
            module = getModule(module, name);
            if (module == Symbol.NONE) {
                error("could not find module '" + main.substring(0, j) + "'");
                return;
            }
            if (module == Symbol.ERROR) {
                error("term '" + main.substring(0, j) + "' is not a module");
                return;
            }
        }

        Name name = Compiler.MAIN_N;
        Type type = MAIN_TYPE;
        Symbol method = getMethod(module, name, type.erasure());
        if (method == Symbol.NONE) {
            error("module '" + main + "' has no method '" + name + "'");
            return;
        }
        if (method == Symbol.ERROR) {
            error("module '" + main + "' has no method '" + name +
                "' with type '" + type + "'");
            return;
        }

        int pos = Position.NOPOS;
        global.units = new Unit[1];
        global.units[0] = new Unit(global, new Sourcefile((byte[])null,false));
        global.units[0].body = new Tree[1];
        global.units[0].body[0] = global.treeGen.Apply(
            global.treeGen.mkRef(pos, module.thisType(), method),
            new Tree[] { global.make.Literal(pos, args) });
    }

    //########################################################################
    // Private Methods

    private Symbol getModule(Symbol module, Name name) {
        Symbol symbol = module.lookup(name);
        if (symbol == Symbol.NONE || symbol.isModule()) return symbol;
        switch (symbol.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int k = 0; k < alts.length; k++)
                if (alts[k].isModule()) return alts[k];
        }
        return Symbol.ERROR;
    }

    private Symbol getMethod(Symbol module, Name name, Type type) {
        Symbol symbol = module.lookup(name);
        if (symbol == Symbol.NONE || isMethod(symbol, type)) return symbol;
        switch (symbol.type()) {
        case OverloadedType(Symbol[] alts, _):
            for (int k = 0; k < alts.length; k++)
                if (isMethod(alts[k], type)) return alts[k];
        }
        return Symbol.ERROR;
    }

    private boolean isMethod(Symbol symbol, Type type) {
        return symbol.isMethod() && symbol.type().equals(type);
    }

    private void error(String message) {
        global.reporter.error(product + ": " + message);
    }

    //########################################################################
}
