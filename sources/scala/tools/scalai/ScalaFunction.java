/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ScalaFunction.java,v 1.9 2002/07/03 16:08:00 paltherr Exp $
// $Id$

package scalai;

import ch.epfl.lamp.util.SourceFile;

import scalac.ast.Tree;
import scalac.ast.Tree.ValDef;
import scalac.symtab.Symbol;
import scalac.util.Debug;

public class ScalaFunction extends CodeGenerator {

    //########################################################################
    // Private Fields

    private final Compiler compiler;
    private final SourceFile source;
    private final Symbol symbol;
    private final ValDef[] params;
    private final Tree body;

    //########################################################################
    // Public Constructors

    public ScalaFunction(Compiler compiler, SourceFile source, Symbol symbol, ValDef[] params, Tree body) {
        this.compiler = compiler;
        this.source = source;
        this.symbol = symbol;
        this.params = params;
        this.body = body;
    }

    //########################################################################
    // Public Methods - CodeGenerator interface

    public CodeContainer generate() {
        return compiler.compile(source, symbol, body, Tree.symbolOf(params));
    }

    //########################################################################
    // Public Methods - Object interface

    // !!!
    public String toString() {
        return "ScalaFunction(" + Debug.show(symbol) + ")";
    }

    //########################################################################
}
