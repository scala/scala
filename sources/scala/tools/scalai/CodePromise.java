/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: CodePromise.java,v 1.1 2002/06/06 11:42:25 paltherr Exp $
// $Id$

package scala.tools.scalai;

public final class CodePromise {

    //########################################################################
    // Private Fields

    private CodeGenerator generator;
    private CodeContainer container;

    //########################################################################
    // Public Constructors

    public CodePromise(CodeGenerator generator) {
        this.generator = generator;
        this.container = null;
    }

    public CodePromise(CodeContainer container) {
        this.generator = null;
        this.container = container;
    }

    //########################################################################
    // Public Methods

    public CodeContainer force() {
        if (container == null) {
            container = generator.generate();
            generator = null;
        }
        return container;
    }

    //########################################################################
}
