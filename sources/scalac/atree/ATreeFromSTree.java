/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.Unit;
import scalac.symtab.Definitions;
import scalac.util.Debug;

/** This class translates syntax trees into attributed trees. */
public class ATreeFromSTree {

    //########################################################################
    // Private Fields

    /** The global definitions */
    private final Definitions definitions;

    /** The attributed tree factory */
    private final ATreeFactory make;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ATreeFromSTree(Definitions definitions) {
        this.definitions = definitions;
        this.make = new ATreeFactory();
    }

    //########################################################################
    // Public Methods - Translating units

    /** Translates the unit's body and stores the result in it. */
    public void translate(Unit unit) {
    }

    //########################################################################
    // Private Methods - Translating constants

    /** Translates the constant. */
    private AConstant constant(Object value) {
        if (value instanceof Boolean  ) return make.BOOLEAN((Boolean  )value);
        if (value instanceof Byte     ) return make.BYTE   (((Byte    )value));
        if (value instanceof Short    ) return make.SHORT  ((Short    )value);
        if (value instanceof Character) return make.CHAR   ((Character)value);
        if (value instanceof Integer  ) return make.INT    ((Integer  )value);
        if (value instanceof Long     ) return make.LONG   ((Long     )value);
        if (value instanceof Float    ) return make.FLOAT  ((Float    )value);
        if (value instanceof Double   ) return make.DOUBLE ((Double   )value);
        if (value instanceof String   ) return make.STRING ((String   )value);
        throw Debug.abort("illegal constant", value +" -- "+ value.getClass());
    }

    //########################################################################
}
