/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
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
        template(unit.repository = new ARepository(), unit.body);
    }

    //########################################################################
    // Private Methods - Translating templates

    /** Translates the templates and adds them to the repository. */
    private void template(ARepository repository, Tree[] trees) {
        for (int i = 0; i < trees.length; i++) template(repository, trees[i]);
    }

    /** Translates the template and adds it to the repository. */
    private void template(ARepository repository, Tree tree) {
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            AClass clasz = new AClass(tree.symbol());
            repository.addClass(clasz);
            member(clasz, body);
            return;

        case PackageDef(_, Template(_, Tree[] body)):
            template(repository, body);
            return;

        case ValDef(_, _, _, Tree rhs):
            // !!!
            return;

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    //########################################################################
    // Private Methods - Translating members

    /** Translates the members and adds them to the class. */
    private void member(AClass clasz, Tree[] trees) {
        for (int i = 0; i < trees.length; i++) member(clasz, trees[i]);
    }

    /** Translates the member and adds it to the class. */
    private void member(AClass clasz, Tree tree) {
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, _):
            template(clasz, tree);
            return;

        case ValDef(_, _, _, Tree rhs):
            AField field = new AField(tree.symbol(), false);
            clasz.addField(field);
            return;

        case DefDef(_, _, _, _, _, Tree rhs):
            AMethod method = new AMethod(tree.symbol(), false);
            clasz.addMethod(method);
            return;

        default:
            throw Debug.abort("illegal case", tree);
        }
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
