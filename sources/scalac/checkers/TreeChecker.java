package scalac.checkers;

import scalac.Unit;
import scalac.symtab.Definitions;

/**
 * This checker checks that trees are well-formed. It checks both the
 * shape and the attribution of the trees.
 */
public class TreeChecker {

    //########################################################################
    // Private Fields

    /** The global definitions */
    private final Definitions definitions;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public TreeChecker(Definitions definitions) {
        this.definitions = definitions;
    }

    //########################################################################
    // Public Methods - Checking units

    /** Checks the unit. Returns true. */
    public boolean check(Unit unit) {
        return true;
    }

    //########################################################################
}
