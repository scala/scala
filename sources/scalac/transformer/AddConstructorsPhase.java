/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

//package scala.compiler.backend;
package scalac.transformer;

import java.util.HashMap;

import scalac.checkers.*;
import scalac.Global;
import scalac.PhaseDescriptor;

public class AddConstructorsPhase extends PhaseDescriptor {

    /** Maps the old constructor symbols to the new ones
     */
    HashMap constructors = new HashMap();

    public String name() {
	return "addconstructors";
    }

    public String description() {
	return "add explicit constructor for each class";
    }

    public String taskDescription() {
	return "added constructors";
    }

    public void apply(Global global) {
	new AddConstructors(global, constructors).apply();
    }

    public Checker[] postCheckers(Global global) {
        return new Checker[] {
            new CheckSymbols(global),
            new CheckTypes(global),
            new CheckOwners(global),
	    new CheckNames(global)
        };
    }
}
