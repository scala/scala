/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: AddInterfacesPhase.java,v 1.9 2002/04/19 10:55:15 schinz Exp $
// $Id$

package scalac.transformer;

import scalac.*;
import scalac.checkers.*;
import java.util.*;


public class AddInterfacesPhase extends PhaseDescriptor {
    /** Mapping from class symbols to their interface symbol.
     */
    public final Map/*<Symbol,Symbol>*/ classToInterface = new HashMap();

    /** Mapping from interface symbols to class symbols.
     */
    protected final Map/*<Symbol,Symbol>*/ interfaceToClass = new HashMap();

    /** Mapping from interface member symbols to class member symbols.
     */
    protected final Map/*<Symbol,Symbol>*/ ifaceMemberToClass = new HashMap();

    public String name () {
        return "addinterfaces";
    }

    public String description () {
        return "add one interface per class";
    }

    public String taskDescription() {
        return "added interfaces";
    }

    public void apply(Global global) {
        new AddInterfaces(global, this).apply();
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
