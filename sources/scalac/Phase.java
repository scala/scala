/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

/** Representation of a compiler phase. PhaseDescriptors create
 *  phase. Phases operate subsequently on all compilation units.
 *
 *  @author  Matthias Zenger
 *  @version 1.0
 */
public abstract class Phase {

    /** the global environment
     */
    public final Global global;

    /** the descriptor for this phase
     */
    public final PhaseDescriptor descr;

    /** constructor
     */
    public Phase(Global global, PhaseDescriptor descr) {
        this.global = global;
        this.descr = descr;
    }

    /** apply this phase to all compilation units
     */
    public void apply() {
        for (int i = 0; i < global.units.length; i++) {
            apply(global.units[i]);
        }
    }

    /** apply this phase to the given compilation unit
     */
    public abstract void apply(Unit unit);
}
