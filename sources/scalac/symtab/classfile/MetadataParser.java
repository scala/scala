/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scalac.Global;
import scalac.Phase;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** Common superclass for all metadata parsers that load symbols
 *  in the context of the first phase of the compiler.
 */
public abstract class MetadataParser extends Type.LazyType {

    /** the global compilation environment
     */
    protected Global global;

    public MetadataParser(Global global) {
        this.global = global;
    }

    /** Complete symbol 'sym' by loading the members of the symbol.
     */
    public final void complete(Symbol sym) {
        Phase phase = global.currentPhase;
        global.currentPhase = global.PHASE.ANALYZER.phase();
	doComplete(sym);
        global.currentPhase = phase;
    }

    /** Perform the actual loading of the symbol.
     */
    protected abstract void doComplete(Symbol sym);

}
