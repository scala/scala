/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import ch.epfl.lamp.util.SourceFile;
import ch.epfl.lamp.util.Position;

import scalac.symtab.NameMangler;
import scalac.ast.Tree;


/** A representation for a compilation unit in scala
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class Unit {

    /** the global compilation environment
     */
    public final Global global;

    /** the associated source code file
     */
    public final SourceFile source;

    /** does this unit come from the interpreter console
     */
    public final boolean console;

    /** the content of the compilation unit in tree form
     */
    public Tree[] body;

    /** the generated symbol data; Symbol -> byte[]
    public SymData symdata;
     */

    /** the name mangler
     */
    public NameMangler mangler = new NameMangler();

    public Unit(Global global, SourceFile source, boolean console) {
        this.global = global;
        this.source = source;
        this.console = console;
    }

    /** issue an error in this compilation unit at a specific location
     */
    public void error(int pos, String message) {
        global.reporter.error(decode(pos), message);
    }

    /** issue a warning in this compilation unit at a specific location
     */
    public void warning(int pos, String message) {
        global.reporter.warning(decode(pos), message);
    }

    /** return a string representation
     */
    public String toString() {
        return source.toString();
    }


    private Position decode(int pos) {
        Position position = new Position(pos);
        if (position.file().id() == 0)
            if (/* !!! source.id() > Position.FILE_MASK && */ position.line() != 0)
                return source.getPosition(position.line(), position.column());
        return position;
    }

}
