/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;


import scalac.CompilationUnit;
import scalac.ast.TreeGen;
import scalac.util.*;
import scalac.symtab.*;

/** this class takes care of tedious stuff which has nothing to do with
 *  matching
 */
abstract class PatternTool {

    public static final Name RESULT_N = Name.fromString("$result");

    /** the current compilation unit
     */
    final CompilationUnit unit;

    /** the global fresh name creator
     */
    final FreshNameCreator fresh;

    /** the global definitions component
     */
    final Definitions defs;

    /** the global tree generation component
     */
    final TreeGen gen;

    // constructor
    public PatternTool( CompilationUnit unit ) {
	this.unit = unit;
	this.fresh = unit.fresh;
	this.gen = unit.global.treeGen;
	this.defs = unit.global.definitions;
    }

} // class PatternTool
