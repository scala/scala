/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;


import scalac.Unit;
import scalac.ast.TreeGen;
import scalac.util.*;
import scalac.symtab.*;

/** this class takes care of tedious stuff which has nothing to do with
 *  matching
 */
abstract class PatternTool {

    public static final Name RESULT_N = Name.fromString("$result");
    public static final Name SCALA_N = Name.fromString("scala");
    public static final Name BOOLEAN_N = Name.fromString("Boolean");
    public static final Name AND_N = Name.fromString("$amp$amp");
    public static final Name OR_N = Name.fromString("$bar$bar");
    public static final Name EQUALS_N = Name.fromString("$eq$eq");
    public static final Name SCALA_MATCHERROR_N = Name.fromString("scala.MatchError");
    public static final Name MATCHERROR_N = Name.fromString("MatchError");
    public static final Name FAIL_N = Name.fromString("fail");
    public static final Name LENGTH_N = Name.fromString("length");
    public static final Name APPLY_N = Name.fromString("apply");

    /** the current compilation unit
     */
    final Unit unit;

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
    public PatternTool( Unit unit ) {
	this.unit = unit;
	this.fresh = unit.global.freshNameCreator;
	this.gen = unit.global.treeGen;
	this.defs = unit.global.definitions;
    }

} // class PatternTool
