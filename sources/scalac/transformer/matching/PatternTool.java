/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;


import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;

/** this class takes care of tedious stuff which has nothing to do with
 *  matching
 */
abstract class PatternTool {

    public static final Name RESULT_N = Name.fromString("$result");
    public static final Name SCALA_N = Name.fromString("scala");
    public static final Name BOOLEAN_N = Name.fromString("Boolean");
    public static final Name AND_N = Name.fromString("$amp$amp");
    public static final Name OR_N = Name.fromString("$bar$bar");
    public static final Name NOT_N = Name.fromString("$bang");
    public static final Name EQUALS_N = Name.fromString("$eq$eq");
    public static final Name SCALA_MATCHERROR_N = Name.fromString("scala.MatchError");
    public static final Name MATCHERROR_N = Name.fromString("MatchError");
    public static final Name FAIL_N = Name.fromString("fail");
    public static final Name WILDCARD_N = Name.fromString("_");
    public static final Name LENGTH_N = Name.fromString("length");
    public static final Name APPLY_N = Name.fromString("apply");

    /** the current compilation unit
     */
    Unit unit;

    /** the global fresh name creator
     */
    FreshNameCreator fresh;

    /** the global tree factory
     */
    TreeFactory make;

    /** the global definitions component
     */
    Definitions defs;

    /** the global tree generation component
     */
    TreeGen gen;

    /** type inference engine
     */
    Infer infer;

    /** the statics of class Boolean
     */
    Symbol statics; // REMOVE

    /** the eqeq symbol
     */
    Symbol eqSymInt; // REMOVE
    Symbol eqSymBool; // REMOVE

    /** the eqeq symbol
     */
    Symbol notSym;

    // constructor
    public PatternTool( Unit unit, Infer infer ) {
	this.unit = unit;
	this.infer = infer;
	this.fresh = unit.global.freshNameCreator;
	this.make = unit.global.make;
	this.gen = unit.global.treeGen;
	this.defs = unit.global.definitions;

	this.notSym = defs.BOOLEAN_CLASS.lookup/*Term*/( NOT_N );
	assert !(notSym instanceof NoSymbol) : " Boolean.! not found ";

    } // PatternTool( Unit unit, .... )

} // class PatternTool
