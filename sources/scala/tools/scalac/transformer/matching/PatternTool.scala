/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */



import scalac.CompilationUnit;
//import scalac.ast.TreeGen;
//import scalac.util.*;
//import scalac.symtab.*;

package scala.tools.scalac.transformer.matching {
/** this class takes care of tedious stuff which has nothing to do with
 *  matching
 */
 abstract class PatternTool(unit: CompilationUnit)  {

   final def fresh = unit.fresh;
   final def gen = unit.global.treeGen;
   final def defs = unit.global.definitions;

 } // class PatternTool
}
