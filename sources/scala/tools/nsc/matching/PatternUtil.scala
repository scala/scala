/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scala.tools.nsc.matching;

import scala.tools.util.Position;

/** utility functions
 */
abstract class PatternUtil {

  val global: Global;

  import global._;

  var pos: Position;

  var unit: CompilationUnit = _;

  import global.definitions;

  def containsBinding(pat: Tree): Boolean = {
    var generatedVars = false;

    def handleVariableSymbol(sym: Symbol): Unit  =
      if (sym.name.toString().indexOf("$") == -1) {
        generatedVars = true; // .add(sym);
      }

    def isVariableName(name: Name): Boolean =
      ( treeInfo.isVariableName(name) ) && ( name != nme.USCOREkw ) ;

    def isVariableSymbol(sym: Symbol): Boolean =
      ( sym != null )&&( !sym.isPrimaryConstructor );

    def traverse(tree: Tree): Unit = {

      tree match {
        case x @ Ident(name) =>
          if(x.symbol != definitions.PatternWildcard)
            error("shouldn't happen?!");

        case Bind(name, subtree) =>
          var sym: Symbol = _;

        if (isVariableName(name)
            && isVariableSymbol( {sym = tree.symbol; tree.symbol} ))
          handleVariableSymbol(sym);

        traverse( subtree );

        // congruence

        case Apply(fun, args) => args foreach traverse;
        case Sequence(trees)  => trees foreach traverse
        case Star(arg)        => traverse(arg)
        case Typed(expr, tpe) => traverse(expr); // needed??

        case  _ : Select |
               _ : Alternative |
               _ : Select |
               _ : Literal =>  ; // no variables

        case _ =>
          error("unknown pattern node:" + tree + " = " + tree.getClass());
      }
    }
    traverse(pat);
    generatedVars;
  }

} // class PatternUtil
