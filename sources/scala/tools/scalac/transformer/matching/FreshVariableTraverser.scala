/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$




import scalac.ast.Traverser;

import scalac.ast.Tree;
import Tree.Ident;
import Tree.Bind;

import scalac.util.Name;
import scalac.util.FreshNameCreator;

import scalac.symtab._;

import java.util.HashMap;
import java.util.Vector;

package scala.tools.scalac.transformer.matching {

/** A tree traverser for handling fresh variables
 * todo: access method instead of
 * @author  Burak Emir
 * @version 1.0
 */
class FreshVariableTraverser(val pos: Int, val owner:Symbol, val fresh: FreshNameCreator )
extends VariableTraverser {

/*
    int    pos;
    Symbol owner;
    FreshNameCreator fresh;
*/
    /**
     */
    private val helpMap = new HashMap();

    /**
     * @param pos
     * @param owner
     * @param fresh
    public FreshVariableTraverser
        this.pos   = pos;
        this.owner = owner;
        this.fresh = fresh;

        helpMap
    }
     */

  def getHelpMap(): HashMap = {
    return helpMap;
  }

  /**
   * @param sym
   */
  def handleVariableSymbol(sym: Symbol ): Unit = {
    //Console.println("FreshVT:"+sym); // DEBUG
    val helpVar =
      owner.newVariable(pos,
                        0,
                        fresh.newName(sym.name.toString()));
    helpVar.setType(sym.getType());

    helpMap.put(sym, helpVar);
  }

}

object FreshVariableTraverser {
  def getVars( t:Tree,  owner:Symbol,  fresh:FreshNameCreator ) = {
    val fvt = new FreshVariableTraverser( t.pos, owner, fresh );
    fvt.traverse( t );
    fvt.getHelpMap();
  }
}
}
