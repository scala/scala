/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */


import scalac._;
import scalac.ast._;
import scalac.util._;
import scalac.symtab._;
import Tree.ValDef;

package scala.tools.scalac.transformer.matching {

/** the environment for a body of a case
 * @param owner the owner of the variables created here
 */
class CaseEnv(val owner:Symbol, unit:CompilationUnit) {

  private var boundVars:scala.Array[ValDef] = new Array[ValDef](4);
  private var numVars = 0;

  /** substitutes a symbol on the right hand side of a ValDef
   */
   def substitute(oldSym: Symbol, newInit: Tree): Unit = {
     var i = 0; while( i < numVars) {
       if( boundVars(i).rhs.symbol() == oldSym ) {
         boundVars(i).rhs = newInit;
         return;
       }
       i = i + 1;
     }
   }

  def newBoundVar(sym:Symbol, tpe: Type, init:Tree ): Unit = {
    sym.setOwner( owner ); // FIXME should be corrected earlier
    if (numVars == boundVars.length) {
      val newVars = new Array[ValDef](numVars * 2);
      System.arraycopy(boundVars, 0, newVars, 0, numVars);
      this.boundVars = newVars;
    }
    sym.setType(tpe);
    this.boundVars(numVars) = unit.global.treeGen.ValDef(sym, init.duplicate());
    numVars = numVars + 1;
  }

  def getBoundVars(): Array[ValDef] = {
    val newVars = new Array[ValDef](numVars);
    System.arraycopy(boundVars, 0, newVars, 0, numVars);
    return newVars;
  }

  override def equals(obj: Any): Boolean = {
    if (!(obj.isInstanceOf[CaseEnv]))
      return false;
    val env = obj.asInstanceOf[CaseEnv];
    if (env.numVars != numVars)
      return false;
    var i = 0; while(i < numVars) {
      if ((boundVars(i).name != env.boundVars(i).name) ||
	  !boundVars(i).tpe.getType().isSameAs(env.boundVars(i).tpe.getType()) ||
	  (boundVars(i).rhs != env.boundVars(i).rhs))
	return false;
      i = i + 1;
    }
    return true;
  }

} // class CaseEnv
}
