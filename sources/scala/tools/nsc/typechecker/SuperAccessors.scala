/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.typechecker;

import nsc.util.ListBuffer;
import nsc.symtab.Flags._;

/** A sample transform.
 */
abstract class SuperAccessors extends transform.Transform {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._;
  import posAssigner.atPos;
  import typer.typed;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "superaccessors";

  protected def newTransformer(unit: CompilationUnit): Transformer = new SuperAccTransformer;

  class SuperAccTransformer extends Transformer {
    var accDefs: List[Pair[Symbol, ListBuffer[Tree]]] = List();

    private def accDefBuf(clazz: Symbol) = accDefs.dropWhile(._1.!=(clazz)).head._2;

    override def transform(tree: Tree): Tree = tree match {
      case Template(parents, body) =>
	val ownAccDefs = new ListBuffer[Tree];
	accDefs = Pair(currentOwner, ownAccDefs) :: accDefs;
	val body1 = transformTrees(body);
	accDefs = accDefs.tail;
	copy.Template(tree, parents, ownAccDefs.toList ::: body1);
      case Select(sup @ Super(_, mix), name) =>
	val clazz = sup.symbol;
	if (tree.isTerm && mix == nme.EMPTY.toTypeName &&
	    (clazz.isTrait || clazz != currentOwner.enclClass)) {
	  val supername = nme.superName(tree.symbol.name);
	  var superAcc = clazz.info.decl(supername).suchThat(.alias.==(tree.symbol));
	  if (superAcc == NoSymbol) {
	    if (settings.debug.value) log("add super acc " + tree.symbol + tree.symbol.locationString + " to `" + clazz);//debug
            superAcc =
              clazz.newMethod(tree.pos, supername)
		.setFlag(SUPERACCESSOR | PRIVATE)
		.setAlias(tree.symbol)
		.setInfo(clazz.thisType.memberType(tree.symbol));
            clazz.info.decls enter superAcc;
	    accDefBuf(clazz) += typed(DefDef(superAcc, vparamss => EmptyTree))
	  }
	  atPos(sup.pos) {
	    Select(gen.This(clazz), superAcc) setType tree.tpe;
	  }
	} else tree
      case _ =>
        super.transform(tree)
    }
  }
}
