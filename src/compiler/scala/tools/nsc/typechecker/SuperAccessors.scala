/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.collection.mutable.ListBuffer;
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
    private var validCurrentOwner = true;
    private var accDefs: List[Pair[Symbol, ListBuffer[Tree]]] = List();

    private def accDefBuf(clazz: Symbol) = accDefs.dropWhile(._1.!=(clazz)).head._2;

    private def transformArgs(args: List[Tree], formals: List[Type]) = {
      if (!formals.isEmpty && formals.last.symbol == definitions.ByNameParamClass)
        ((args take (formals.length - 1) map transform) :::
         withInvalidOwner { args drop (formals.length - 1) map transform })
      else
        args map transform
    }

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
	    (clazz.isTrait || clazz != currentOwner.enclClass || !validCurrentOwner)) {
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
	    Select(gen.mkAttributedThis(clazz), superAcc) setType tree.tpe;
	  }
	} else tree
      case Apply(fn, args) =>
        copy.Apply(tree, transform(fn), transformArgs(args, fn.tpe.paramTypes))
      case Function(vparams, body) =>
        withInvalidOwner {
          copy.Function(tree, vparams, transform(body))
        }
      case _ =>
        super.transform(tree)
    }

    override def atOwner[A](owner: Symbol)(trans: => A): A = {
      if (owner.isClass) validCurrentOwner = true;
      super.atOwner(owner)(trans)
    }

    private def withInvalidOwner[A](trans: => A): A = {
      val prevValidCurrentOwner = validCurrentOwner;
      validCurrentOwner = false;
      val result = trans;
      validCurrentOwner = prevValidCurrentOwner;
      result
    }
  }
}
