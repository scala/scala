/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.collection.mutable.ListBuffer;
import nsc.symtab.Flags._;

/** This phase adds super accessors for all super calls that
 *  either appear in a trait or have as a target a member of some outer class.
 *  It also replaces references to parameter accessors with aliases by super
 *  references to these aliases.
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
      case Select(qual @ This(_), name) =>
        val sym = tree.symbol;
 	if ((sym hasFlag PARAMACCESSOR) && (sym.alias != NoSymbol)) {
          val result = typed {
            Select(
              Super(qual.symbol, nme.EMPTY.toTypeName/*qual.symbol.info.parents.head.symbol.name*/) setPos qual.pos,
              sym.alias) setPos tree.pos
          }
	  if (settings.debug.value)
	    System.out.println("alias replacement: " + tree + " ==> " + result);//debug
          transform(result)
        } else tree
      case Select(sup @ Super(_, mix), name) =>
        val sym = tree.symbol;
	val clazz = sup.symbol;
	if (tree.isTerm && mix == nme.EMPTY.toTypeName &&
	    (clazz.isTrait || clazz != currentOwner.enclClass || !validCurrentOwner)) {
	  val supername = nme.superName(sym.name);
	  var superAcc = clazz.info.decl(supername).suchThat(.alias.==(sym));
	  if (superAcc == NoSymbol) {
	    if (settings.debug.value) log("add super acc " + sym + sym.locationString + " to `" + clazz);//debug
            superAcc =
              clazz.newMethod(tree.pos, supername)
		.setFlag(SUPERACCESSOR | PRIVATE)
		.setInfo(clazz.thisType.memberType(sym))
		.setAlias(sym)
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
