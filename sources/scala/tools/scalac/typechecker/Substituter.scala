/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global};
import scalac.{symtab => scalac_symtab}
import scalac.ApplicationError;
import scalac.util._;
import scalac.ast._;

package scala.tools.scalac.typechecker {

import scalac_symtab._;

// Tree Substitution -------------------------------------------------------------

class Substituter(global: scalac_Global, gen: TreeGen) extends Transformer(global) {

  var tparams: Array[Symbol] = _;
  var targs: Array[Type] = _;
  var typeSubstituter: Type$SubstTypeMap = _;

  def apply(tree: Tree, tparams: Array[Symbol], targs: Array[Type]): Tree = {
    this.tparams = tparams;
    this.targs = targs;
    this.typeSubstituter = new Type$SubstTypeMap(tparams, targs) {
      override def matches(sym1: Symbol, sym2: Symbol): boolean = {
	sym1.name == sym2.name && sym1.owner() == sym2.owner();
      }
    }
    transform(tree);
  }

  val elimInferredPolyMap: Type$Map = new Type$Map() {
    def apply(t: Type): Type = {
      t match {
	case Type$PolyType(tparams1: Array[Symbol], restp: Type) =>
	  if (tparams1.length == tparams.length && tparams1(0) == tparams(0)) {
	    for (val i <- Iterator.range(1, tparams.length))
	      assert(tparams1(i) == tparams(i));
	    return apply(restp);
	  }
	case _ =>
      }
      map(t)
    }
  };

  override def transform(trees: Array[Tree]): Array[Tree] =
    super.transform(trees);

  override def transform(tree: Tree): Tree = {
    // System.out.println("[" + ArrayApply.toString(targs,"",",","") + "/" + ArrayApply.toString(tparams,"",",","") + "]" + tree + "@" + tree.symbol());//DEBUG
    if (tree.getType() != null) {
      tree.setType(
	typeSubstituter.apply(elimInferredPolyMap.apply(tree.getType())));
    }
    tree match {
      case Tree$Ident(name) =>
	if (name.isTypeName()) {
	  val sym: Symbol = tree.symbol();
	  var i = 0;
	  while (i < tparams.length) {
	    if (sym != null && typeSubstituter.matches(tparams(i), sym)) {
	      return gen.mkType(tree.pos, targs(i));
	    }
	    i = i + 1;
	  }
	}
	tree

      case Tree$TypeApply(fun, targs) =>
	var proceed: boolean = true;
	fun.getType() match {
	  case Type$PolyType(tparams1, _) =>
	    if (tparams1.length == tparams.length && tparams1(0) == tparams(0) && targs.length == tparams.length) {
	      proceed = false;
	      for (val i <- Iterator.range(0, tparams.length))
		if (!typeSubstituter.matches(targs(i).getType().symbol(), tparams(i)))
		  proceed = true;
	    }
	  case _ =>
	}
	val fun1: Tree = if (proceed) transform(fun) else fun;
	val targs1: Array[Tree] = transform(targs);
	copy.TypeApply(tree, fun1, targs1);

      case _ =>
	super.transform(tree);
    }
  }
}
}
