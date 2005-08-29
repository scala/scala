/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

import symtab._;
import Flags._;
import util.ListBuffer;
import collection.mutable.HashMap;

abstract class Flatten extends InfoTransform {
  import global._;
  import definitions._;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "flatten";

  private val flattened = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if (pre.symbol.isClass && !pre.symbol.isPackageClass) =>
        assert(args.isEmpty);
        typeRef(sym.toplevelClass.owner.thisType, sym, args)
      case ClassInfoType(parents, decls, clazz) =>
	val parents1 = List.mapConserve(parents)(this);
	if (parents1 eq parents) tp
	else ClassInfoType(parents1, decls, clazz)
      case _ =>
        mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type): Type = flattened(tp);

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new Flattener(unit);

  class Flattener(unit: CompilationUnit) extends Transformer {

    /** Buffers for lifted out classes */
    private val liftedDefs = new HashMap[Symbol, ListBuffer[Tree]];

    override def transform(tree: Tree): Tree = {
      tree match {
      	case PackageDef(_, _) =>
          liftedDefs(tree.symbol.moduleClass) = new ListBuffer;
	case _ =>
      }
      postTransform(super.transform(tree))
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol;
      val tree1 = tree match {
        case ClassDef(_, _, _, _, _) if sym.isNestedClass =>
	  liftedDefs(sym.toplevelClass.owner) += tree;
	  EmptyTree
	case Super(qual, mix) if (mix != nme.EMPTY.toTypeName) =>
	  val ps = tree.symbol.info.parents dropWhile (p => p.symbol.name != mix);
	  assert(!ps.isEmpty, tree);
	  val mix1 = if (ps.head.symbol.isNestedClass) atPhase(phase.next)(ps.head.symbol.name)
		     else mix;
	  copy.Super(tree, qual, mix1)
        case _ =>
          tree
      }
      tree1 setType flattened(tree1.tpe);
      if (sym != null && sym.isNestedClass && !(sym hasFlag FLATTENED)) {
	sym setFlag FLATTENED;
	atPhase(phase.next) {
	  System.out.println("re-enter " + sym + " in " + sym.owner);//debug
	  val scope = sym.owner.info.decls;
	  val old = scope lookup sym.name;
	  if (old != NoSymbol) scope unlink old;
	  scope enter sym;
	}
      }
      tree1
    }

    /** Transform statements and add lifted definitions to them. */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val stats1 = super.transformStats(stats, exprOwner);
      if (currentOwner.isPackageClass && liftedDefs(currentOwner).hasNext)
        stats1 ::: liftedDefs(currentOwner).toList
      else
        stats1
    }
  }
}
