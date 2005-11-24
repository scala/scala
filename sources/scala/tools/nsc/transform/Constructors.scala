/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

import symtab._;
import Flags._;
import util.{ListBuffer, TreeSet}

abstract class Constructors extends Transform {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors";

  protected def newTransformer(unit: CompilationUnit): Transformer = new ConstructorTransformer;

  class ConstructorTransformer extends Transformer {

    def transformClassTemplate(impl: Template): Template = {
      val clazz = impl.symbol.owner;
      val stats = impl.body;
      val localTyper = typer.atOwner(impl, clazz);
      var constr: DefDef = null;
      var constrParams: List[Symbol] = null;
      var constrBody: Block = null;
      // decompose primary constructor into the three entities above.
      for (val stat <- stats) {
	stat match {
	  case ddef @ DefDef(_, _, _, List(vparams), _, rhs @ Block(_, Literal(_))) =>
	    if (ddef.symbol.isPrimaryConstructor) {
	      constr = ddef;
	      constrParams = vparams map (.symbol);
	      constrBody = rhs
	    }
	  case _ =>
	}
      }

      val paramAccessors = clazz.constrParamAccessors;
      def parameter(acc: Symbol) = {
        val accname = nme.getterName(acc.originalName);
        val ps = constrParams.filter { param => accname == param.name }
        if (ps.isEmpty) assert(false, "" + accname + " not in " + constrParams);
        ps.head
      }

      val intoConstructorTransformer = new Transformer {
	override def transform(tree: Tree): Tree = tree match {
	  case Apply(Select(This(_), _), List())
	  if ((tree.symbol hasFlag PARAMACCESSOR) && tree.symbol.owner == clazz) =>
            gen.Ident(parameter(tree.symbol.accessed)) setPos tree.pos
	  case Select(This(_), _)
	  if ((tree.symbol hasFlag PARAMACCESSOR) && tree.symbol.owner == clazz) =>
	    gen.Ident(parameter(tree.symbol)) setPos tree.pos
	  case _ =>
	    super.transform(tree)
	}
      }

      def intoConstructor(oldowner: Symbol, tree: Tree) =
	intoConstructorTransformer.transform(
          new ChangeOwnerTraverser(oldowner, constr.symbol)(tree));

      def mkAssign(to: Symbol, from: Tree): Tree =
	atPos(to.pos) {
	  localTyper.typed {
	    Assign(Select(This(clazz), to), from)
	  }
	}

      val defBuf = new ListBuffer[Tree];
      val constrStatBuf = new ListBuffer[Tree];
      constrBody.stats foreach (constrStatBuf +=);

      for (val stat <- stats) stat match {
	case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
	  stat.symbol.tpe match {
	    case MethodType(List(), tp @ ConstantType(c)) =>
	      defBuf += copy.DefDef(
		stat, mods, name, tparams, vparamss, tpt,
		Literal(c) setPos rhs.pos setType tp)
	    case _ =>
	      if (!stat.symbol.isPrimaryConstructor) defBuf += stat
	  }
	case ValDef(mods, name, tpt, rhs) =>
	  if (stat.symbol.tpe.isInstanceOf[ConstantType])
	    assert(stat.symbol.getter != NoSymbol, stat)
	  else {
	    if (rhs != EmptyTree) {
	      constrStatBuf += mkAssign(stat.symbol, intoConstructor(stat.symbol, rhs));
	    }
	    defBuf += copy.ValDef(stat, mods, name, tpt, EmptyTree)
	  }
        case ClassDef(_, _, _, _, _) =>
          defBuf += transform(stat)
	case _ =>
	  constrStatBuf += intoConstructor(impl.symbol, stat)
      }

      val accessed = new TreeSet[Symbol]((x, y) => x isLess y);

      def isAccessed(sym: Symbol) =
	sym.owner != clazz ||
	!(sym hasFlag PARAMACCESSOR) ||
	!(sym hasFlag LOCAL) ||
	(accessed contains sym);

      val accessTraverser = new Traverser {
        override def traverse(tree: Tree) = {
	  tree match {
            case Select(_, _) =>
              if (!isAccessed(tree.symbol)) accessed addEntry tree.symbol;
            case _ =>
          }
          super.traverse(tree)
	}
      }

      for (val stat <- defBuf.elements) accessTraverser.traverse(stat);

      val paramInits = for (val acc <- paramAccessors; isAccessed(acc))
		       yield mkAssign(acc, Ident(parameter(acc)));

      defBuf += copy.DefDef(
	constr, constr.mods, constr.name, constr.tparams, constr.vparamss, constr.tpt,
	copy.Block(constrBody, paramInits ::: constrStatBuf.toList, constrBody.expr));

      copy.Template(impl, impl.parents, defBuf.toList filter (stat => isAccessed(stat.symbol)))
    }

    override def transform(tree: Tree): Tree = tree match {
      case ClassDef(mods, name, tparams, tpt, impl) if !tree.symbol.hasFlag(INTERFACE) =>
	copy.ClassDef(tree, mods, name, tparams, tpt, transformClassTemplate(impl))
      case _ =>
        super.transform(tree)
    }
  }
}
