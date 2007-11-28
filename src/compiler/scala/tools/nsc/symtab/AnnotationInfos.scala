/* NSC -- new Scala compiler
 * Copyright 2007-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.tools.nsc.transform.Reifiers
import util._

/** AnnotationInfo and its helpers */
trait AnnotationInfos {
  self: SymbolTable =>

  /** Convert a tree to a Constant, if possible */
  private def tree2cons(tree: Tree): Option[Constant] =
    tree match {
      case Literal(v) => Some(v)

      case Apply(
        TypeApply(
          meth@Select(_,_),
	  List(elemType)),
	members)
      if (definitions.ArrayModule_apply.alternatives contains meth.symbol) =>
	trees2consArray(members, tree.tpe)


      case Apply(meth, members)
      if (definitions.ArrayModule_apply.alternatives contains meth.symbol) =>
 	trees2consArray(members, tree.tpe)


      case tree =>
        //println("could not convert: " + tree);
        None
    }

  private def trees2consArray(trees: Seq[Tree], arrayType:Type)
  : Option[Constant] =
  {
    val mems = trees.map(tree2cons)

    if (mems.exists(_.isEmpty))
      None
    else
      Some(new ArrayConstant(
	mems.map(_.get).toArray,
	arrayType))
  }


  /** An argument to an annotation.  It includes a parse tree,
   *  and it includes a compile-time constant for the tree if possible.
   */
  class AnnotationArgument(val intTree: Tree) {
    def this(cons: Constant) = this(
      Literal(cons).setType(cons.tpe))


    @deprecated
    lazy val tree = {
      object reifiers extends Reifiers {
	val symbols: AnnotationInfos.this.type = AnnotationInfos.this
      }

      reifiers.reify(intTree)
    }

    val constant: Option[Constant] = tree2cons(intTree)

    def isConstant = !constant.isEmpty

    override def toString: String =
      constant match {
        case Some(cons) => cons.escapedStringValue
        case None => intTree.toString
      }
  }

  /** Typed information about an annotation.  It can be attached to
   *  either a symbol or an annotated type.
   */
  case class AnnotationInfo(
    atp: Type,
    args: List[AnnotationArgument],
    assocs: List[(Name, AnnotationArgument)])
  {
    override def toString: String =
      atp +
      (if (args.isEmpty) ""
       else args.mkString("(", ", ", ")")) +
      (if (assocs.isEmpty) ""
       else (assocs map { case (x, y) => x+" = "+y } mkString ("{", ", ", "}")))

    /** Check whether all arguments and assocations are constants */
    def isConstant =
      ((args forall (_.isConstant)) &&
       (assocs map (_._2) forall (_.isConstant)))

    /** Check whether the type or any of the arguments are erroneous */
    def isErroneous = atp.isErroneous || args.exists(_.intTree.isErroneous)

    /** Check whether any of the arguments mention a symbol */
    def refsSymbol(sym: Symbol) =
      args.exists(_.intTree.exists(_.symbol == sym))

    /** Change all ident's with Symbol "from" to instead use symbol "to" */
    def substIdentSyms(from: Symbol, to: Symbol) = {
      val subs = new TreeSymSubstituter(List(from), List(to))
      AnnotationInfo(atp,
		     args.map(arg => new AnnotationArgument(subs(arg.intTree))),
		     assocs)
    }
  }
}
