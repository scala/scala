/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id: ExplicitOuter.scala 5642 2006-01-26 13:00:58Z odersky $
package scala.tools.nsc.transform;

import symtab._;
import Flags._;

abstract class CheckDefined extends Transform {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "checkdefined";

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CheckDefinedTransformer(unit);

  class CheckDefinedTransformer(unit: CompilationUnit) extends Transformer {

    var qualNode: Tree = EmptyTree;

    private def isAlwaysInitialized(tp: Type): boolean = tp match {
      case ConstantType(_) => true
      case ThisType(_) => true
      case SuperType(_, _) => true
      case SingleType(_, sym) => sym.isModule || isAlwaysInitialized(tp.singleDeref)
      case TypeRef(_, sym, _) => sym.isModuleClass
      case _ => false
    }

    def checkDefined(tree: Tree): Tree = {
/*
      System.out.println("check def? " + tree + " " +
                         (tree ne qualNode) + " " +
                         (tree.symbol hasFlag ACCESSOR) + " " +
                         !(tree.symbol hasFlag PARAMACCESSOR) + " " +
                         (tree.tpe <:< AnyClass.tpe) + " " +
                         (tree.tpe.symbol != AllClass) + " " +
                         !tree.tpe.isNullable + " " +
                         !(tree.tpe <:< AnyValClass.tpe) + " " +
                         !isAlwaysInitialized(tree.tpe));
*/
      if ((tree ne qualNode) &&
          (tree.symbol hasFlag ACCESSOR) &&
          !(tree.symbol hasFlag PARAMACCESSOR) &&
          (tree.tpe <:< AnyClass.tpe) &&
          !(tree.tpe <:< AnyValClass.tpe) &&
          (tree.tpe.symbol != AllClass) &&
          !tree.tpe.isNullable &&
          !isAlwaysInitialized(tree.tpe)) {
        if (settings.debug.value) log("check def " + tree + ":" + tree.tpe + " in " + unit.source.file);
        atPos(tree.pos) {
          Apply(
            TypeApply(
              gen.mkRef(checkDefinedMethod),
              List(TypeTree(tree.tpe))
            ) setType MethodType(List(tree.tpe), tree.tpe),
            List(tree)
          ) setType tree.tpe
        }
      } else tree;
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case Select(qual, name) =>
          val savedQualNode = qualNode;
          qualNode = qual;
          val tree1 = super.transform(tree);
          qualNode = savedQualNode;
          checkDefined(tree1)
        case Apply(fn, List()) =>
          checkDefined(super.transform(tree))
        case _ =>
          super.transform(tree)
      }
    }
  }
}
