/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.tools.nsc.transform.Reifiers
import util._

/** AnnotationInfo and its helpers */
trait AnnotationInfos {
  self: SymbolTable =>

  /** <p>An argument to a Scala annotation. Usually created with a
   *  compiler tree representing the argument. If the tree can
   *  be converted to a compile-time Constant, the method
   *  <code>constant</code> returns <code>Some(c)</code>, and
   *  the Pickler will write the constant to the classfile,
   *  instead of the tree (optimisation).</p>
   */
  case class AnnotationArgument(intTree: Tree) {

    /** This constructor is only used by the UnPickler, if it reads
     *  an AnnotationArgument that was pickled as <code>Constant</code>.
     */
    def this(cons: Constant) {
      this(Literal(cons).setType(cons.tpe))
    }

    /** Contains <code>Some(c)</code> if the <code>intTree</code> can be
     *  converted into a compile-time Constant. Used to pickle Literals
     *  as Constants
     */
    val constant = intTree match {
      case Literal(c) => Some(c)
      case _ => None
    }

    def isConstant = !constant.isEmpty

    override def toString = intTree.toString
  }

  /** Subclasses of this class are used to represent Arguments
   *  to Java-Annotations.
   */
  abstract class ConstantAnnotationArgument {
    /** A tree representing that constant. Required to store java
     *  annotations in the pickle.
     */
    def toTree: Tree = this match {
      case LiteralAnnotationArgument(const) =>
        Literal(const)
      case ArrayAnnotationArgument(args) =>
        Apply(Ident("array"), args.toList map (_.toTree))
      case NestedAnnotationArgument(annInfo) =>
        Apply(Ident("annotation"), annInfo.assocs.map(asc => Assign(Ident(asc._1), asc._2.toTree))) setType annInfo.atp
    }
  }

  /** Represents a compile-time Constant (Boolean, Byte, Short,
   *  Char, Int, Long, Float, Double, String, java.lang.Class or
   *  an instance of a Java enumeration value).
   */
  case class LiteralAnnotationArgument(const: Constant)
  extends ConstantAnnotationArgument {
    override def toString = const.escapedStringValue
  }

  /** Represents an array of constants */
  case class ArrayAnnotationArgument(args: Array[ConstantAnnotationArgument])
  extends ConstantAnnotationArgument {
    override def toString = args.mkString("[", ", ", "]")
  }

  /** Represents a constant nested Java annotation */
  case class NestedAnnotationArgument(annInfo: AnnotationInfo)
  extends ConstantAnnotationArgument {
    // The nested annotation should not have any Scala annotation arguments
    assert(annInfo.args.isEmpty, annInfo.args)
    override def toString = annInfo.toString
  }

  class AnnotationInfoBase

  /** <p>Typed information about an annotation. It can be attached to
   *  either a symbol or an annotated type.</p>
   *  <p>Annotations are written to the classfile as Java annotations
   *  if <code>atp</code> conforms to <code>ClassfileAnnotation</code>
   *  (the classfile parser adds this interface to any Java annotation
   *  class).</p>
   *  <p>Annotations are pickled (written to scala symbtab attribute
   *  in the classfile) if <code>atp</code> inherits form
   *  <code>StaticAnnotation</code>.</p>
   *  <p>Arguments to a Scala annotation are stored in the parameter
   *  <code>args</code>. They are represented as compiler trees in
   *  general, see class AnnotationInfo. Argument to Java annotaions
   *  are stored as name-value pairs in <code>assocs</code>.</p>
   */
  case class AnnotationInfo(atp: Type, args: List[AnnotationArgument],
                            assocs: List[(Name, ConstantAnnotationArgument)])
  extends AnnotationInfoBase {

    assert(args.isEmpty || assocs.isEmpty) // Java: args empty. Scala: assocs empty.

    override def toString: String = atp +
      (if (!args.isEmpty) args.mkString("(", ", ", ")") else "") +
      (if (!assocs.isEmpty) (assocs map { case (x, y) => x+" = "+y } mkString ("(", ", ", ")")) else "")

    /** Check whether all arguments and assocations are constants */
    def isConstant = (args forall (_.isConstant))

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

  // Definition annotations parsed in Namer (typeCompleter of definitions) have to be lazy (#1782)
  case class LazyAnnotationInfo(annot: () => AnnotationInfo) extends AnnotationInfoBase
}
