/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import scala.tools.nsc.transform.Reifiers
import util._

/** AnnotationInfo and its helpers */
trait AnnotationInfos extends reflect.generic.AnnotationInfos { self: SymbolTable =>

  /** Arguments to classfile annotations (which are written to
   *  bytecode as java annotations) are either:
   *  <ul>
   *   <li>constants</li>
   *   <li>arrays of constants</li>
   *   <li>or nested classfile annotations</li>
   *  </ul>
   */
  abstract class ClassfileAnnotArg

  /** Represents a compile-time Constant (Boolean, Byte, Short,
   *  Char, Int, Long, Float, Double, String, java.lang.Class or
   *  an instance of a Java enumeration value).
   */
  case class LiteralAnnotArg(const: Constant)
  extends ClassfileAnnotArg {
    override def toString = const.escapedStringValue
  }

  object LiteralAnnotArg extends LiteralAnnotArgExtractor

  /** Represents an array of classfile annotation arguments */
  case class ArrayAnnotArg(args: Array[ClassfileAnnotArg])
  extends ClassfileAnnotArg {
    override def toString = args.mkString("[", ", ", "]")
  }

  object ArrayAnnotArg extends ArrayAnnotArgExtractor

  /** A specific annotation argument that encodes an array of bytes as an array of `Long`. The type of the argument
    * declared in the annotation must be `String`. This specialised class is used to encode scala signatures for
    * reasons of efficiency, both in term of class-file size and in term of compiler performance. */
  case class ScalaSigBytes(bytes: Array[Byte]) extends ClassfileAnnotArg {
    override def toString = (bytes map { byte => (byte & 0xff).toHexString }).mkString("[ ", " ", " ]")
    lazy val encodedBytes =
      reflect.generic.ByteCodecs.encode(bytes)
    def isLong: Boolean = (encodedBytes.length > 65535)
    def sigAnnot: Type =
      if (this.isLong)
        definitions.ScalaLongSignatureAnnotation.tpe
      else
        definitions.ScalaSignatureAnnotation.tpe
  }

  /** Represents a nested classfile annotation */
  case class NestedAnnotArg(annInfo: AnnotationInfo)
  extends ClassfileAnnotArg {
    // The nested annotation should not have any Scala annotation arguments
    assert(annInfo.args.isEmpty, annInfo.args)
    override def toString = annInfo.toString
  }

  object NestedAnnotArg extends NestedAnnotArgExtractor

  class AnnotationInfoBase

  /** <p>
   *    Typed information about an annotation. It can be attached to
   *    either a symbol or an annotated type.
   *  </p>
   *  <p>
   *    Annotations are written to the classfile as java annotations
   *    if <code>atp</code> conforms to <code>ClassfileAnnotation</code>
   *    (the classfile parser adds this interface to any Java annotation
   *    class).
   *  </p>
   *  <p>
   *    Annotations are pickled (written to scala symtab attribute
   *    in the classfile) if <code>atp</code> inherits form
   *    <code>StaticAnnotation</code>.
   *  </p>
   *  <p>
   *    <code>args</code> stores arguments to Scala annotations,
   *    represented as  typed trees. Note that these trees are not
   *    transformed by any phases following the type-checker.
   *  </p>
   *  <p>
   *    <code>assocs</code> stores arguments to classfile annotations
   *    as name-value pairs.
   *  </p>
   */
  case class AnnotationInfo(atp: Type, args: List[Tree],
                            assocs: List[(Name, ClassfileAnnotArg)])
  extends AnnotationInfoBase {

    // Classfile annot: args empty. Scala annot: assocs empty.
    assert(args.isEmpty || assocs.isEmpty)

    private var rawpos: Position = NoPosition
    def pos = rawpos
    def setPos(pos: Position): this.type = {
      rawpos = pos
      this
    }

    lazy val isTrivial: Boolean = atp.isTrivial && !(args exists (_.exists(_.isInstanceOf[This]))) // see annotationArgRewriter

    override def toString: String = atp +
      (if (!args.isEmpty) args.mkString("(", ", ", ")") else "") +
      (if (!assocs.isEmpty) (assocs map { case (x, y) => x+" = "+y } mkString ("(", ", ", ")")) else "")

    /** Check whether the type or any of the arguments are erroneous */
    def isErroneous = atp.isErroneous || args.exists(_.isErroneous)

    /** Check whether any of the arguments mention a symbol */
    def refsSymbol(sym: Symbol) =
      args.exists(_.exists(_.symbol == sym))

    /** Change all ident's with Symbol "from" to instead use symbol "to" */
    def substIdentSyms(from: Symbol, to: Symbol) = {
      val subs = new TreeSymSubstituter(List(from), List(to))
      AnnotationInfo(atp, args.map(subs(_)), assocs).setPos(pos)
    }

    // !!! when annotation arguments are not literal strings, but any sort of
    // assembly of strings, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.
    def stringArg(index: Int): Option[String] = if(args.size > index) Some(args(index) match {
      case Literal(const) => const.stringValue
      case x              => x.toString // should not be necessary, but better than silently ignoring an issue
    }) else None

    def intArg(index: Int): Option[Int] = if(args.size > index) Some(args(index)) collect {
      case Literal(Constant(x: Int)) => x
    } else None
  }

  object AnnotationInfo extends AnnotationInfoExtractor

  lazy val classfileAnnotArgManifest: ClassManifest[ClassfileAnnotArg] =
    reflect.ClassManifest.classType(classOf[ClassfileAnnotArg])

  /** Symbol annotations parsed in Namer (typeCompleter of
   *  definitions) have to be lazy (#1782)
   */
  case class LazyAnnotationInfo(annot: () => AnnotationInfo)
  extends AnnotationInfoBase
}
