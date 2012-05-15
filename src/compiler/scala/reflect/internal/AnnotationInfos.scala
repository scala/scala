/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import util._
import pickling.ByteCodecs

/** AnnotationInfo and its helpers */
trait AnnotationInfos extends api.AnnotationInfos { self: SymbolTable =>
  import definitions.{ ThrowsClass, StaticAnnotationClass, isMetaAnnotation }

  // Common annotation code between Symbol and Type.
  // For methods altering the annotation list, on Symbol it mutates
  // the Symbol's field directly.  For Type, a new AnnotatedType is
  // created which wraps the original type.
  trait Annotatable[Self] {
    /** The annotations on this type. */
    def annotations: List[AnnotationInfo]                     // Annotations on this type.
    def setAnnotations(annots: List[AnnotationInfo]): Self    // Replace annotations with argument list.
    def withAnnotations(annots: List[AnnotationInfo]): Self   // Add annotations to this type.
    def filterAnnotations(p: AnnotationInfo => Boolean): Self // Retain only annotations meeting the condition.
    def withoutAnnotations: Self                              // Remove all annotations from this type.

    /** Symbols of any @throws annotations on this symbol.
     */
    def throwsAnnotations(): List[Symbol] = annotations collect {
      case AnnotationInfo(tp, Literal(Constant(tpe: Type)) :: Nil, _) if tp.typeSymbol == ThrowsClass => tpe.typeSymbol
    }

    /** Test for, get, or remove an annotation */
    def hasAnnotation(cls: Symbol) = annotations exists (_ matches cls)
    def getAnnotation(cls: Symbol) = annotations find (_ matches cls)
    def removeAnnotation(cls: Symbol): Self = filterAnnotations(ann => !(ann matches cls))
    final def withAnnotation(annot: AnnotationInfo): Self = withAnnotations(List(annot))
  }

  /** Arguments to classfile annotations (which are written to
   *  bytecode as java annotations) are either:
   *
   *  - constants
   *  - arrays of constants
   *  - or nested classfile annotations
   */
  abstract class ClassfileAnnotArg extends Product

  /** Represents a compile-time Constant (`Boolean`, `Byte`, `Short`,
   *  `Char`, `Int`, `Long`, `Float`, `Double`, `String`, `java.lang.Class` or
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

  /** A specific annotation argument that encodes an array of bytes as an
   *  array of `Long`. The type of the argument declared in the annotation
   *  must be `String`. This specialised class is used to encode Scala
   *  signatures for reasons of efficiency, both in term of class-file size
   *  and in term of compiler performance.
   */
  case class ScalaSigBytes(bytes: Array[Byte]) extends ClassfileAnnotArg {
    override def toString = (bytes map { byte => (byte & 0xff).toHexString }).mkString("[ ", " ", " ]")
    lazy val encodedBytes = ByteCodecs.encode(bytes)    // TODO remove after migration to ASM-based GenJVM complete
    def isLong: Boolean = (encodedBytes.length > 65535) // TODO remove after migration to ASM-based GenJVM complete
    lazy val sevenBitsMayBeZero: Array[Byte] = {
      mapToNextModSevenBits(scala.reflect.internal.pickling.ByteCodecs.encode8to7(bytes))
    }
    def fitsInOneString: Boolean = {
      val numZeros = (sevenBitsMayBeZero count { b => b == 0 })
      val res = (sevenBitsMayBeZero.length + numZeros) <= 65535
      assert(this.isLong == !res, "As things stand, can't just swap in `fitsInOneString()` for `isLong()`")
      res
    }
    def sigAnnot: Type =
      if (this.isLong)
        definitions.ScalaLongSignatureAnnotation.tpe
      else
        definitions.ScalaSignatureAnnotation.tpe

    private def mapToNextModSevenBits(src: Array[Byte]): Array[Byte] = {
      var i = 0
      val srclen = src.length
      while (i < srclen) {
        val in = src(i)
        src(i) = (if (in == 0x7f) 0.toByte else (in + 1).toByte)
        i += 1
      }
      src
    }

  }

  /** Represents a nested classfile annotation */
  case class NestedAnnotArg(annInfo: AnnotationInfo) extends ClassfileAnnotArg {
    // The nested annotation should not have any Scala annotation arguments
    assert(annInfo.args.isEmpty, annInfo.args)
    override def toString = annInfo.toString
  }

  object NestedAnnotArg extends NestedAnnotArgExtractor

  object AnnotationInfo extends AnnotationInfoExtractor {
    def marker(atp: Type): AnnotationInfo =
      apply(atp, Nil, Nil)

    def lazily(lazyInfo: => AnnotationInfo) =
      new LazyAnnotationInfo(lazyInfo)

    def apply(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)]): AnnotationInfo =
      new CompleteAnnotationInfo(atp, args, assocs)

    def unapply(info: AnnotationInfo): Option[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])] =
      Some((info.atp, info.args, info.assocs))
  }

  class CompleteAnnotationInfo(
    val atp: Type,
    val args: List[Tree],
    val assocs: List[(Name, ClassfileAnnotArg)]
  ) extends AnnotationInfo {
    // Classfile annot: args empty. Scala annot: assocs empty.
    assert(args.isEmpty || assocs.isEmpty, atp)

    // necessary for reification, see Reifiers.scala for more info
    private var orig: Tree = EmptyTree
    def original = orig
    def setOriginal(t: Tree): this.type = {
      orig = t
      this setPos t.pos
      this
    }

    override def toString = (
      atp +
      (if (!args.isEmpty) args.mkString("(", ", ", ")") else "") +
      (if (!assocs.isEmpty) (assocs map { case (x, y) => x+" = "+y } mkString ("(", ", ", ")")) else "")
    )
  }

  /** Symbol annotations parsed in `Namer` (typeCompleter of
   *  definitions) have to be lazy (#1782)
   */
  final class LazyAnnotationInfo(lazyInfo: => AnnotationInfo) extends AnnotationInfo {
    private var forced = false
    private lazy val forcedInfo =
      try {
        val result = lazyInfo
        if (result.pos == NoPosition) result setPos pos
        result
      } finally forced = true

    def atp: Type                               = forcedInfo.atp
    def args: List[Tree]                        = forcedInfo.args
    def assocs: List[(Name, ClassfileAnnotArg)] = forcedInfo.assocs
    def original: Tree                          = forcedInfo.original
    def setOriginal(t: Tree): this.type         = { forcedInfo.setOriginal(t); this }

    // We should always be able to print things without forcing them.
    override def toString = if (forced) forcedInfo.toString else "@<?>"

    override def pos: Position = if (forced) forcedInfo.pos else NoPosition
  }

  /** Typed information about an annotation. It can be attached to either
   *  a symbol or an annotated type.
   *
   *  Annotations are written to the classfile as Java annotations
   *  if `atp` conforms to `ClassfileAnnotation` (the classfile parser adds
   *  this interface to any Java annotation class).
   *
   *  Annotations are pickled (written to scala symtab attribute in the
   *  classfile) if `atp` inherits form `StaticAnnotation`.
   *
   *  `args` stores arguments to Scala annotations, represented as typed
   *  trees. Note that these trees are not transformed by any phases
   *  following the type-checker.
   *
   *  `assocs` stores arguments to classfile annotations as name-value pairs.
   */
  sealed abstract class AnnotationInfo {
    def atp: Type
    def args: List[Tree]
    def assocs: List[(Name, ClassfileAnnotArg)]

    // necessary for reification, see Reifiers.scala for more info
    def original: Tree
    def setOriginal(t: Tree): this.type

    // see annotationArgRewriter
    lazy val isTrivial = atp.isTrivial && !hasArgWhich(_.isInstanceOf[This])

    private var rawpos: Position = NoPosition
    def pos = rawpos
    def setPos(pos: Position): this.type = { // Syncnote: Setpos inaccessible to reflection, so no sync in rawpos necessary.
      rawpos = pos
      this
    }

    /** Annotations annotating annotations are confusing so I drew
     *  an example.  Given the following code:
     *
     *  class A {
     *    @(deprecated @setter) @(inline @getter)
     *    var x: Int = 0
     *  }
     *
     *  For the setter `x_=` in A, annotations contains one AnnotationInfo =
     *    List(deprecated @setter)
     *  The single AnnotationInfo in that list, i.e. `@(deprecated @setter)`, has metaAnnotations =
     *    List(setter)
     *
     *  Similarly, the getter `x` in A has an @inline annotation, which has
     *  metaAnnotations = List(getter).
     */
    def symbol = atp.typeSymbol

    /** These are meta-annotations attached at the use site; they
     *  only apply to this annotation usage.  For instance, in
     *    `@(deprecated @setter @field) val ...`
     *  metaAnnotations = List(setter, field).
     */
    def metaAnnotations: List[AnnotationInfo] = atp match {
      case AnnotatedType(metas, _, _) => metas
      case _                          => Nil
    }

    /** The default kind of members to which this annotation is attached.
     *  For instance, for scala.deprecated defaultTargets =
     *    List(getter, setter, beanGetter, beanSetter).
     */
    def defaultTargets = symbol.annotations map (_.symbol) filter isMetaAnnotation
    // Test whether the typeSymbol of atp conforms to the given class.
    def matches(clazz: Symbol) = symbol isNonBottomSubClass clazz
    // All subtrees of all args are considered.
    def hasArgWhich(p: Tree => Boolean) = args exists (_ exists p)

    /** Check whether the type or any of the arguments are erroneous */
    def isErroneous = atp.isErroneous || args.exists(_.isErroneous)

    def isStatic = symbol isNonBottomSubClass StaticAnnotationClass

    /** Check whether any of the arguments mention a symbol */
    def refsSymbol(sym: Symbol) = hasArgWhich(_.symbol == sym)

    /** Change all ident's with Symbol "from" to instead use symbol "to" */
    def substIdentSyms(from: Symbol, to: Symbol) =
      AnnotationInfo(atp, args map (_ substTreeSyms (from -> to)), assocs) setPos pos

    def stringArg(index: Int) = constantAtIndex(index) map (_.stringValue)
    def intArg(index: Int)    = constantAtIndex(index) map (_.intValue)
    def symbolArg(index: Int) = argAtIndex(index) collect {
      case Apply(fun, Literal(str) :: Nil) if fun.symbol == definitions.Symbol_apply =>
        newTermName(str.stringValue)
    }

    // !!! when annotation arguments are not literals, but any sort of
    // expression, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.
    def constantAtIndex(index: Int): Option[Constant] =
      argAtIndex(index) collect { case Literal(x) => x }

    def argAtIndex(index: Int): Option[Tree] =
      if (index < args.size) Some(args(index)) else None

    override def hashCode = atp.## + args.## + assocs.##
    override def equals(other: Any) = other match {
      case x: AnnotationInfo  => (atp == x.atp) && (args == x.args) && (assocs == x.assocs)
      case _                  => false
    }
  }

  lazy val classfileAnnotArgTag: ArrayTag[ClassfileAnnotArg] = arrayTag[ClassfileAnnotArg]

  object UnmappableAnnotation extends CompleteAnnotationInfo(NoType, Nil, Nil)
}
