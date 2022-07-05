/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.ListMap

/** AnnotationInfo and its helpers */
trait AnnotationInfos extends api.Annotations { self: SymbolTable =>
  import definitions._

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

    def staticAnnotations = annotations filter (_.isStatic)

    def addThrowsAnnotation(throwableSym: Symbol): Self = {
      val throwableTpe = if (throwableSym.isMonomorphicType) throwableSym.tpe else {
        debuglog(s"Encountered polymorphic exception `${throwableSym.fullName}` while parsing class file.")
        // in case we encounter polymorphic exception the best we can do is to convert that type to
        // monomorphic one by introducing existentials, see scala/bug#7009 for details
        existentialAbstraction(throwableSym.typeParams, throwableSym.tpe)
      }
      this withAnnotation AnnotationInfo(appliedType(ThrowsClass, throwableTpe :: Nil), List(Literal(Constant(throwableTpe))), Nil)
    }

    /** Tests for, get, or remove an annotation */
    def hasAnnotation(cls: Symbol): Boolean =
      //OPT inlined from exists to save on #closures; was:  annotations exists (_ matches cls)
      dropOtherAnnotations(annotations, cls) ne Nil

    def getAnnotation(cls: Symbol): Option[AnnotationInfo] =
      //OPT inlined from exists to save on #closures; was:  annotations find (_ matches cls)
      dropOtherAnnotations(annotations, cls) match {
        case ann :: _ => Some(ann)
        case _ => None
      }

    def removeAnnotation(cls: Symbol): Self = filterAnnotations(ann => !(ann matches cls))

    def withAnnotation(annot: AnnotationInfo): Self

    @tailrec private
    def dropOtherAnnotations(anns: List[AnnotationInfo], cls: Symbol): List[AnnotationInfo] = anns match {
      case ann :: rest => if (ann matches cls) anns else dropOtherAnnotations(rest, cls)
      case Nil => Nil
    }
  }

  /**
   * Arguments to constant annotations (Annotations defined in Java or extending
   * ConstantAnnotation). Arguments are either:
   *  - constants
   *  - arrays of constants
   *  - or nested classfile annotations (only for Java annotation)
   *
   * TODO: rename to `ConstantAnnotationArg`
   */
  @nowarn("""cat=deprecation&origin=scala\.reflect\.api\.Annotations\.JavaArgumentApi""")
  sealed abstract class ClassfileAnnotArg extends Product with JavaArgumentApi
  type JavaArgument = ClassfileAnnotArg
  implicit val JavaArgumentTag: ClassTag[ClassfileAnnotArg] = ClassTag[ClassfileAnnotArg](classOf[ClassfileAnnotArg])
  case object UnmappableAnnotArg extends ClassfileAnnotArg

  /** Represents a compile-time Constant (`Boolean`, `Byte`, `Short`,
   *  `Char`, `Int`, `Long`, `Float`, `Double`, `String`, `java.lang.Class` or
   *  an instance of a Java enumeration value).
   */
  case class LiteralAnnotArg(const: Constant) extends ClassfileAnnotArg {
    override def toString = const.escapedStringValue
  }

  /** Represents an array of classfile annotation arguments */
  case class ArrayAnnotArg(args: Array[ClassfileAnnotArg]) extends ClassfileAnnotArg {
    override def toString = args.mkString("[", ", ", "]")
  }

  /** Represents a nested classfile annotation */
  case class NestedAnnotArg(annInfo: AnnotationInfo) extends ClassfileAnnotArg {
    // The nested annotation should not have any Scala annotation arguments
    assert(annInfo.args.isEmpty, annInfo.args)
    override def toString = annInfo.toString
  }

  object AnnotationInfo {
    def marker(atp: Type): AnnotationInfo =
      apply(atp, Nil, Nil)

    def lazily(lazyInfo: => AnnotationInfo) =
      new LazyAnnotationInfo(lazyInfo)

    def lazily(lazySymbol: => Symbol, lazyInfo: => AnnotationInfo) =
      new ExtraLazyAnnotationInfo(lazySymbol, lazyInfo)

    def apply(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)]): AnnotationInfo =
      new CompleteAnnotationInfo(atp, args, assocs)

    def unapply(info: AnnotationInfo): Some[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])] =
      Some((info.atp, info.args, info.assocs))

    def mkFilter(category: Symbol, defaultRetention: Boolean)(ann: AnnotationInfo) =
      (ann.metaAnnotations, ann.defaultTargets) match {
        case (Nil, Nil)      => defaultRetention
        case (Nil, defaults) => defaults contains category
        case (metas, _)      => metas exists (_ matches category)
      }
  }

  class CompleteAnnotationInfo(
    val atp: Type,
    val args: List[Tree],
    val assocs: List[(Name, ClassfileAnnotArg)]
  ) extends AnnotationInfo {
    // Classfile annot: args empty. Scala annot: assocs empty.
    assert(args.isEmpty || assocs.isEmpty, atp)

    // necessary for reification, see Reifiers.scala for more info
    private[this] var orig: Tree = EmptyTree
    def original = orig
    def setOriginal(t: Tree): this.type = {
      orig = t
      this setPos t.pos
      this
    }

    override def toString = completeAnnotationToString(this)
  }

  private[scala] def completeAnnotationToString(annInfo: AnnotationInfo) = {
    import annInfo._
    val s_args = if (!args.isEmpty) args.mkString("(", ", ", ")") else ""
    val s_assocs = if (!assocs.isEmpty) assocs.map { case (x, y) => s"$x = $y" }.mkString("(", ", ", ")") else ""
    s"${atp}${s_args}${s_assocs}"
  }

  /** Symbol annotations parsed in `Namer` (typeCompleter of
   *  definitions) have to be lazy (#1782)
   */
  class LazyAnnotationInfo(lazyInfo: => AnnotationInfo) extends AnnotationInfo {
    private[this] var forced = false
    private lazy val forcedInfo = try lazyInfo finally forced = true

    def atp: Type                               = forcedInfo.atp
    def args: List[Tree]                        = forcedInfo.args
    def assocs: List[(Name, ClassfileAnnotArg)] = forcedInfo.assocs
    def original: Tree                          = forcedInfo.original
    def setOriginal(t: Tree): this.type         = { forcedInfo.setOriginal(t); this }

    // We should always be able to print things without forcing them.
    override def toString = if (forced) forcedInfo.toString else "@<?>"

    override def pos: Position = if (forced) forcedInfo.pos else NoPosition

    override def completeInfo(): Unit = forcedInfo
  }

  final class ExtraLazyAnnotationInfo(sym: => Symbol, lazyInfo: => AnnotationInfo) extends LazyAnnotationInfo(lazyInfo) {
    private[this] lazy val typeSymbol = sym
    override def symbol: Symbol = typeSymbol
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
  abstract class AnnotationInfo extends AnnotationApi {
    def atp: Type
    def args: List[Tree]
    def assocs: List[(Name, ClassfileAnnotArg)]

    def tpe = atp
    def scalaArgs = args
    def javaArgs = ListMap(assocs: _*)

    // necessary for reification, see Reifiers.scala for more info
    def original: Tree
    def setOriginal(t: Tree): this.type

    // see annotationArgRewriter
    lazy val isTrivial = atp.isTrivial && !hasArgWhich(_.isInstanceOf[This])

    private[this] var rawpos: Position = NoPosition
    def pos = rawpos
    def setPos(pos: Position): this.type = { // Syncnote: Setpos inaccessible to reflection, so no sync in rawpos necessary.
      rawpos = pos
      this
    }

    // Forces LazyAnnotationInfo, no op otherwise
    def completeInfo(): Unit = ()

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
      case AnnotatedType(metas, _) => metas
      case _                       => Nil
    }

    /** The default kind of members to which this annotation is attached.
      * For instance, for scala.deprecated defaultTargets =
      * List(getter, setter, beanGetter, beanSetter).
      *
      * NOTE: have to call symbol.initialize, since we won't get any annotations if the symbol hasn't yet been completed
      */
    def defaultTargets = symbol.initialize.annotations map (_.symbol) filter isMetaAnnotation

    // Test whether the typeSymbol of atp conforms to the given class.
    def matches(clazz: Symbol) = !symbol.isInstanceOf[StubSymbol] && (symbol isNonBottomSubClass clazz)
    // All subtrees of all args are considered.
    def hasArgWhich(p: Tree => Boolean) = args exists (_ exists p)

    /** Check whether the type or any of the arguments are erroneous */
    def isErroneous = atp.isErroneous || args.exists(_.isErroneous)

    final def isStatic = symbol.isStaticAnnotation

    /** Check whether any of the arguments mention a symbol */
    def refsSymbol(sym: Symbol) = hasArgWhich(_.symbol == sym)

    def stringArg(index: Int)  = constantAtIndex(index) map (_.stringValue)
    def intArg(index: Int)     = constantAtIndex(index) map (_.intValue)
    def booleanArg(index: Int) = constantAtIndex(index) map (_.booleanValue)
    def symbolArg(index: Int) = argAtIndex(args, index) collect {
      case Apply(fun, Literal(str) :: Nil) if fun.symbol == definitions.Symbol_apply =>
        newTermName(str.stringValue)
    }

    // !!! when annotation arguments are not literals, but any sort of
    // expression, there is a fair chance they will turn up here not as
    // Literal(const) but some arbitrary AST.
    //
    // We recurse over Typed / Annotated trees to allow things like:
    // `@implicitNotFound("$foo": @nowarn)`
    def constantAtIndex(index: Int): Option[Constant] = {
      @tailrec
      def lit(tree: Tree): Option[Constant] = tree match {
        case Literal(c)      => Some(c)
        case Typed(t, _)     => lit(t)
        case Annotated(_, t) => lit(t)
        case _               => None
      }
      if (args.nonEmpty) argAtIndex(args, index).flatMap(lit)
      else if (assocs.nonEmpty) argAtIndex(assocs, index) collect {
        case (_, LiteralAnnotArg(const)) => const
      } else None
    }

    def argAtIndex[T](l: List[T], index: Int): Option[T] =
      if (index < l.size) Some(l(index)) else None

    def transformArgs(f: List[Tree] => List[Tree]): AnnotationInfo =
      new CompleteAnnotationInfo(atp, f(args), assocs)

    override def hashCode = atp.## + args.## + assocs.##
    override def equals(other: Any) = other match {
      case x: AnnotationInfo  => (atp == x.atp) && (args == x.args) && (assocs == x.assocs)
      case _                  => false
    }
  }

  type Annotation = AnnotationInfo
  object Annotation extends AnnotationExtractor {
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, ClassfileAnnotArg]): Annotation =
      AnnotationInfo(tpe, scalaArgs, javaArgs.toList)
    def unapply(annotation: Annotation): Some[(Type, List[Tree], ListMap[Name, ClassfileAnnotArg])] =
      Some((annotation.tpe, annotation.scalaArgs, annotation.javaArgs))
  }
  implicit val AnnotationTag: ClassTag[AnnotationInfo] = ClassTag[AnnotationInfo](classOf[AnnotationInfo])

  protected[scala] def annotationToTree(ann: Annotation): Tree = {
    def reverseEngineerArgs(): List[Tree] = {
      def reverseEngineerArg(jarg: ClassfileAnnotArg): Tree = jarg match {
        case LiteralAnnotArg(const) =>
          val tpe = if (const.tag == UnitTag) UnitTpe else ConstantType(const)
          Literal(const) setType tpe
        case ArrayAnnotArg(jargs) =>
          val args = jargs.map(reverseEngineerArg _)
          // TODO: I think it would be a good idea to typecheck Java annotations using a more traditional algorithm
          // sure, we can't typecheck them as is using the `new jann(foo = bar)` syntax (because jann is going to be an @interface)
          // however we can do better than `typedAnnotation` by desugaring the aforementioned expression to
          // something like `new jann() { override def annotatedType() = ...; override def foo = bar }`
          // and then using the results of that typecheck to produce a Java-compatible classfile entry
          // in that case we're going to have correctly typed Array.apply calls, however that's 2.12 territory
          // and for 2.11 exposing an untyped call to ArrayModule should suffice
          Apply(Ident(ArrayModule), args.toList)
        case NestedAnnotArg(ann: Annotation) =>
          annotationToTree(ann)
        case _ =>
          EmptyTree
      }
      def reverseEngineerArgs(jargs: List[(Name, ClassfileAnnotArg)]): List[Tree] = jargs match {
        case (name, jarg) :: rest => NamedArg(Ident(name), reverseEngineerArg(jarg)) :: reverseEngineerArgs(rest)
        case Nil => Nil
      }
      if (ann.assocs.isEmpty) ann.args
      else reverseEngineerArgs(ann.assocs)
    }

    // TODO: at the moment, constructor selection is unattributed, because AnnotationInfos lack necessary information
    // later on, in 2.12, for every annotation we could save an entire tree instead of just bits and pieces
    // but for 2.11 the current situation will have to do
    val ctorSelection = Select(New(TypeTree(ann.atp)), nme.CONSTRUCTOR)
    Apply(ctorSelection, reverseEngineerArgs()) setType ann.atp
  }

  protected[scala] def treeToAnnotation(tree: Tree): Annotation = tree match {
    case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
      def encodeJavaArg(arg: Tree): ClassfileAnnotArg = arg match {
        case Literal(const) => LiteralAnnotArg(const)
        case Apply(ArrayModule, args) => ArrayAnnotArg(args.map(encodeJavaArg).toArray)
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) => NestedAnnotArg(treeToAnnotation(arg))
        case _ => throw new Exception(s"unexpected java argument shape $arg: literals, arrays and nested annotations are supported")
      }
      // TODO: Java annotations with a single `value` parameter can be created without a named argument.
      def encodeJavaArgs(args: List[Tree]): List[(Name, ClassfileAnnotArg)] = args match {
        case NamedArg(Ident(name), arg) :: rest => (name, encodeJavaArg(arg)) :: encodeJavaArgs(rest)
        case arg :: rest => throw new Exception(s"unexpected java argument shape $arg: only NamedArg trees are supported")
        case Nil => Nil
      }
      val atp = tpt.tpe
      if (atp != null && (atp.typeSymbol isNonBottomSubClass StaticAnnotationClass)) AnnotationInfo(atp, args, Nil)
      else if (atp != null && (atp.typeSymbol.isJavaDefined || atp.typeSymbol.isNonBottomSubClass(ConstantAnnotationClass))) AnnotationInfo(atp, Nil, encodeJavaArgs(args))
      else throw new Exception(s"unexpected annotation type $atp: only subclasses of StaticAnnotation and ClassfileAnnotation are supported")
    case _ =>
      throw new Exception("""unexpected tree shape: only q"new $annType(..$args)" is supported""")
  }

  object UnmappableAnnotation extends CompleteAnnotationInfo(NoType, Nil, Nil)

  class ErroneousAnnotation() extends CompleteAnnotationInfo(ErrorType, Nil, Nil)

  /** Extracts the type of the thrown exception from an AnnotationInfo.
    *
    * Supports both “old-style” `@throws(classOf[Exception])`
    * as well as “new-style” `@throws[Exception]("cause")` annotations.
    */
  object ThrownException {
    def unapply(ann: AnnotationInfo): Option[Type] = ann match {
      case AnnotationInfo(tpe, _, _) if tpe.typeSymbol != ThrowsClass => None
      case AnnotationInfo(_, List(Literal(Constant(tpe: Type))), _)   => Some(tpe) // old-style
      case AnnotationInfo(TypeRef(_, _, arg :: _), _, _)              => Some(arg) // new-style
      case AnnotationInfo(TypeRef(_, _, Nil), _, _)                   => Some(ThrowableTpe)
      case _                                                          => None
    }
  }
}
