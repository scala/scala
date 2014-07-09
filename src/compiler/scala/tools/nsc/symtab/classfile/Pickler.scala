/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab
package classfile

import java.lang.Float.floatToIntBits
import java.lang.Double.doubleToLongBits
import scala.io.Codec
import scala.reflect.internal.pickling.{ PickleBuffer, PickleFormat }
import scala.reflect.internal.util.shortClassOfInstance
import scala.collection.mutable.LinkedHashMap
import PickleFormat._
import Flags._

/**
 * Serialize a top-level module and/or class.
 *
 * @see EntryTags.scala for symbol table attribute format.
 *
 * @author Martin Odersky
 * @version 1.0
 */
abstract class Pickler extends SubComponent {
  import global._

  val phaseName = "pickler"

  def newPhase(prev: Phase): StdPhase = new PicklePhase(prev)

  class PicklePhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      def pickle(tree: Tree) {
        def add(sym: Symbol, pickle: Pickle) = {
          if (currentRun.compiles(sym) && !currentRun.symData.contains(sym)) {
            debuglog("pickling " + sym)
            pickle putSymbol sym
            currentRun.symData(sym) = pickle
          }
        }

        tree match {
          case PackageDef(_, stats) =>
            stats foreach pickle
          case ClassDef(_, _, _, _) | ModuleDef(_, _, _) =>
            val sym = tree.symbol
            val pickle = new Pickle(sym)
            add(sym, pickle)
            add(sym.companionSymbol, pickle)
            pickle.writeArray()
            currentRun registerPickle sym
          case _ =>
        }
      }

      try {
        pickle(unit.body)
      } catch {
        case e: FatalError =>
          for (t <- unit.body) {
            // If there are any erroneous types in the tree, then we will crash
            // when we pickle it: so let's report an error instead.  We know next
            // to nothing about what happened, but our supposition is a lot better
            // than "bad type: <error>" in terms of explanatory power.
            //
            // OPT: do this only as a recovery after fatal error. Checking in advance was expensive.
            if (t.isErroneous) {
              if (settings.debug) e.printStackTrace()
              reporter.error(t.pos, "erroneous or inaccessible type")
              return
            }
          }
          throw e
      }
    }
  }

  private class Pickle(root: Symbol) extends PickleBuffer(new Array[Byte](4096), -1, 0) {
    private val rootName  = root.name.toTermName
    private val rootOwner = root.owner
    private var entries   = new Array[AnyRef](256)
    private var ep        = 0
    private val index     = new LinkedHashMap[AnyRef, Int]
    private lazy val nonClassRoot = findSymbol(root.ownersIterator)(!_.isClass)

    private def isRootSym(sym: Symbol) =
      sym.name.toTermName == rootName && sym.owner == rootOwner

    /** Returns usually symbol's owner, but picks classfile root instead
     *  for existentially bound variables that have a non-local owner.
     *  Question: Should this be done for refinement class symbols as well?
     *
     *  Note: tree pickling also finds its way here; e.g. in SI-7501 the pickling
     *  of trees in annotation arguments considers the parameter symbol of a method
     *  called in such a tree as "local". The condition `sym.isValueParameter` was
     *  added to fix that bug, but there may be a better way.
     */
    private def localizedOwner(sym: Symbol) =
      if (isLocalToPickle(sym) && !isRootSym(sym) && !isLocalToPickle(sym.owner))
        // don't use a class as the localized owner for type parameters that are not owned by a class: those are not instantiated by asSeenFrom
        // however, they would suddenly be considered by asSeenFrom if their localized owner became a class (causing the crashes of #4079, #2741)
        (if ((sym.isTypeParameter || sym.isValueParameter) && !sym.owner.isClass) nonClassRoot
         else root)
      else sym.owner

    /** Is root in symbol.owner*, or should it be treated as a local symbol
     *  anyway? This is the case if symbol is a refinement class,
     *  an existentially bound variable, or a higher-order type parameter.
     */
    private def isLocalToPickle(sym: Symbol): Boolean = (sym != NoSymbol) && !sym.isPackageClass && (
         isRootSym(sym)
      || sym.isRefinementClass
      || sym.isAbstractType && sym.hasFlag(EXISTENTIAL) // existential param
      || sym.isParameter
      || isLocalToPickle(sym.owner)
    )
    private def isExternalSymbol(sym: Symbol): Boolean = (sym != NoSymbol) && !isLocalToPickle(sym)

    // Phase 1 methods: Populate entries/index ------------------------------------

    /** Store entry e in index at next available position unless
     *  it is already there.
     *
     *  @return      true iff entry is new.
     */
    private def putEntry(entry: AnyRef): Boolean = index.get(entry) match {
      case Some(_) => false
      case None =>
        if (ep == entries.length) {
          val entries1 = new Array[AnyRef](ep * 2)
          System.arraycopy(entries, 0, entries1, 0, ep)
          entries = entries1
        }
        entries(ep) = entry
        index(entry) = ep
        ep = ep + 1
        true
    }

    private def deskolemizeTypeSymbols(ref: AnyRef): AnyRef = ref match {
      case sym: Symbol => deskolemize(sym)
      case _           => ref
    }

    /** If the symbol is a type skolem, deskolemize and log it.
     *  If we fail to deskolemize, in a method like
     *    trait Trait[+A] { def f[CC[X]] : CC[A] }
     *  the applied type CC[A] will hold a different CC symbol
     *  than the type-constructor type-parameter CC.
     */
    private def deskolemize(sym: Symbol): Symbol = {
      if (sym.isTypeSkolem) {
        val sym1 = sym.deSkolemize
        log({
          val what0 = sym.defString
          val what = sym1.defString match {
            case `what0` => what0
            case other   => what0 + "->" + other
          }
          val where = sym.enclMethod.fullLocationString
          s"deskolemizing $what in $where"
        })
        sym1
      }
      else sym
    }

    /** Store symbol in index. If symbol is local, also store everything it references.
     */
    def putSymbol(sym0: Symbol) {
      val sym = deskolemize(sym0)

      if (putEntry(sym)) {
        if (isLocalToPickle(sym)) {
          putEntry(sym.name)
          putSymbol(sym.owner)
          putSymbol(sym.privateWithin)
          putType(sym.info)
          if (sym.hasSelfType)
            putType(sym.typeOfThis)
          putSymbol(sym.alias)
          if (!sym.children.isEmpty) {
            val (locals, globals) = sym.children partition (_.isLocalClass)
            val children =
              if (locals.isEmpty) globals
              else {
                // The LOCAL_CHILD was introduced in 12a2b3b to fix Aladdin bug 1055. When a sealed
                // class/trait has local subclasses, a single <local child> class symbol is added
                // as pickled child (instead of a reference to the anonymous class; that was done
                // initially, but seems not to work, as the bug shows).
                // Adding the LOCAL_CHILD is necessary to retain exhaustivity warnings under separate
                // compilation. See test neg/aladdin1055.
                val parents = (if (sym.isTrait) List(definitions.ObjectTpe) else Nil) ::: List(sym.tpe)
                globals + sym.newClassWithInfo(tpnme.LOCAL_CHILD, parents, EmptyScope, pos = sym.pos)
              }

            putChildren(sym, children.toList sortBy (_.sealedSortName))
          }
          for (annot <- (sym.annotations filter (ann => ann.isStatic && !ann.isErroneous)).reverse)
            putAnnotation(sym, annot)
        }
        else if (sym != NoSymbol) {
          putEntry(if (sym.isModuleClass) sym.name.toTermName else sym.name)
          if (!sym.owner.isRoot) putSymbol(sym.owner)
        }
      }
    }

    private def putSymbols(syms: List[Symbol]) =
      syms foreach putSymbol

    /** Store type and everything it refers to in map index.
     */
    private def putType(tp: Type): Unit = if (putEntry(tp)) {
      tp match {
        case NoType | NoPrefix =>
          ;
        case ThisType(sym) =>
          putSymbol(sym)
        case SingleType(pre, sym) =>
          putType(pre)
          putSymbol(sym)
        case SuperType(thistpe, supertpe) =>
          putType(thistpe)
          putType(supertpe)
        case ConstantType(value) =>
          putConstant(value)
        case TypeRef(pre, sym, args) =>
          putType(pre)
          putSymbol(sym)
          putTypes(args)
        case TypeBounds(lo, hi) =>
          putType(lo)
          putType(hi)
        case tp: CompoundType =>
          putSymbol(tp.typeSymbol)
          putTypes(tp.parents)
          putSymbols(tp.decls.toList)
        case MethodType(params, restpe) =>
          putType(restpe)
          putSymbols(params)
        case NullaryMethodType(restpe) =>
          putType(restpe)
        case PolyType(tparams, restpe) =>
          putType(restpe)
          putSymbols(tparams)
        case ExistentialType(tparams, restpe) =>
          putType(restpe)
          putSymbols(tparams)
        case AnnotatedType(_, underlying) =>
          putType(underlying)
          tp.staticAnnotations foreach putAnnotation
        case _ =>
          throw new FatalError("bad type: " + tp + "(" + tp.getClass + ")")
      }
    }
    private def putTypes(tps: List[Type]) { tps foreach putType }

    private object putTreeTraverser extends Traverser {
      // Only used when pickling trees, i.e. in an argument of some Annotation
      // annotations in Modifiers are removed by the typechecker
      override def traverseModifiers(mods: Modifiers): Unit = if (putEntry(mods)) putEntry(mods.privateWithin)
      override def traverseName(name: Name): Unit           = putEntry(name)
      override def traverseConstant(const: Constant): Unit  = putEntry(const)
      override def traverse(tree: Tree): Unit               = putTree(tree)

      def put(tree: Tree): Unit = {
        if (tree.canHaveAttrs)
          putType(tree.tpe)
        if (tree.hasSymbolField)
          putSymbol(tree.symbol)

        super.traverse(tree)
      }
    }
    private def putTree(tree: Tree) {
      if (putEntry(tree))
        putTreeTraverser put tree
    }

    /** Store a constant in map index, along with anything it references.
     */
    private def putConstant(c: Constant) {
      if (putEntry(c)) {
        if (c.tag == StringTag) putEntry(newTermName(c.stringValue))
        else if (c.tag == ClazzTag) putType(c.typeValue)
        else if (c.tag == EnumTag) putSymbol(c.symbolValue)
      }
    }

    private def putChildren(sym: Symbol, children: List[Symbol]) {
      putEntry(sym -> children)
      children foreach putSymbol
    }

    /** used in putSymbol only, i.e. annotations on definitions, not on types */
    private def putAnnotation(sym: Symbol, annot: AnnotationInfo) {
      // if an annotation with the same arguments is applied to the
      // same symbol multiple times, it's only pickled once.
      if (putEntry(sym -> annot))
        putAnnotationBody(annot)
    }

    private def putAnnotation(annot: AnnotationInfo) {
      if (putEntry(annot))
        putAnnotationBody(annot)
    }

    /** Puts the members of an AnnotationInfo */
    private def putAnnotationBody(annot: AnnotationInfo) {
      def putAnnotArg(arg: Tree) {
        arg match {
          case Literal(c) => putConstant(c)
          case _ => putTree(arg)
        }
      }
      def putClassfileAnnotArg(carg: ClassfileAnnotArg) {
        (carg: @unchecked) match {
          case LiteralAnnotArg(const)  => putConstant(const)
          case ArrayAnnotArg(args)     => if (putEntry(carg)) args foreach putClassfileAnnotArg
          case NestedAnnotArg(annInfo) => putAnnotation(annInfo)
        }
      }
      val AnnotationInfo(tpe, args, assocs) = annot
      putType(tpe)
      args foreach putAnnotArg
      assocs foreach { asc =>
        putEntry(asc._1)
        putClassfileAnnotArg(asc._2)
      }
    }

    // Phase 2 methods: Write all entries to byte array ------------------------------

    /** Write a reference to object, i.e., the object's number in the map index.
     */
    private def writeRef(ref: AnyRef) {
      writeNat(index(deskolemizeTypeSymbols(ref)))
    }
    private def writeRefs(refs: List[AnyRef]): Unit = refs foreach writeRef

    private def writeRefsWithLength(refs: List[AnyRef]) {
      writeNat(refs.length)
      writeRefs(refs)
    }

    /** Write name, owner, flags, and info of a symbol.
     */
    private def writeSymInfo(sym: Symbol) {
      writeRef(sym.name)
      writeRef(localizedOwner(sym))
      writeLongNat((rawToPickledFlags(sym.rawflags & PickledFlags)))
      if (sym.hasAccessBoundary) writeRef(sym.privateWithin)
      writeRef(sym.info)
    }

    /** Write a name in UTF8 format. */
    private def writeName(name: Name) {
      ensureCapacity(name.length * 3)
      val utfBytes = Codec toUTF8 name.toString
      scala.compat.Platform.arraycopy(utfBytes, 0, bytes, writeIndex, utfBytes.length)
      writeIndex += utfBytes.length
    }

    /** Write an annotation */
    private def writeAnnotation(annot: AnnotationInfo) {
      def writeAnnotArg(arg: Tree) {
        arg match {
          case Literal(c) => writeRef(c)
          case _ => writeRef(arg)
        }
      }

      writeRef(annot.atp)
      annot.args foreach writeAnnotArg
      annot.assocs foreach { asc =>
        writeRef(asc._1)
        writeClassfileAnnotArg(asc._2)
      }
    }

    /** Write a ClassfileAnnotArg (argument to classfile annotation) */
    def writeClassfileAnnotArg(carg: ClassfileAnnotArg) {
      (carg: @unchecked) match {
        case LiteralAnnotArg(const)  => writeRef(const)
        case ArrayAnnotArg(args)     => writeRef(carg)
        case NestedAnnotArg(annInfo) => writeRef(annInfo)
      }
    }

    private object writeTreeBodyTraverser extends Traverser {
      private var refs = false
      @inline private def asRefs[T](body: => T): T = {
        val saved = refs
        refs = true
        try body finally refs = saved
      }
      override def traverseModifiers(mods: Modifiers): Unit          = if (refs) writeRef(mods) else super.traverseModifiers(mods)
      override def traverseName(name: Name): Unit                    = writeRef(name)
      override def traverseConstant(const: Constant): Unit           = writeRef(const)
      override def traverseParams(params: List[Tree]): Unit          = writeRefsWithLength(params)
      override def traverseParamss(vparamss: List[List[Tree]]): Unit = {
        writeNat(vparamss.length)
        super.traverseParamss(vparamss)
      }
      override def traverse(tree: Tree): Unit = {
        if (refs)
          writeRef(tree)
        else {
          writeRef(tree.tpe)
          if (tree.hasSymbolField)
            writeRef(tree.symbol)

          asRefs(super.traverse(tree))
        }
      }
    }

    /** Write an entry */
    private def writeEntry(entry: AnyRef) {
      def writeLocalSymbolBody(sym: Symbol) {
        writeSymInfo(sym)
        sym match {
          case _: ClassSymbol if sym.hasSelfType => writeRef(sym.typeOfThis)
          case _: TermSymbol if sym.alias.exists => writeRef(sym.alias)
          case _                                 =>
        }
      }
      def writeExtSymbolBody(sym: Symbol) {
        val name = if (sym.isModuleClass) sym.name.toTermName else sym.name
        writeRef(name)
        if (!sym.owner.isRoot)
          writeRef(sym.owner)
      }
      def writeSymbolBody(sym: Symbol) {
        if (sym ne NoSymbol) {
          if (isLocalToPickle(sym))
            writeLocalSymbolBody(sym)
          else
            writeExtSymbolBody(sym)
        }
      }

      // NullaryMethodType reuses POLYtpe since those can never have an empty list of tparams.
      // TODO: is there any way this can come back and bite us in the bottom?
      // ugliness and thrift aside, this should make this somewhat more backward compatible
      // (I'm not sure how old scalac's would deal with nested PolyTypes, as these used to be folded into one)
      def writeTypeBody(tpe: Type): Unit = tpe match {
        case NoType | NoPrefix                   =>
        case ThisType(sym)                       => writeRef(sym)
        case SingleType(pre, sym)                => writeRef(pre) ; writeRef(sym)
        case SuperType(thistpe, supertpe)        => writeRef(thistpe) ; writeRef(supertpe)
        case ConstantType(value)                 => writeRef(value)
        case TypeBounds(lo, hi)                  => writeRef(lo) ; writeRef(hi)
        case TypeRef(pre, sym, args)             => writeRef(pre) ; writeRef(sym); writeRefs(args)
        case MethodType(formals, restpe)         => writeRef(restpe) ; writeRefs(formals)
        case NullaryMethodType(restpe)           => writeRef(restpe); writeRefs(Nil)
        case PolyType(tparams, restpe)           => writeRef(restpe); writeRefs(tparams)
        case ExistentialType(tparams, restpe)    => writeRef(restpe); writeRefs(tparams)
        case StaticallyAnnotatedType(annots, tp) => writeRef(tp) ; writeRefs(annots)
        case AnnotatedType(_, tp)                => writeTypeBody(tp) // write the underlying type if there are no static annotations
        case CompoundType(parents, _, clazz)     => writeRef(clazz); writeRefs(parents)
      }

      def writeTreeBody(tree: Tree) {
        writeNat(picklerSubTag(tree))
        if (!tree.isEmpty)
          writeTreeBodyTraverser traverse tree
      }

      def writeConstant(c: Constant): Unit = c.tag match {
        case BooleanTag => writeLong(if (c.booleanValue) 1 else 0)
        case FloatTag   => writeLong(floatToIntBits(c.floatValue).toLong)
        case DoubleTag  => writeLong(doubleToLongBits(c.doubleValue))
        case StringTag  => writeRef(newTermName(c.stringValue))
        case ClazzTag   => writeRef(c.typeValue)
        case EnumTag    => writeRef(c.symbolValue)
        case tag        => if (ByteTag <= tag && tag <= LongTag) writeLong(c.longValue)
      }

      def writeModifiers(mods: Modifiers) {
        val pflags = rawToPickledFlags(mods.flags)
        writeNat((pflags >> 32).toInt)
        writeNat((pflags & 0xFFFFFFFF).toInt)
        writeRef(mods.privateWithin)
      }

      def writeSymbolTuple(target: Symbol, other: Any) {
        writeRef(target)
        other match {
          case annot: AnnotationInfo             => writeAnnotation(annot)
          case children: List[Symbol @unchecked] => writeRefs(children)
          case _                                 =>
        }
      }

      def writeBody(entry: AnyRef): Unit = entry match {
        case tree: Tree              => writeTreeBody(tree)
        case sym: Symbol             => writeSymbolBody(sym)
        case tpe: Type               => writeTypeBody(tpe)
        case name: Name              => writeName(name)
        case const: Constant         => writeConstant(const)
        case mods: Modifiers         => writeModifiers(mods)
        case annot: AnnotationInfo   => writeAnnotation(annot)
        case (target: Symbol, other) => writeSymbolTuple(target, other)
        case ArrayAnnotArg(args)     => args foreach writeClassfileAnnotArg
        case _                       => devWarning(s"Unexpected entry to pickler ${shortClassOfInstance(entry)} $entry")
      }

      // begin writeEntry
      // The picklerTag method can't determine if it's an external symbol reference
      val tag = entry match {
        case sym: Symbol if isExternalSymbol(sym) => if (sym.isModuleClass) EXTMODCLASSref else EXTref
        case _                                    => picklerTag(entry)
      }
      writeNat(tag)
      writeByte(0) // reserve a place to record the number of bytes written
      val start = writeIndex
      writeBody(entry)
      val length = writeIndex - start
      patchNat(start - 1, length) // patch bytes written over the placeholder
    }

    /** Write byte array */
    def writeArray() {
      assert(writeIndex == 0)
      writeNat(MajorVersion)
      writeNat(MinorVersion)
      writeNat(ep)

      entries take ep foreach writeEntry
    }

    override def toString = "" + rootName + " in " + rootOwner
  }
}
