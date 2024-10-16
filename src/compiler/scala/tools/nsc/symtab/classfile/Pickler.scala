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

package scala.tools.nsc
package symtab
package classfile

import java.lang.Float.floatToIntBits
import java.lang.Double.doubleToLongBits
import java.util.Arrays.fill

import scala.io.Codec
import scala.reflect.internal.pickling.{PickleBuffer, PickleFormat}
import scala.reflect.internal.util.shortClassOfInstance
import scala.collection.mutable
import PickleFormat._
import Flags._
import scala.annotation.{nowarn, tailrec}

/**
 * Serialize a top-level module and/or class.
 *
 * @see [[scala.reflect.internal.pickling.PickleFormat PickleFormat]] for symbol table attribute format.
 *
 * @author Martin Odersky
 */
abstract class Pickler extends SubComponent {
  import global._

  val phaseName = "pickler"

  def newPhase(prev: Phase): StdPhase = new PicklePhase(prev)

  final def pickle(sym: Symbol, companion: Symbol, noPrivates: Boolean): Pickle = {
    initPickle(sym, noPrivates) { pickle =>
      def reserveDeclEntries(sym: Symbol): Unit = {
        if (pickle.reserveEntry(sym)) {
          if (sym.isClass) sym.info.decls.foreach(reserveDeclEntries)
          else if (sym.isModule) reserveDeclEntries(sym.moduleClass)
        }
      }

      val syms = sym :: (if (companion != NoSymbol) companion :: Nil else Nil)
      syms.foreach(reserveDeclEntries)
      syms.foreach { sym =>
        pickle.putDecl(sym)
      }
      pickle.writeArray()
    }
  }

  class PicklePhase(prev: Phase) extends StdPhase(prev) {
    import global.genBCode.postProcessor.classfileWriters.FileWriter
    private lazy val sigWriter: Option[FileWriter] =
      if (settings.YpickleWrite.isSetByUser && !settings.YpickleWrite.value.isEmpty) {
        val file = settings.pathFactory.getFile(settings.YpickleWrite.value) // might be a JAR (possibly still to be created) or a directory
        Some(FileWriter(global, file, None))
      }
      else
        None

    @nowarn("cat=lint-nonlocal-return")
    def apply(unit: CompilationUnit): Unit = {
      def pickle(tree: Tree): Unit = {
        tree match {
          case PackageDef(_, stats) =>
            stats foreach pickle
          case ClassDef(_, _, _, _) | ModuleDef(_, _, _) =>
            val sym = tree.symbol
            def shouldPickle(sym: Symbol) = currentRun.compiles(sym) && !currentRun.symData.contains(sym)
            if (shouldPickle(sym)) {
              def pickle(noPrivates: Boolean, writeToSymData: Boolean, writeToSigFile: Boolean): Unit = {
                val companion = sym.companionSymbol.filter(_.owner == sym.owner).filter(shouldPickle) // exclude companionship between package- and package object-owned symbols.
                val pickle = Pickler.this.pickle(sym, companion, noPrivates)
                if (writeToSymData) {
                  currentRun.symData(sym) = pickle
                  companion.andAlso(sym => currentRun.symData(sym) = pickle)
                  currentRun registerPickle sym
                }
                if (writeToSigFile)
                  writeSigFile(sym, pickle)
              }
              if (sigWriter.isDefined && settings.YpickleWriteApiOnly.value) {
                pickle(noPrivates = false, writeToSymData = true, writeToSigFile = false)
                pickle(noPrivates = true, writeToSymData = false, writeToSigFile = true)
              } else {
                pickle(noPrivates = false, writeToSymData = true, writeToSigFile = true)
              }
            }
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
              if (settings.isDebug) e.printStackTrace()
              reporter.error(t.pos, "erroneous or inaccessible type")
              return
            }
          }
          throw e
      }
    }

    override def run(): Unit = {
      try super.run()
      finally {
        closeSigWriter()
        _index = null
        _entries = null
      }
    }

    private def writeSigFile(sym: Symbol, pickle: PickleBuffer): Unit = {
      sigWriter.foreach { writer =>
        val binaryName = sym.javaBinaryNameString
        val binaryClassName = if (sym.isModule) binaryName.stripSuffix(nme.MODULE_SUFFIX_STRING) else binaryName
        val relativePath = binaryClassName + ".sig"
        val data = pickle.bytes.take(pickle.writeIndex)
        writer.writeFile(relativePath, data)
      }
    }
    private def closeSigWriter(): Unit = {
      sigWriter.foreach { writer =>
        writer.close()
        if (settings.verbose.value)
          reporter.echo(NoPosition, "[sig files written]")
      }
    }

    override protected def shouldSkipThisPhaseForJava: Boolean = !settings.YpickleJava.value
  }

  type Index   = mutable.HashMap[AnyRef, Int] // a map from objects (symbols, types, names, ...) to indices into Entries
  type Entries = Array[AnyRef]

  final val InitEntriesSize = 256
  private[this] var _index: Index = _
  private[this] var _entries: Entries = _

  final def initPickle(root: Symbol, noPrivates: Boolean)(f: Pickle => Unit): Pickle = {
    if (_index eq null)   { _index   = new Index(InitEntriesSize, mutable.HashMap.defaultLoadFactor) }
    if (_entries eq null) { _entries = new Entries(InitEntriesSize) }
    val pickle = new Pickle(root, _index, _entries, noPrivates)
    try f(pickle) finally { pickle.close(); _index.clear(); fill(_entries, null) }
    pickle
  }

  class Pickle private[Pickler](root: Symbol, private var index: Index, private var entries: Entries, noPrivates: Boolean)
      extends PickleBuffer(new Array[Byte](4096), -1, 0) {
    private val rootName  = root.name.toTermName
    private val rootOwner = root.owner
    private var ep        = 0
    private lazy val nonClassRoot = findSymbol(root.ownersIterator)(!_.isClass)
    def include(sym: Symbol) = !noPrivates || !sym.isPrivate || (sym.owner.isTrait && sym.isAccessor)

    def close(): Unit = { index = null; entries = null }

    private def isRootSym(sym: Symbol) =
      sym.name.toTermName == rootName && sym.owner == rootOwner

    /** Usually `sym.owner`, except when `sym` is pickle-local, while `sym.owner` is not.
      *
      * In the latter case, the alternative owner is the pickle root,
      * or a non-class owner of root (so that term-owned parameters remain term-owned).
      *
      * Note: tree pickling also finds its way here; e.g. in scala/bug#7501 the pickling
      * of trees in annotation arguments considers the parameter symbol of a method
      * called in such a tree as "local". The condition `sym.isValueParameter` was
      * added to fix that bug, but there may be a better way.
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
    @tailrec
    private def isLocalToPickle(sym: Symbol): Boolean = (sym != NoSymbol) && !sym.isPackageClass && (
         isRootSym(sym)
      || sym.isRefinementClass
      || sym.isAbstractType && sym.hasFlag(EXISTENTIAL) // existential param
      || sym.isParameter
      || isLocalToPickle(sym.owner)
    )
    private def isExternalSymbol(sym: Symbol): Boolean = (sym != NoSymbol) && !isLocalToPickle(sym)

    // Phase 1 methods: Populate entries/index ------------------------------------
    private val reserved = mutable.BitSet()
    final def reserveEntry(sym: Symbol): Boolean = {
      if (include(sym)) {
        reserved(ep) = true
        putEntry(sym)
        true
      } else false
    }

    /** Store entry e in index at next available position unless
     *  it is already there.
     *
     *  @return      true iff entry is new.
     */
    private def putEntry(entry: AnyRef): Boolean = {
      assert(index ne null, this)
      index.get(entry) match {
        case Some(i) =>
          reserved.remove(i)
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

    def putDecl(sym: Symbol): Unit = if (include(sym)) putSymbol(sym)

    /** Store symbol in index. If symbol is local, also store everything it references.
     */
    def putSymbol(sym0: Symbol): Unit = {
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
                val parents = if (sym.isTrait) List(definitions.ObjectTpe, sym.tpe) else List(sym.tpe)
                globals + sym.newClassWithInfo(tpnme.LOCAL_CHILD, parents, EmptyScope, pos = sym.pos)
              }

            putChildren(sym, children.toList sortBy (_.sealedSortName))
          }
          for (annot <- sym.annotations.filter(ann => ann.isStatic && !ann.isErroneous))
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
          tp.decls.toList.foreach(putDecl)
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
    private def putTypes(tps: List[Type]): Unit = { tps foreach putType }

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
    private def putTree(tree: Tree): Unit = {
      if (putEntry(tree))
        putTreeTraverser put tree
    }

    /** Store a constant in map index, along with anything it references.
     */
    private def putConstant(c: Constant): Unit = {
      if (putEntry(c)) {
        if (c.tag == StringTag) putEntry(newTermName(c.stringValue))
        else if (c.tag == ClazzTag) putType(c.typeValue)
        else if (c.tag == EnumTag) putSymbol(c.symbolValue)
      }
    }

    private def putChildren(sym: Symbol, children: List[Symbol]): Unit = {
      putEntry(sym -> children)
      children foreach putSymbol
    }

    /** used in putSymbol only, i.e. annotations on definitions, not on types */
    private def putAnnotation(sym: Symbol, annot: AnnotationInfo): Unit = {
      // if an annotation with the same arguments is applied to the
      // same symbol multiple times, it's only pickled once.
      if (putEntry(sym -> annot))
        putAnnotationBody(annot)
    }

    private def putAnnotation(annot: AnnotationInfo): Unit = {
      if (putEntry(annot))
        putAnnotationBody(annot)
    }

    /** Puts the members of an AnnotationInfo */
    private def putAnnotationBody(annot: AnnotationInfo): Unit = {
      def putAnnotArg(arg: Tree): Unit = {
        arg match {
          case Literal(c) => putConstant(c)
          case _ => putTree(arg)
        }
      }
      def putClassfileAnnotArg(carg: ClassfileAnnotArg): Unit = {
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
    private def writeRef(ref: AnyRef): Unit = {
      assert(index ne null, this)
      writeNat(index(deskolemizeTypeSymbols(ref)))
    }
    private def writeRefs(refs: List[AnyRef]): Unit = refs foreach writeRef

    private def writeRefsWithLength(refs: List[AnyRef]): Unit = {
      writeNat(refs.length)
      writeRefs(refs)
    }

    /** Write name, owner, flags, and info of a symbol.
     */
    private def writeSymInfo(sym: Symbol): Unit = {
      writeRef(sym.name)
      writeRef(localizedOwner(sym))
      writeLongNat((rawToPickledFlags(sym.rawflags & PickledFlags)))
      if (sym.hasAccessBoundary) writeRef(sym.privateWithin)
      writeRef(sym.info)
    }

    /** Write a name in UTF8 format. */
    private def writeName(name: Name): Unit = {
      ensureCapacity(name.length * 3)
      val utfBytes = Codec toUTF8 name.toString
      System.arraycopy(utfBytes, 0, bytes, writeIndex, utfBytes.length)
      writeIndex += utfBytes.length
    }

    /** Write an annotation */
    private def writeAnnotation(annot: AnnotationInfo): Unit = {
      def writeAnnotArg(arg: Tree): Unit = {
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
    def writeClassfileAnnotArg(carg: ClassfileAnnotArg): Unit = {
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
    private def writeEntry(entry: AnyRef): Unit = {
      def writeLocalSymbolBody(sym: Symbol): Unit = {
        writeSymInfo(sym)
        sym match {
          case _: ClassSymbol if sym.hasSelfType => writeRef(sym.typeOfThis)
          case _: TermSymbol if sym.alias.exists => writeRef(sym.alias)
          case _                                 =>
        }
      }
      def writeExtSymbolBody(sym: Symbol): Unit = {
        val name = if (sym.isModuleClass) sym.name.toTermName else sym.name
        writeRef(name)
        if (!sym.owner.isRoot)
          writeRef(sym.owner)
      }
      def writeSymbolBody(sym: Symbol): Unit = {
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
        case x                                   => throw new MatchError(x)
      }

      def writeTreeBody(tree: Tree): Unit = {
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
        case ctag       => if (ByteTag <= ctag && ctag <= LongTag) writeLong(c.longValue)
      }

      def writeModifiers(mods: Modifiers): Unit = {
        val pflags = rawToPickledFlags(mods.flags)
        writeNat((pflags >> 32).toInt)
        writeNat((pflags & 0xFFFFFFFF).toInt)
        writeRef(mods.privateWithin)
      }

      def writeSymbolTuple(target: Symbol, other: Any): Unit = {
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
    final def writeArray(): Unit = {
      assert(writeIndex == 0, "Index must be zero")
      assert(index ne null, this)
      writeNat(MajorVersion)
      writeNat(MinorVersion)
      writeNat(ep)

      entries take ep foreach writeEntry
    }

    override def toString = "" + rootName + " in " + rootOwner
  }
}
