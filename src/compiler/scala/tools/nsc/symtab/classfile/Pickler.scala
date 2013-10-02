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
      // If there are any erroneous types in the tree, then we will crash
      // when we pickle it: so let's report an error instead.  We know next
      // to nothing about what happened, but our supposition is a lot better
      // than "bad type: <error>" in terms of explanatory power.
      for (t <- unit.body) {
        if (t.isErroneous) {
          unit.error(t.pos, "erroneous or inaccessible type")
          return
        }

        if (!t.isDef && t.hasSymbolField && t.symbol.isTermMacro) {
          unit.error(t.pos, "macro has not been expanded")
          return
        }
      }

      pickle(unit.body)
    }
  }

  private class Pickle(root: Symbol) extends PickleBuffer(new Array[Byte](4096), -1, 0) {
    private val rootName  = root.name.toTermName
    private val rootOwner = root.owner
    private var entries   = new Array[AnyRef](256)
    private var ep        = 0
    private val index     = new LinkedHashMap[AnyRef, Int]
    private lazy val nonClassRoot = findOrElse(root.ownersIterator)(!_.isClass)(NoSymbol)

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
      if (isLocal(sym) && !isRootSym(sym) && !isLocal(sym.owner))
        // don't use a class as the localized owner for type parameters that are not owned by a class: those are not instantiated by asSeenFrom
        // however, they would suddenly be considered by asSeenFrom if their localized owner became a class (causing the crashes of #4079, #2741)
        (if ((sym.isTypeParameter || sym.isValueParameter) && !sym.owner.isClass) nonClassRoot
         else root)
      else sym.owner

    /** Is root in symbol.owner*, or should it be treated as a local symbol
     *  anyway? This is the case if symbol is a refinement class,
     *  an existentially bound variable, or a higher-order type parameter.
     */
    private def isLocal(sym: Symbol): Boolean =
      !sym.isPackageClass && sym != NoSymbol &&
      (isRootSym(sym) ||
       sym.isRefinementClass ||
       sym.isAbstractType && sym.hasFlag(EXISTENTIAL) || // existential param
       sym.isParameter ||
       isLocal(sym.owner))

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

    /** If the symbol is a type skolem, deskolemize and log it.
     *  If we fail to deskolemize, in a method like
     *    trait Trait[+A] { def f[CC[X]] : CC[A] }
     *  the applied type CC[A] will hold a different CC symbol
     *  than the type-constructor type-parameter CC.
     */
    private def deskolemize(sym: Symbol) = {
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
        if (isLocal(sym)) {
          putEntry(sym.name)
          putSymbol(sym.owner)
          putSymbol(sym.privateWithin)
          putType(sym.info)
          if (sym.thisSym.tpeHK != sym.tpeHK)
            putType(sym.typeOfThis)
          putSymbol(sym.alias)
          if (!sym.children.isEmpty) {
            val (locals, globals) = sym.children partition (_.isLocalClass)
            val children =
              if (locals.isEmpty) globals
              else globals + sym.newClassWithInfo(tpnme.LOCAL_CHILD, List(sym.tpe), EmptyScope, pos = sym.pos)

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
          putType(pre); putSymbol(sym)
        case SuperType(thistpe, supertpe) =>
          putType(thistpe)
          putType(supertpe)
        case ConstantType(value) =>
          putConstant(value)
        case TypeRef(pre, sym, args) =>
//          if (sym.isAbstractType && (sym hasFlag EXISTENTIAL))
//            if (!(boundSyms contains sym))
//              println("unbound existential: "+sym+sym.locationString)
          putType(pre); putSymbol(sym); putTypes(args)
        case TypeBounds(lo, hi) =>
          putType(lo); putType(hi)
        case RefinedType(parents, decls) =>
          val rclazz = tp.typeSymbol
          for (m <- decls.iterator)
            if (m.owner != rclazz) abort("bad refinement member "+m+" of "+tp+", owner = "+m.owner)
          putSymbol(rclazz); putTypes(parents); putSymbols(decls.toList)
        case ClassInfoType(parents, decls, clazz) =>
          putSymbol(clazz); putTypes(parents); putSymbols(decls.toList)
        case MethodType(params, restpe) =>
          putType(restpe); putSymbols(params)
        case NullaryMethodType(restpe) =>
          putType(restpe)
        case PolyType(tparams, restpe) =>
          /* no longer needed since all params are now local
          tparams foreach { tparam =>
            if (!isLocal(tparam)) locals += tparam // similar to existential types, these tparams are local
          }
          */
          putType(restpe); putSymbols(tparams)
        case ExistentialType(tparams, restpe) =>
//          val savedBoundSyms = boundSyms // boundSyms are known to be local based on the EXISTENTIAL flag  (see isLocal)
//          boundSyms = tparams ::: boundSyms
//          try {
            putType(restpe)
            //          } finally {
//            boundSyms = savedBoundSyms
//          }
          putSymbols(tparams)
        case AnnotatedType(annotations, underlying, selfsym) =>
          putType(underlying)
          if (settings.selfInAnnots) putSymbol(selfsym)
          putAnnotations(annotations filter (_.isStatic))
        case _ =>
          throw new FatalError("bad type: " + tp + "(" + tp.getClass + ")")
      }
    }
    private def putTypes(tps: List[Type]) { tps foreach putType }

    private def putTree(tree: Tree): Unit = if (putEntry(tree)) {
      if (tree != EmptyTree)
        putType(tree.tpe)
      if (tree.hasSymbolField)
        putSymbol(tree.symbol)

      tree match {
        case EmptyTree =>

        case tree@PackageDef(pid, stats) =>
          putTree(pid)
          putTrees(stats)

        case ClassDef(mods, name, tparams, impl) =>
          putMods(mods)
          putEntry(name)
          putTree(impl)
          putTrees(tparams)

        case ModuleDef(mods, name, impl) =>
          putMods(mods)
          putEntry(name)
          putTree(impl)

        case ValDef(mods, name, tpt, rhs) =>
          putMods(mods)
          putEntry(name)
          putTree(tpt)
          putTree(rhs)

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          putMods(mods)
          putEntry(name)
          putTrees(tparams)
          putTreess(vparamss)
          putTree(tpt)
          putTree(rhs)

        case TypeDef(mods, name, tparams, rhs) =>
          putMods(mods)
          putEntry(name)
          putTree(rhs)
          putTrees(tparams)

        case LabelDef(name, params, rhs) =>
          putEntry(name)
          putTree(rhs)
          putTrees(params)

        case Import(expr, selectors) =>
          putTree(expr)
          for (ImportSelector(from, _, to, _) <- selectors) {
            putEntry(from)
            putEntry(to)
          }
/*
        case DocDef(comment, definition) =>  should not be needed
          putConstant(Constant(comment))
          putTree(definition)
*/
        case Template(parents, self, body) =>
          putTrees(parents)
          putTree(self)
          putTrees(body)

        case Block(stats, expr) =>
          putTree(expr)
          putTrees(stats)

        case CaseDef(pat, guard, body) =>
          putTree(pat)
          putTree(guard)
          putTree(body)

        case Alternative(trees) =>
          putTrees(trees)

        case Star(elem) =>
          putTree(elem)

        case Bind(name, body) =>
          putEntry(name)
          putTree(body)

        case UnApply(fun: Tree, args) =>
          putTree(fun)
          putTrees(args)

        case ArrayValue(elemtpt, trees) =>
          putTree(elemtpt)
          putTrees(trees)


        case Function(vparams, body) =>
          putTree(body)
          putTrees(vparams)

        case Assign(lhs, rhs) =>
          putTree(lhs)
          putTree(rhs)

        case If(cond, thenp, elsep) =>
          putTree(cond)
          putTree(thenp)
          putTree(elsep)

        case Match(selector, cases) =>
          putTree(selector)
          putTrees(cases)

        case Return(expr) =>
          putTree(expr)

        case Try(block, catches, finalizer) =>
          putTree(block)
          putTree(finalizer)
          putTrees(catches)

        case Throw(expr) =>
          putTree(expr)

        case New(tpt) =>
          putTree(tpt)

        case Typed(expr, tpt) =>
          putTree(expr)
          putTree(tpt)

        case TypeApply(fun, args) =>
          putTree(fun)
          putTrees(args)

        case Apply(fun, args) =>
          putTree(fun)
          putTrees(args)

        case ApplyDynamic(qual, args) =>
          putTree(qual)
          putTrees(args)

        case Super(qual, mix) =>
          putTree(qual)
          putEntry(mix:Name)

        case This(qual) =>
          putEntry(qual)

        case Select(qualifier, selector) =>
          putTree(qualifier)
          putEntry(selector)

        case Ident(name) =>
          putEntry(name)

        case Literal(value) =>
          putEntry(value)

        case TypeTree() =>

        case Annotated(annot, arg) =>
          putTree(annot)
          putTree(arg)

        case SingletonTypeTree(ref) =>
          putTree(ref)

        case SelectFromTypeTree(qualifier, selector) =>
          putTree(qualifier)
          putEntry(selector)

        case CompoundTypeTree(templ: Template) =>
          putTree(templ)

        case AppliedTypeTree(tpt, args) =>
          putTree(tpt)
          putTrees(args)

        case TypeBoundsTree(lo, hi) =>
          putTree(lo)
          putTree(hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          putTree(tpt)
          putTrees(whereClauses)
      }
    }

    private def putTrees(trees: List[Tree]) = trees foreach putTree
    private def putTreess(treess: List[List[Tree]]) = treess foreach putTrees

    /** only used when pickling trees, i.e. in an
     *  argument of some Annotation */
    private def putMods(mods: Modifiers) = if (putEntry(mods)) {
      // annotations in Modifiers are removed by the typechecker
      val Modifiers(_, privateWithin, Nil) = mods
      putEntry(privateWithin)
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
      assert(putEntry((sym, children)))
      children foreach putSymbol
    }

    /** used in putSymbol only, i.e. annotations on definitions, not on types */
    private def putAnnotation(sym: Symbol, annot: AnnotationInfo) {
      // if an annotation with the same arguments is applied to the
      // same symbol multiple times, it's only pickled once.
      if (putEntry((sym, annot)))
        putAnnotationBody(annot)
    }

    /** used in AnnotatedType only, i.e. annotations on types */
    private def putAnnotations(annots: List[AnnotationInfo]) {
      annots foreach putAnnotation
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
    private def writeRef(ref0: AnyRef) {
      val ref = ref0 match {
        case sym: Symbol => deskolemize(sym)
        case _           => ref0
      }
      writeNat(index(ref))
    }
    private def writeRefs(refs: List[AnyRef]) { refs foreach writeRef }
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

    /** Write an entry */
    private def writeEntry(entry: AnyRef) {
      def writeBody(entry: AnyRef): Int = entry match {
        case name: Name =>
          writeName(name)
          if (name.isTermName) TERMname else TYPEname
        case NoSymbol =>
          NONEsym
        case sym: Symbol if !isLocal(sym) =>
          val tag =
            if (sym.isModuleClass) {
              writeRef(sym.name.toTermName); EXTMODCLASSref
            } else {
              writeRef(sym.name); EXTref
            }
          if (!sym.owner.isRoot) writeRef(sym.owner)
          tag
        case sym: ClassSymbol =>
          writeSymInfo(sym)
          if (sym.thisSym.tpe_* != sym.tpe_*) writeRef(sym.typeOfThis)
          CLASSsym
        case sym: TypeSymbol =>
          writeSymInfo(sym)
          if (sym.isAbstractType) TYPEsym else ALIASsym
        case sym: TermSymbol =>
          writeSymInfo(sym)
          if (sym.alias != NoSymbol) writeRef(sym.alias)
          if (sym.isModule) MODULEsym else VALsym
        case NoType =>
          NOtpe
        case NoPrefix =>
          NOPREFIXtpe
        case ThisType(sym) =>
          writeRef(sym); THIStpe
        case SingleType(pre, sym) =>
          writeRef(pre); writeRef(sym); SINGLEtpe
        case SuperType(thistpe, supertpe) =>
          writeRef(thistpe); writeRef(supertpe); SUPERtpe
        case ConstantType(value) =>
          writeRef(value); CONSTANTtpe
        case TypeRef(pre, sym, args) =>
          writeRef(pre); writeRef(sym); writeRefs(args); TYPEREFtpe
        case TypeBounds(lo, hi) =>
          writeRef(lo); writeRef(hi); TYPEBOUNDStpe
        case tp @ RefinedType(parents, decls) =>
          writeRef(tp.typeSymbol); writeRefs(parents); REFINEDtpe
        case ClassInfoType(parents, decls, clazz) =>
          writeRef(clazz); writeRefs(parents); CLASSINFOtpe
        case mt @ MethodType(formals, restpe) =>
          writeRef(restpe); writeRefs(formals) ; METHODtpe
        case mt @ NullaryMethodType(restpe) =>
          // reuse POLYtpe since those can never have an empty list of tparams.
          // TODO: is there any way this can come back and bite us in the bottom?
          // ugliness and thrift aside, this should make this somewhat more backward compatible
          // (I'm not sure how old scalac's would deal with nested PolyTypes, as these used to be folded into one)
          writeRef(restpe); writeRefs(Nil); POLYtpe
        case PolyType(tparams, restpe) => // invar: tparams nonEmpty
          writeRef(restpe); writeRefs(tparams); POLYtpe
        case ExistentialType(tparams, restpe) =>
          writeRef(restpe); writeRefs(tparams); EXISTENTIALtpe
        case c @ Constant(_) =>
          if (c.tag == BooleanTag) writeLong(if (c.booleanValue) 1 else 0)
          else if (ByteTag <= c.tag && c.tag <= LongTag) writeLong(c.longValue)
          else if (c.tag == FloatTag) writeLong(floatToIntBits(c.floatValue).toLong)
          else if (c.tag == DoubleTag) writeLong(doubleToLongBits(c.doubleValue))
          else if (c.tag == StringTag) writeRef(newTermName(c.stringValue))
          else if (c.tag == ClazzTag) writeRef(c.typeValue)
          else if (c.tag == EnumTag) writeRef(c.symbolValue)
          LITERAL + c.tag // also treats UnitTag, NullTag; no value required
        case AnnotatedType(annotations, tp, selfsym) =>
          annotations filter (_.isStatic) match {
            case Nil          => writeBody(tp) // write the underlying type if there are no annotations
            case staticAnnots =>
              if (settings.selfInAnnots && selfsym != NoSymbol)
                writeRef(selfsym)
              writeRef(tp)
              writeRefs(staticAnnots)
              ANNOTATEDtpe
          }
        // annotations attached to a symbol (i.e. annots on terms)
        case (target: Symbol, annot@AnnotationInfo(_, _, _)) =>
          writeRef(target)
          writeAnnotation(annot)
          SYMANNOT

        case ArrayAnnotArg(args) =>
          args foreach writeClassfileAnnotArg
          ANNOTARGARRAY

        case (target: Symbol, children: List[_]) =>
          writeRef(target)
          writeRefs(children.asInstanceOf[List[Symbol]])
          CHILDREN

        case EmptyTree =>
          writeNat(EMPTYtree)
          TREE

        case tree@PackageDef(pid, stats) =>
          writeNat(PACKAGEtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(tree.mods)
          writeRef(pid)
          writeRefs(stats)
          TREE

        case tree@ClassDef(mods, name, tparams, impl) =>
          writeNat(CLASStree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(mods)
          writeRef(name)
          writeRef(impl)
          writeRefs(tparams)
          TREE

        case tree@ModuleDef(mods, name, impl) =>
          writeNat(MODULEtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(mods)
          writeRef(name)
          writeRef(impl)
          TREE

        case tree@ValDef(mods, name, tpt, rhs) =>
          writeNat(VALDEFtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(mods)
          writeRef(name)
          writeRef(tpt)
          writeRef(rhs)
          TREE

        case tree@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          writeNat(DEFDEFtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(mods)
          writeRef(name)
          writeRefsWithLength(tparams)
          writeNat(vparamss.length)
          vparamss foreach writeRefsWithLength
          writeRef(tpt)
          writeRef(rhs)
          TREE

        case tree@TypeDef(mods, name, tparams, rhs) =>
          writeNat(TYPEDEFtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(mods)
          writeRef(name)
          writeRef(rhs)
          writeRefs(tparams)
          TREE

        case tree@LabelDef(name, params, rhs) =>
          writeNat(LABELtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(name)
          writeRef(rhs)
          writeRefs(params)
          TREE

        case tree@Import(expr, selectors) =>
          writeNat(IMPORTtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(expr)
          for (ImportSelector(from, _, to, _) <- selectors) {
            writeRef(from)
            writeRef(to)
          }
          TREE

        case tree@DocDef(comment, definition) =>
          writeNat(DOCDEFtree)
          writeRef(tree.tpe)
          writeRef(Constant(comment))
          writeRef(definition)
          TREE

        case tree@Template(parents, self, body) =>
          writeNat(TEMPLATEtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRefsWithLength(parents)
          writeRef(self)
          writeRefs(body)
          TREE

        case tree@Block(stats, expr) =>
          writeNat(BLOCKtree)
          writeRef(tree.tpe)
          writeRef(expr)
          writeRefs(stats)
          TREE

        case tree@CaseDef(pat, guard, body) =>
          writeNat(CASEtree)
          writeRef(tree.tpe)
          writeRef(pat)
          writeRef(guard)
          writeRef(body)
          TREE

        case tree@Alternative(trees) =>
          writeNat(ALTERNATIVEtree)
          writeRef(tree.tpe)
          writeRefs(trees)
          TREE

        case tree@Star(elem) =>
          writeNat(STARtree)
          writeRef(tree.tpe)
          writeRef(elem)
          TREE

        case tree@Bind(name, body) =>
          writeNat(BINDtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(name)
          writeRef(body)
          TREE

        case tree@UnApply(fun: Tree, args) =>
          writeNat(UNAPPLYtree)
          writeRef(tree.tpe)
          writeRef(fun)
          writeRefs(args)
          TREE

        case tree@ArrayValue(elemtpt, trees) =>
          writeNat(ARRAYVALUEtree)
          writeRef(tree.tpe)
          writeRef(elemtpt)
          writeRefs(trees)
          TREE

        case tree@Function(vparams, body) =>
          writeNat(FUNCTIONtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(body)
          writeRefs(vparams)
          TREE

        case tree@Assign(lhs, rhs) =>
          writeNat(ASSIGNtree)
          writeRef(tree.tpe)
          writeRef(lhs)
          writeRef(rhs)
          TREE

        case tree@If(cond, thenp, elsep) =>
          writeNat(IFtree)
          writeRef(tree.tpe)
          writeRef(cond)
          writeRef(thenp)
          writeRef(elsep)
          TREE

        case tree@Match(selector, cases) =>
          writeNat(MATCHtree)
          writeRef(tree.tpe)
          writeRef(selector)
          writeRefs(cases)
          TREE

        case tree@Return(expr) =>
          writeNat(RETURNtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(expr)
          TREE

        case tree@Try(block, catches, finalizer) =>
          writeNat(TREtree)
          writeRef(tree.tpe)
          writeRef(block)
          writeRef(finalizer)
          writeRefs(catches)
          TREE

        case tree@Throw(expr) =>
          writeNat(THROWtree)
          writeRef(tree.tpe)
          writeRef(expr)
          TREE

        case tree@New(tpt) =>
          writeNat(NEWtree)
          writeRef(tree.tpe)
          writeRef(tpt)
          TREE

        case tree@Typed(expr, tpt) =>
          writeNat(TYPEDtree)
          writeRef(tree.tpe)
          writeRef(expr)
          writeRef(tpt)
          TREE

        case tree@TypeApply(fun, args) =>
          writeNat(TYPEAPPLYtree)
          writeRef(tree.tpe)
          writeRef(fun)
          writeRefs(args)
          TREE

        case tree@Apply(fun, args) =>
          writeNat(APPLYtree)
          writeRef(tree.tpe)
          writeRef(fun)
          writeRefs(args)
          TREE

        case tree@ApplyDynamic(qual, args) =>
          writeNat(APPLYDYNAMICtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(qual)
          writeRefs(args)
          TREE

        case tree@Super(qual, mix) =>
          writeNat(SUPERtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(qual)
          writeRef(mix)
          TREE

        case tree@This(qual) =>
          writeNat(THIStree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(qual)
          TREE

        case tree@Select(qualifier, selector) =>
          writeNat(SELECTtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(qualifier)
          writeRef(selector)
          TREE

        case tree@Ident(name) =>
          writeNat(IDENTtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(name)
          TREE

        case tree@Literal(value) =>
          writeNat(LITERALtree)
          writeRef(tree.tpe)
          writeRef(value)
          TREE

        case tree@TypeTree() =>
          writeNat(TYPEtree)
          writeRef(tree.tpe)
          TREE

        case tree@Annotated(annot, arg) =>
          writeNat(ANNOTATEDtree)
          writeRef(tree.tpe)
          writeRef(annot)
          writeRef(arg)
          TREE

        case tree@SingletonTypeTree(ref) =>
          writeNat(SINGLETONTYPEtree)
          writeRef(tree.tpe)
          writeRef(ref)
          TREE

        case tree@SelectFromTypeTree(qualifier, selector) =>
          writeNat(SELECTFROMTYPEtree)
          writeRef(tree.tpe)
          writeRef(qualifier)
          writeRef(selector)
          TREE

        case tree@CompoundTypeTree(templ: Template) =>
          writeNat(COMPOUNDTYPEtree)
          writeRef(tree.tpe)
          writeRef(templ)
          TREE

        case tree@AppliedTypeTree(tpt, args) =>
          writeNat(APPLIEDTYPEtree)
          writeRef(tree.tpe)
          writeRef(tpt)
          writeRefs(args)
          TREE

        case tree@TypeBoundsTree(lo, hi) =>
          writeNat(TYPEBOUNDStree)
          writeRef(tree.tpe)
          writeRef(lo)
          writeRef(hi)
          TREE

        case tree@ExistentialTypeTree(tpt, whereClauses) =>
          writeNat(EXISTENTIALTYPEtree)
          writeRef(tree.tpe)
          writeRef(tpt)
          writeRefs(whereClauses)
          TREE

        case Modifiers(flags, privateWithin, _) =>
          val pflags = rawToPickledFlags(flags)
          writeNat((pflags >> 32).toInt)
          writeNat((pflags & 0xFFFFFFFF).toInt)
          writeRef(privateWithin)
          MODIFIERS

        // annotations on types (not linked to a symbol)
        case annot@AnnotationInfo(_, _, _) =>
          writeAnnotation(annot)
          ANNOTINFO

        case _ =>
          throw new FatalError("bad entry: " + entry + " " + entry.getClass)
      }

      // begin writeEntry
      val startpos = writeIndex
      // reserve some space so that the patchNat's most likely won't need to shift
      writeByte(0); writeByte(0)
      patchNat(startpos, writeBody(entry))
      patchNat(startpos + 1, writeIndex - (startpos + 2))
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
