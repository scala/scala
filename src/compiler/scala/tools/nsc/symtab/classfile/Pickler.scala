/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import java.lang.{Float, Double}
import scala.tools.nsc.util.{Position, NoPosition, ShowPickled}
import Flags._
import PickleFormat._

/**
 * Serialize a top-level module and/or class.
 *
 * @see <code>EntryTags.scala</code> for symbol table attribute format.
 *
 * @author Martin Odersky
 * @version 1.0
 */
abstract class Pickler extends SubComponent {
  import global._

  private final val showSig = false

  val phaseName = "pickler"

  def newPhase(prev: Phase): StdPhase = new PicklePhase(prev)

  class PicklePhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      def pickle(tree: Tree) {

        def add(sym: Symbol, pickle: Pickle) = {
          if (currentRun.compiles(sym) && !currentRun.symData.contains(sym)) {
            if (settings.debug.value) log("pickling " + sym)
            pickle.putSymbol(sym)
            currentRun.symData(sym) = pickle
          }
        }

        tree match {
          case PackageDef(_, stats) =>
            stats foreach pickle
          case ClassDef(_, _, _, _) | ModuleDef(_, _, _) =>
            val sym = tree.symbol
            val pickle = new Pickle(sym, sym.name.toTermName, sym.owner)
            add(sym, pickle)
            add(sym.linkedSym, pickle)
            pickle.finish
            // pickleHash is used to track changes in a signature (-> IDE)
            val doPickleHash = global.doPickleHash
            if (doPickleHash) {
              var i = 0
              while (i < pickle.writeIndex) {
                unit.pickleHash += pickle.bytes(i).toLong // toLong needed to work around bug
                i += 1
              }
            }
          case _ =>
        }
      }
      pickle(unit.body)
    }
  }

  private class Pickle(root: Symbol, rootName: Name, rootOwner: Symbol)
        extends PickleBuffer(new Array[Byte](4096), -1, 0) {
    import scala.collection.mutable.LinkedHashMap
    private var entries = new Array[AnyRef](256)
    private var ep = 0
    private val index = new LinkedHashMap[AnyRef, Int]

    /** Is symbol an existentially bound variable with a package as owner?
     *  Such symbols should be treated as if they were local.
     */
    private def isUnrootedExistential(sym: Symbol) =
      sym.isAbstractType && sym.hasFlag(EXISTENTIAL)

    private def normalizedOwner(sym: Symbol) =
      if (isUnrootedExistential(sym)) root else sym.owner

    /** Is root in symbol.owner*?
     *
     *  @param sym ...
     *  @return    ...
     */
    private def isLocal(sym: Symbol): Boolean =
      sym.isRefinementClass ||
      sym.name.toTermName == rootName && sym.owner == rootOwner ||
      sym != NoSymbol && isLocal(sym.owner) ||
      isUnrootedExistential(sym)

    private def staticAnnotations(annots: List[AnnotationInfo]) =
      annots filter(ann =>
        ann.atp.typeSymbol isNonBottomSubClass definitions.StaticAnnotationClass)

    // Phase 1 methods: Populate entries/index ------------------------------------

    /** Store entry <code>e</code> in index at next available position unless
     *  it is already there.
     *
     *  @param entry ...
     *  @return      <code>true</code> iff entry is new.
     */
    private def putEntry(entry: AnyRef): Boolean = index.get(entry) match {
      case Some(_) => false
      case None =>
        if (ep == entries.length) {
          val entries1 = new Array[AnyRef](ep * 2)
          Array.copy(entries, 0, entries1, 0, ep)
          entries = entries1
        }
        entries(ep) = entry
        index(entry) = ep
        ep = ep + 1
        true
    }

    /** Store symbol in <code>index</code>. If symbol is local, also store
     * everything it refers to.
     *
     *  @param sym ...
     */
    def putSymbol(sym: Symbol) {
      if (putEntry(sym)) {
        if (isLocal(sym)) {
          putEntry(sym.name)
          putSymbol(sym.owner)
          putSymbol(sym.privateWithin)
          putType(sym.info)
          if (sym.thisSym.tpeHK != sym.tpeHK)
            putType(sym.typeOfThis);
          putSymbol(sym.alias)
          putSymbol(sym.defaultGetter)
          if (!sym.children.isEmpty) {
            val (locals, globals) = sym.children.toList.partition(_.isLocalClass)
            val children =
              if (locals.isEmpty) globals
              else {
                val localChildDummy = sym.newClass(sym.pos, nme.LOCALCHILD)
                localChildDummy.setInfo(ClassInfoType(List(sym.tpe), EmptyScope, localChildDummy))
                localChildDummy :: globals
              }
            putChildren(sym, children.sort((x, y) => x isLess y))
          }
          for (annot <- staticAnnotations(sym.annotations.reverse))
            putAnnotation(sym, annot)
        } else if (sym != NoSymbol) {
          putEntry(if (sym.isModuleClass) sym.name.toTermName else sym.name)
          if (!sym.owner.isRoot) putSymbol(sym.owner)
        }
      }
    }

    private def putSymbols(syms: List[Symbol]) =
      syms foreach putSymbol

    /** Store type and everythig it refers to in map <code>index</code>.
     *
     *  @param tp ...
     */
    private def putType(tp: Type): Unit = if (putEntry(tp)) {
      tp match {
        case NoType | NoPrefix | DeBruijnIndex(_, _) =>
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
          putType(pre); putSymbol(sym); putTypes(args)
        case TypeBounds(lo, hi) =>
          putType(lo); putType(hi)
        case RefinedType(parents, decls) =>
          val rclazz = tp.typeSymbol
          for (m <- decls.iterator)
            if (m.owner != rclazz) assert(false, "bad refinement member "+m+" of "+tp+", owner = "+m.owner)
          putSymbol(rclazz); putTypes(parents); putSymbols(decls.toList)
        case ClassInfoType(parents, decls, clazz) =>
          putSymbol(clazz); putTypes(parents); putSymbols(decls.toList)
        case MethodType(params, restpe) =>
          putType(restpe); putSymbols(params)
        case PolyType(tparams, restpe) =>
          putType(restpe); putSymbols(tparams)
        case ExistentialType(tparams, restpe) =>
          putType(restpe); putSymbols(tparams)
        case AnnotatedType(annotations, underlying, selfsym) =>
          putType(underlying)
          if (settings.selfInAnnots.value) putSymbol(selfsym)
          putAnnotations(staticAnnotations(annotations))
        case _ =>
          throw new FatalError("bad type: " + tp + "(" + tp.getClass + ")")
      }
    }
    private def putTypes(tps: List[Type]) { tps foreach putType }

    private def putTree(tree: Tree): Unit = if (putEntry(tree)) {
      if (tree != EmptyTree)
        putType(tree.tpe)
      if (tree.hasSymbol)
        putSymbol(tree.symbol)

      tree match {
        case EmptyTree =>

        case tree@PackageDef(name, stats) =>
          putEntry(name)
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
          for ((from,to) <- selectors) {
            putEntry(from)
            putEntry(to)
          }

        case DocDef(comment, definition) =>
          putConstant(Constant(comment))
          putTree(definition)

        case Template(parents, self, body) =>
          writeNat(parents.length)
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

        case Sequence(trees) =>
          putTrees(trees)

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
          putEntry(qual:Name)
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

    private def putTrees(trees: List[Tree]) =
      trees.foreach(putTree _)

    private def putTreess(treess: List[List[Tree]]) =
      treess.foreach(putTrees _)

    /** only used when pickling trees, i.e. in an
     *  argument of some Annotation */
    private def putMods(mods: Modifiers) = if (putEntry(mods)) {
      // annotations in Modifiers are removed by the typechecker
      val Modifiers(flags, privateWithin, Nil) = mods
      putEntry(privateWithin)
    }

    /** Store a constant in map <code>index</code> along with
     *  anything it references.
     */
    private def putConstant(c: Constant) {
      if (putEntry(c)) {
        if (c.tag == StringTag) putEntry(newTermName(c.stringValue))
        else if (c.tag == ClassTag) putType(c.typeValue)
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
        carg match {
          case LiteralAnnotArg(const) =>
            putConstant(const)
          case ArrayAnnotArg(args) =>
            if (putEntry(carg))
              args foreach putClassfileAnnotArg
          case NestedAnnotArg(annInfo) =>
            putAnnotation(annInfo)
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

    /** Write a reference to object, i.e., the object's number in the map
     *  <code>index</code>.
     *
     *  @param ref ...
     */
    private def writeRef(ref: AnyRef) { writeNat(index(ref)) }
    private def writeRefs(refs: List[AnyRef]) { refs foreach writeRef }

    /** Write name, owner, flags, and info of a symbol.
     */
    private def writeSymInfo(sym: Symbol) {
      writeRef(sym.name)
      writeRef(normalizedOwner(sym))
      writeLongNat((rawFlagsToPickled(sym.flags & PickledFlags)))
      if (sym.privateWithin != NoSymbol) writeRef(sym.privateWithin)
      writeRef(sym.info)
    }

    /** Write a name in UTF8 format. */
    private def writeName(name: Name) {
      ensureCapacity(name.length * 3)
      writeIndex = name.copyUTF8(bytes, writeIndex)
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
      carg match {
        case LiteralAnnotArg(const) =>
          writeRef(const)
        case ArrayAnnotArg(args) =>
          writeRef(carg)
        case NestedAnnotArg(annInfo) =>
          writeRef(annInfo)
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
          if (sym.thisSym.tpe != sym.tpe) writeRef(sym.typeOfThis)
          CLASSsym
        case sym: TypeSymbol =>
          writeSymInfo(sym)
          if (sym.isAbstractType) TYPEsym else ALIASsym
        case sym: TermSymbol =>
          if (!sym.isModule && sym.defaultGetter != NoSymbol)
            writeRef(sym.defaultGetter)
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
        case MethodType(formals, restpe) =>
          writeRef(restpe); writeRefs(formals)
          if (entry.isInstanceOf[ImplicitMethodType]) IMPLICITMETHODtpe
          else METHODtpe
        case PolyType(tparams, restpe) =>
          writeRef(restpe); writeRefs(tparams); POLYtpe
        case ExistentialType(tparams, restpe) =>
          writeRef(restpe); writeRefs(tparams); EXISTENTIALtpe
        case DeBruijnIndex(l, i) =>
          writeNat(l); writeNat(i); DEBRUIJNINDEXtpe
        case c @ Constant(_) =>
          if (c.tag == BooleanTag) writeLong(if (c.booleanValue) 1 else 0)
          else if (ByteTag <= c.tag && c.tag <= LongTag) writeLong(c.longValue)
          else if (c.tag == FloatTag) writeLong(Float.floatToIntBits(c.floatValue))
          else if (c.tag == DoubleTag) writeLong(Double.doubleToLongBits(c.doubleValue))
          else if (c.tag == StringTag) writeRef(newTermName(c.stringValue))
          else if (c.tag == ClassTag) writeRef(c.typeValue)
          else if (c.tag == EnumTag) writeRef(c.symbolValue)
          LITERAL + c.tag // also treats UnitTag, NullTag; no value required
        case AnnotatedType(annotations, tp, selfsym) =>
          val staticAnnots = staticAnnotations(annotations)
          if (staticAnnots isEmpty) {
            writeBody(tp) // write the underlying type if there are no annotations
          } else {
            if (settings.selfInAnnots.value && selfsym != NoSymbol)
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

        case (target: Symbol, children: List[Symbol]) =>
          writeRef(target)
          for (c <- children) writeRef(c.asInstanceOf[Symbol])
          CHILDREN

        case EmptyTree =>
          writeNat(EMPTYtree)
          TREE

        case tree@PackageDef(name, stats) =>
          writeNat(PACKAGEtree)
          writeRef(tree.tpe)
          writeRef(tree.symbol)
          writeRef(tree.mods)
          writeRef(name)
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
          writeNat(tparams.length)
          writeRefs(tparams)
          writeNat(vparamss.length)
          for(vparams <- vparamss) {
            writeNat(vparams.length)
            writeRefs(vparams)
          }
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
          for ((from, to) <- selectors) {
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
          writeNat(parents.length)
          writeRefs(parents)
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

        case tree@Sequence(trees) =>
          writeNat(SEQUENCEtree)
          writeRef(tree.tpe)
          writeRefs(trees)
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
          val pflags = rawFlagsToPickled(flags)
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

    /** Print entry for diagnostics */
    private def printEntry(entry: AnyRef) {
      def printRef(ref: AnyRef) {
        print(index(ref)+
              (if (ref.isInstanceOf[Name]) "("+ref+") " else " "))
      }
      def printRefs(refs: List[AnyRef]) { refs foreach printRef }
      def printSymInfo(sym: Symbol) {
        var posOffset = 0
        printRef(sym.name)
        printRef(normalizedOwner(sym))
        print(flagsToString(sym.flags & PickledFlags)+" ")
        if (sym.privateWithin != NoSymbol) printRef(sym.privateWithin)
        printRef(sym.info)
      }
      def printBody(entry: AnyRef) = entry match {
        case name: Name =>
          print((if (name.isTermName) "TERMname " else "TYPEname ")+name)
        case NoSymbol =>
          print("NONEsym")
        case sym: Symbol if !isLocal(sym) =>
          if (sym.isModuleClass) {
            print("EXTMODCLASSref "); printRef(sym.name.toTermName)
          } else {
            print("EXTref "); printRef(sym.name)
          }
          if (!sym.owner.isRoot) printRef(sym.owner)
        case sym: ClassSymbol =>
          print("CLASSsym ")
          printSymInfo(sym)
          if (sym.thisSym.tpe != sym.tpe) printRef(sym.typeOfThis)
        case sym: TypeSymbol =>
          print(if (sym.isAbstractType) "TYPEsym " else "ALIASsym ")
          printSymInfo(sym)
        case sym: TermSymbol =>
          print(if (sym.isModule) "MODULEsym " else "VALsym ")
          printSymInfo(sym)
          if (!sym.isModule) printRef(sym.defaultGetter)
          if (sym.alias != NoSymbol) printRef(sym.alias)
        case NoType =>
          print("NOtpe")
        case NoPrefix =>
          print("NOPREFIXtpe")
        case ThisType(sym) =>
          print("THIStpe "); printRef(sym)
        case SingleType(pre, sym) =>
          print("SINGLEtpe "); printRef(pre); printRef(sym);
        case ConstantType(value) =>
          print("CONSTANTtpe "); printRef(value);
        case TypeRef(pre, sym, args) =>
          print("TYPEREFtpe "); printRef(pre); printRef(sym); printRefs(args);
        case TypeBounds(lo, hi) =>
          print("TYPEBOUNDStpe "); printRef(lo); printRef(hi);
        case tp @ RefinedType(parents, decls) =>
          print("REFINEDtpe "); printRef(tp.typeSymbol); printRefs(parents);
        case ClassInfoType(parents, decls, clazz) =>
          print("CLASSINFOtpe "); printRef(clazz); printRefs(parents);
        case MethodType(formals, restpe) =>
          print(if (entry.isInstanceOf[ImplicitMethodType]) "IMPLICITMETHODtpe " else "METHODtpe ");
          printRef(restpe); printRefs(formals)
        case PolyType(tparams, restpe) =>
          print("POLYtpe "); printRef(restpe); printRefs(tparams);
        case ExistentialType(tparams, restpe) =>
          print("EXISTENTIALtpe "); printRef(restpe); printRefs(tparams);
        case DeBruijnIndex(l, i) =>
          print("DEBRUIJNINDEXtpe "); print(l+" "+i)
        case c @ Constant(_) =>
          print("LITERAL ")
          if (c.tag == BooleanTag) print("Boolean "+(if (c.booleanValue) 1 else 0))
          else if (c.tag == ByteTag) print("Byte "+c.longValue)
          else if (c.tag == ShortTag) print("Short "+c.longValue)
          else if (c.tag == CharTag) print("Char "+c.longValue)
          else if (c.tag == IntTag) print("Int "+c.longValue)
          else if (c.tag == LongTag) print("Long "+c.longValue)
          else if (c.tag == FloatTag) print("Float "+c.floatValue)
          else if (c.tag == DoubleTag) print("Double "+c.doubleValue)
          else if (c.tag == StringTag) { print("String "); printRef(newTermName(c.stringValue)) }
          else if (c.tag == ClassTag) { print("Class "); printRef(c.typeValue) }
          else if (c.tag == EnumTag) { print("Enum "); printRef(c.symbolValue) }
        case AnnotatedType(annots, tp, selfsym) =>
          if (settings.selfInAnnots.value) {
            print("ANNOTATEDWSELFtpe ")
            printRef(tp)
            printRef(selfsym)
            printRefs(annots)
          } else {
            print("ANNOTATEDtpe ")
            printRef(tp)
            printRefs(annots)
          }
        case (target: Symbol, AnnotationInfo(atp, args, Nil)) =>
          print("SYMANNOT ")
          printRef(target)
          printRef(atp)
          for (c <- args) printRef(c)
        case (target: Symbol, children: List[_]) =>
          print("CHILDREN ")
          printRef(target)
          for (c <- children) printRef(c.asInstanceOf[Symbol])
        case AnnotationInfo(atp, args, Nil) =>
          print("ANNOTINFO")
          printRef(atp)
          for (c <- args) printRef(c)
        case _ =>
          throw new FatalError("bad entry: " + entry + " " + entry.getClass)
      }
      printBody(entry); println()
    }

    /** Write byte array */
    def finish {
      assert(writeIndex == 0)
      writeNat(MajorVersion)
      writeNat(MinorVersion)
      writeNat(ep)
      if (showSig) {
        println("Pickled info for "+rootName+" V"+MajorVersion+"."+MinorVersion)
      }
      for (i <- 0 until ep) {
        if (showSig) {
          print((i formatted "%3d: ")+(writeIndex formatted "%5d: "))
          printEntry(entries(i))
        }
        writeEntry(entries(i))
      }
      if (settings.Xshowcls.value == rootName.toString) {
        readIndex = 0
        ShowPickled.printFile(this, Console.out)
      }
    }

    override def toString() = "" + rootName + " in " + rootOwner
  }
}
