/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
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

  val phaseName = "pickler"

  def newPhase(prev: Phase): StdPhase = new PicklePhase(prev)

  class PicklePhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): unit = {
      def pickle(tree: Tree): unit = {

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
          case ClassDef(_, _, _, _, _) | ModuleDef(_, _, _) =>
            val sym = tree.symbol
            val pickle = new Pickle(sym.name.toTermName, sym.owner)
            add(sym, pickle)
            add(sym.linkedSym, pickle)
            pickle.finish
          case _ =>
        }
      }
      pickle(unit.body)
    }
  }

  private class Pickle(rootName: Name, rootOwner: Symbol)
        extends PickleBuffer(new Array[byte](4096), -1, 0) {
    import scala.collection.mutable.HashMap
    private var entries = new Array[AnyRef](256)
    private var ep = 0
    private val index = new HashMap[AnyRef, int]

    /** Is root in symbol.owner*?
     *
     *  @param sym ...
     *  @return    ...
     */
    private def isLocal(sym: Symbol): boolean = (
      sym.isRefinementClass ||
      sym.name.toTermName == rootName && sym.owner == rootOwner ||
      sym != NoSymbol && isLocal(sym.owner)
    )

    // Phase 1 methods: Populate entries/index ------------------------------------

    /** Store entry <code>e</code> in index at next available position unless
     *  it is already there.
     *
     *  @param entry ...
     *  @return      <code>true</code> iff entry is new.
     */
    private def putEntry(entry: AnyRef): boolean = index.get(entry) match {
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
    def putSymbol(sym: Symbol): unit = if (putEntry(sym)) {
      if (isLocal(sym)) {
        putEntry(sym.name)
        putSymbol(sym.owner)
        putSymbol(sym.privateWithin)
        putType(sym.info)
        if (sym.thisSym.tpe != sym.tpe)
          putType(sym.typeOfThis);
        putSymbol(sym.alias)
        if (!sym.children.isEmpty) {
          val (locals, globals) = sym.children.toList.partition(.isLocalClass)
          val children =
            if (locals.isEmpty) globals
            else {
              val localChildDummy = sym.newClass(sym.pos, nme.LOCALCHILD)
              localChildDummy.setInfo(ClassInfoType(List(sym.tpe), EmptyScope, localChildDummy))
              localChildDummy :: globals
            }
          putChildren(sym, children.sort((x, y) => x isLess y))
        }
        for (attr <- sym.attributes.reverse) {
          if (attr.atp.symbol isNonBottomSubClass definitions.StaticAnnotationClass)
            putAnnotation(sym, attr)
        }
      } else if (sym != NoSymbol) {
        putEntry(if (sym.isModuleClass) sym.name.toTermName else sym.name)
        if (!sym.owner.isRoot) putSymbol(sym.owner)
      }
    }
    private def putSymbols(syms: List[Symbol]) =
      syms foreach putSymbol


    /** Store type and everythig it refers to in map <code>index</code>.
     *
     *  @param tp ...
     */
    private def putType(tp: Type): unit = if (putEntry(tp)) {
      tp match {
        case NoType | NoPrefix | DeBruijnIndex(_, _) =>
          ;
        case ThisType(sym) =>
          putSymbol(sym)
        case SingleType(pre, sym) =>
          putType(pre); putSymbol(sym)
        case ConstantType(value) =>
          putConstant(value)
        case TypeRef(pre, sym, args) =>
          putType(pre); putSymbol(sym); putTypes(args)
        case TypeBounds(lo, hi) =>
          putType(lo); putType(hi)
        case RefinedType(parents, decls) =>
          putSymbol(tp.symbol); putTypes(parents); putSymbols(decls.toList)
        case ClassInfoType(parents, decls, clazz) =>
          putSymbol(clazz); putTypes(parents); putSymbols(decls.toList)
        case MethodType(formals, restpe) =>
          putType(restpe); putTypes(formals)
        case PolyType(tparams, restpe) =>
          putType(restpe); putSymbols(tparams)
        case AnnotatedType(attribs, tp) =>
          putType(tp); putAnnotations(attribs)
        case _ =>
          throw new FatalError("bad type: " + tp + "(" + tp.getClass + ")")
      }
    }
    private def putTypes(tps: List[Type]): unit = tps foreach putType

    private def putTree(tree: reflect.Tree): unit = if (putEntry(tree)) {
      tree match {
        case reflect.Ident(sym) => putSymbol(sym)
        case reflect.Select(qual, sym) =>  putTree(qual); putSymbol(sym)
        case reflect.Literal(value) => putConstant(Constant(value))
        case reflect.Apply(fun, args) => putTree(fun); putRefTrees(args)
        case reflect.TypeApply(fun, args) =>  putTree(fun); putRefTypes(args)
        case reflect.Function(params, body) =>
          putRefSymbols(params); putTree(body)
        case reflect.This(sym) =>  putSymbol(sym)
        case reflect.Block(stats, expr) => putRefTrees(stats); putTree(expr)
        case reflect.New(clz) => putTree(clz)
        case reflect.If(condition, trueCase, falseCase) =>
          putTree(condition); putTree(trueCase); putTree(falseCase)
        case reflect.Assign(destination, source) =>
          putTree(destination); putTree(source)
        case reflect.Target(sym, body) => putSymbol(sym); putTree(body)
        case reflect.Goto(target) => putSymbol(target)
        case reflect.ValDef(sym, rhs)  => putSymbol(sym); putTree(rhs)
        case reflect.ClassDef(sym, tpe, impl) =>
          putSymbol(sym); putType(tpe); putTree(impl)
        case reflect.DefDef(sym, vparamss, ret, rhs) =>
          putSymbol(sym); putRefTreess(vparamss); putType(ret); putTree(rhs)
        case reflect.Super(psym) => putSymbol(psym)
        case reflect.Template(parents, body) =>
          putRefTypes(parents); putRefTrees(body)
        case _ =>
          throw new FatalError("bad tree: " + tree + "(" + tree.getClass + ")")
      }
    }
    private def putRefTrees(trees: List[reflect.Tree]) = trees foreach putTree
    private def putRefTreess(trees: List[List[reflect.Tree]]) =
      trees foreach putRefTrees

    private def putType(tpe: reflect.Type): unit = if(putEntry(tpe)) {
      tpe match {
        case reflect.NoPrefix => ()
        case reflect.NoType => ()
        case reflect.NamedType(fullname) => putConstant(Constant(fullname))
        case reflect.PrefixedType(pre, sym) => putType(pre); putSymbol(sym)
        case reflect.SingleType(pre, sym) => putType(pre); putSymbol(sym)
        case reflect.ThisType(clazz) => putSymbol(clazz)
        case reflect.AppliedType(tpe, args) => putType(tpe); putRefTypes(args)
        case reflect.TypeBounds(lo, hi) => putType(lo); putType(hi)
        case reflect.MethodType(formals, restpe) => //can be implicit
          putRefTypes(formals); putType(restpe)
        case reflect.PolyType(typeParams, typeBounds, resultType) =>
          putRefSymbols(typeParams)
          for ((t1,t2) <- typeBounds) {
            putType(t1)
            putType(t2)
          }
          putType(resultType)
        case _ =>
          throw new FatalError("bad type: " + tpe + "(" + tpe.getClass + ")")

      }
    }
    private def putRefTypes(tpes: List[reflect.Type]) =
      tpes foreach putType

    private def putSymbol(sym: reflect.Symbol): unit = if(putEntry(sym)) {
      sym match {
        case reflect.Class(fullname) =>
          putConstant(Constant(fullname))
        case reflect.Method(fullname, tpe) =>
          putConstant(Constant(fullname))
          putType(tpe)
        case reflect.Field(fullname, tpe) =>
          putConstant(Constant(fullname))
          putType(tpe)
        case reflect.TypeField(fullname, tpe) =>
          putConstant(Constant(fullname))
          putType(tpe)
        case reflect.LocalValue(owner, name, tpe) =>
          putSymbol(owner)
          putConstant(Constant(name))
          putType(tpe)
        case reflect.LocalMethod(owner, name, tpe) =>
          putSymbol(owner)
          putConstant(Constant(name))
          putType(tpe)
        case reflect.NoSymbol => ()
        case reflect.RootSymbol => ()
        case reflect.LabelSymbol(name) =>
          putConstant(Constant(name))
      }
    }
    private def putRefSymbols(syms: List[reflect.Symbol]) =
      syms foreach putSymbol

    /** Store constant in map <code>index</code>.
     *
     *  @param c ...
     */
    private def putConstant(c: Constant) =
      if (putEntry(c)) {
        if (c.tag == StringTag) putEntry(newTermName(c.stringValue))
        else if (c.tag == ClassTag) putEntry(c.typeValue)
      }

    private def putChildren(sym: Symbol, children: List[Symbol]): unit = {
      assert(putEntry((sym, children)))
      children foreach putSymbol
    }

    private def putAnnotation(sym: Symbol, attr: AnnotationInfo[Constant]): unit = {
      assert(putEntry((sym, attr)))
      putType(attr.atp)
      for (c <- attr.args) putConstant(c)
      for ((name, c) <- attr.assocs) { putEntry(name); putConstant(c) }
    }

    private def putAnnotation(annot: AnnotationInfo[Any]): unit =
      if(putEntry(annot)) {
        val AnnotationInfo(tpe, args, assocs) = annot
        putType(tpe)
        args foreach putTreeOrConstant
        for ((name, rhs) <- assocs) { putEntry(name); putTreeOrConstant(rhs) }
      }

    private def putTreeOrConstant(x: Any) {
      x match {
        case c:Constant => putConstant(c)
        case tree:reflect.Tree => putTree(tree)
        case _ =>
          throw new FatalError("attribute neither tree nor constant: " + x)
      }
    }

    private def putAnnotations(annots: List[AnnotationInfo[Any]]): unit =
      annots foreach putAnnotation

    // Phase 2 methods: Write all entries to byte array ------------------------------

    private val buf = new PickleBuffer(new Array[byte](4096), -1, 0)

    /** Write a reference to object, i.e., the object's number in the map
     *  <code>index</code>.
     *
     *  @param ref ...
     */
    private def writeRef(ref: AnyRef): unit = writeNat(index(ref))
    private def writeRefs(refs: List[AnyRef]): unit = refs foreach writeRef

    /** Write name, owner, flags, and info of a symbol.
     *
     *  @param sym ...
     *  @return    the position offset
     */
    private def writeSymInfo(sym: Symbol): int = {
      var posOffset = 0
      if (sym.pos != NoPosition && sym.owner.isClass && !sym.pos.offset.isEmpty) {
        writeNat(sym.pos.offset.get)
        posOffset = PosOffset
      }
      writeRef(sym.name)
      writeRef(sym.owner)
      writeNat((sym.flags & PickledFlags).asInstanceOf[int])
      if (sym.privateWithin != NoSymbol) writeRef(sym.privateWithin)
      writeRef(sym.info)
      posOffset
    }

    /** Write a name in UTF8 format. */
    def writeName(name: Name): unit = {
      ensureCapacity(name.length * 3)
      writeIndex = name.copyUTF8(bytes, writeIndex)
    }

    /** Write an entry */
    private def writeEntry(entry: AnyRef): unit = {
      def writeBody(entry: AnyRef): int = entry match {
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
          if (!sym.owner.isRoot) writeRef(sym.owner);
          tag
        case sym: ClassSymbol =>
          val posOffset = writeSymInfo(sym)
          if (sym.thisSym.tpe != sym.tpe) writeRef(sym.typeOfThis)
          CLASSsym + posOffset
        case sym: TypeSymbol =>
          val posOffset = writeSymInfo(sym)
          (if (sym.isAbstractType) TYPEsym else ALIASsym) + posOffset
        case sym: TermSymbol =>
          val posOffset = writeSymInfo(sym)
          if (sym.alias != NoSymbol) writeRef(sym.alias)
          (if (sym.isModule) MODULEsym else VALsym) + posOffset
        case NoType =>
          NOtpe
        case NoPrefix =>
          NOPREFIXtpe
        case ThisType(sym) =>
          writeRef(sym); THIStpe
        case SingleType(pre, sym) =>
          writeRef(pre); writeRef(sym); SINGLEtpe
        case ConstantType(value) =>
          writeRef(value); CONSTANTtpe
        case TypeRef(pre, sym, args) =>
          writeRef(pre); writeRef(sym); writeRefs(args); TYPEREFtpe
        case TypeBounds(lo, hi) =>
          writeRef(lo); writeRef(hi); TYPEBOUNDStpe
        case tp @ RefinedType(parents, decls) =>
          writeRef(tp.symbol); writeRefs(parents); REFINEDtpe
        case ClassInfoType(parents, decls, clazz) =>
          writeRef(clazz); writeRefs(parents); CLASSINFOtpe
        case MethodType(formals, restpe) =>
          writeRef(restpe); writeRefs(formals)
          if (entry.isInstanceOf[ImplicitMethodType]) IMPLICITMETHODtpe
          else METHODtpe
        case PolyType(tparams, restpe) =>
          writeRef(restpe); writeRefs(tparams); POLYtpe
        case DeBruijnIndex(l, i) =>
          writeNat(l); writeNat(i); DEBRUIJNINDEXtpe
        case c @ Constant(_) =>
          if (c.tag == BooleanTag) writeLong(if (c.booleanValue) 1 else 0)
          else if (ByteTag <= c.tag && c.tag <= LongTag) writeLong(c.longValue)
          else if (c.tag == FloatTag) writeLong(Float.floatToIntBits(c.floatValue))
          else if (c.tag == DoubleTag) writeLong(Double.doubleToLongBits(c.doubleValue))
          else if (c.tag == StringTag) writeRef(newTermName(c.stringValue))
          else if (c.tag == ClassTag) writeRef(c.typeValue)
          LITERAL + c.tag
        case AnnotatedType(attribs, tp) =>
          writeRef(tp)
          writeRefs(attribs)
          ANNOTATEDtpe
        case (target: Symbol, attr @ AnnotationInfo(atp, args, assocs)) =>
          writeRef(target)
          writeRef(atp)
          for (c <- args) writeRef(c.asInstanceOf[Constant])
          for ((name, c) <- assocs) { writeRef(name); writeRef(c.asInstanceOf[Constant]) }
          ATTRIBUTE
        case (target: Symbol, children: List[_]) =>
          writeRef(target)
          for (c <- children) writeRef(c.asInstanceOf[Symbol])
          CHILDREN
        case reflect.Ident(sym) =>
          writeNat(IDENTtree)
          writeRef(sym)
          REFLTREE
        case reflect.Select(qual, sym) =>
          writeNat(SELECTtree)
          writeRef(qual)
          writeRef(sym)
          REFLTREE
        case reflect.Literal(value) =>
          writeNat(LITERALtree)
          writeRef(Constant(value))
          REFLTREE
        case reflect.Apply(fun, args) =>
          writeNat(APPLYtree)
          writeRef(fun)
          writeRefs(args)
          REFLTREE
        case reflect.TypeApply(fun, args) =>
          writeNat(TYPEAPPLYtree)
          writeRef(fun)
          writeRef(args)
          REFLTREE
        case reflect.Function(params, body) =>
          writeNat(FUNCTIONtree)
          writeRef(body)
          writeRefs(params)
          REFLTREE
        case reflect.This(sym) =>
          writeNat(THIStree)
          writeRef(sym)
          REFLTREE
        case reflect.Block(stats, expr) =>
          writeNat(BLOCKtree)
          writeRef(expr)
          writeRefs(stats)
          REFLTREE
        case reflect.New(clz) =>
          writeNat(NEWtree)
          writeRef(clz)
          REFLTREE
        case reflect.If(condition, trueCase, falseCase) =>
          writeNat(IFtree)
          writeRef(condition)
          writeRef(trueCase)
          writeRef(falseCase)
          REFLTREE
        case reflect.Assign(destination, source) =>
          writeNat(ASSIGNtree)
          writeRef(destination)
          writeRef(source)
          REFLTREE
        case reflect.Target(sym, body) =>
          writeNat(TARGETtree)
          writeRef(sym)
          writeRef(body)
          REFLTREE
        case reflect.Goto(target) =>
          writeNat(GOTOtree)
          writeRef(target)
          REFLTREE
        case reflect.ValDef(sym, rhs)  =>
          writeNat(VALDEFtree)
          writeRef(sym)
          writeRef(rhs)
          REFLTREE
        case reflect.ClassDef(sym, tpe, impl) =>
          writeNat(CLASSDEFtree)
          writeRef(sym)
          writeRef(tpe)
          writeRef(impl)
          REFLTREE
        case reflect.DefDef(sym, vparamss, ret, rhs) =>
          writeNat(DEFDEFtree)
          writeRef(sym)
          writeRef(ret)
          writeRef(rhs)
          for (vparams <- vparamss) {
            writeNat(vparams.length)
            writeRefs(vparams)
          }
          REFLTREE
        case reflect.Super(psym) =>
          writeNat(SUPERtree)
          writeRef(psym)
          REFLTREE
        case reflect.Template(parents, body) =>
          writeNat(TEMPLATEtree)
          writeNat(parents.length)
          writeRefs(parents)
          writeRefs(body)
          REFLTREE
        case reflect.NoPrefix =>
          writeNat(NOPREFIXrtpe)
          REFLTYPE
        case reflect.NoType =>
          writeNat(NOrtpe)
          REFLTYPE
        case reflect.NamedType(fullname) =>
          writeNat(NAMEDrtpe)
          writeRef(Constant(fullname))
          REFLTYPE
        case reflect.PrefixedType(pre, sym) =>
          writeNat(PREFIXEDrtpe)
          writeRef(pre)
          writeRef(sym)
          REFLTYPE
        case reflect.SingleType(pre, sym) =>
          writeNat(SINGLErtpe)
          writeRef(pre)
          writeRef(sym)
          REFLTYPE
        case reflect.ThisType(clazz) =>
          writeNat(THISrtpe)
          writeRef(clazz)
          REFLTYPE
        case reflect.AppliedType(tpe, args) =>
          writeNat(APPLIEDrtpe)
          writeRef(tpe)
          writeRef(args)
          REFLTYPE
        case reflect.TypeBounds(lo, hi) =>
          writeNat(TYPEBOUNDSrtpe)
          writeRef(lo)
          writeRef(hi)
          REFLTYPE
        case entry@reflect.MethodType(formals, restpe) => //can be implicit
          if(entry.isInstanceOf[ImplicitMethodType])
            writeNat(IMPLICITMETHODrtpe)
          else
            writeNat(METHODrtpe)
          writeRef(restpe)
          writeRefs(formals)
          REFLTYPE
        case reflect.PolyType(typeParams, typeBounds, resultType) =>
          writeNat(POLYrtpe)
          writeRef(resultType)
          writeNat(typeBounds.length)
          for ((t1,t2) <- typeBounds) {
            writeRef(t1)
            writeRef(t2)
          }
          writeRefs(typeParams)
          REFLTYPE
        case reflect.Class(fullname) =>
          writeNat(CLASSrsym)
          writeRef(Constant(fullname))
          REFLSYM
        case reflect.Method(fullname, tpe) =>
          writeNat(METHODrsym)
          writeRef(Constant(fullname))
          writeRef(tpe)
          REFLSYM
        case reflect.Field(fullname, tpe) =>
          writeNat(FIELDrsym)
          writeRef(Constant(fullname))
          writeRef(tpe)
          REFLSYM
        case reflect.TypeField(fullname, tpe) =>
          writeNat(TYPEFIELDrsym)
          writeRef(Constant(fullname))
          writeRef(tpe)
          REFLSYM
        case reflect.LocalValue(owner, name, tpe) =>
          writeNat(LOCALVALUErsym)
          writeRef(owner)
          writeRef(Constant(name))
          writeRef(tpe)
          REFLSYM
        case reflect.LocalMethod(owner, name, tpe) =>
          writeNat(LOCALMETHODrsym)
          writeRef(owner)
          writeRef(Constant(name))
          writeRef(tpe)
          REFLSYM
        case reflect.NoSymbol =>
          writeNat(NOSYMBOLrsym)
          REFLSYM
        case reflect.RootSymbol =>
          writeNat(ROOTSYMBOLrsym)
          REFLSYM
        case reflect.LabelSymbol(name) =>
          writeNat(LABELSYMBOLrsym)
          REFLSYM
        case AnnotationInfo(target, args, assocs) =>
          writeRef(target)
          writeNat(args.length)
          for (tree <- args) writeRef(tree.asInstanceOf[reflect.Tree])
          for ((name, tree) <- assocs) {
            writeRef(name);
            writeRef(tree.asInstanceOf[reflect.Tree])
          }
          ATTRIBTREE

        case _ =>
          throw new FatalError("bad entry: " + entry + " " + entry.getClass)
      }
      val startpos = writeIndex
      writeByte(0); writeByte(0)
      patchNat(startpos, writeBody(entry))
      patchNat(startpos + 1, writeIndex - (startpos + 2))
    }

    /** Write byte array */
    def finish = {
      assert(writeIndex == 0)
      writeNat(MajorVersion)
      writeNat(MinorVersion)
      writeNat(ep)
      if (settings.debug.value) log("" + ep + " entries")//debug
      for (i <- 0 until ep) writeEntry(entries(i))
      if (settings.Xshowcls.value == rootName.toString) {
        readIndex = 0
        ShowPickled.printFile(this, Console.out)
      }
    }

    override def toString() = "" + rootName + " in " + rootOwner
  }
}
