/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import java.lang.{Float, Double}
import scala.collection.mutable.HashMap
import scala.tools.nsc.util.{Position, ShowPickled}
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
        if (!sym.children.isEmpty)
          putChildren(sym, sym.children.toList.sort((x, y) => x isLess y))

        for (val attr <- sym.attributes.reverse) {
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
        case NoType | NoPrefix =>
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
          putType(tp) // the attributes should be stored, but it is not yet
                      // decided how to handle that.
        case _ =>
          throw new FatalError("bad type: " + tp + "(" + tp.getClass() + ")")
      }
    }
    private def putTypes(tps: List[Type]): unit = tps foreach putType

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
      for (val c <- attr.args) putConstant(c)
      for (val (name, c) <- attr.assocs) { putEntry(name); putConstant(c) }
    }

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
      if (sym.pos != Position.NOPOS && sym.owner.isClass) {
        writeNat(sym.pos)
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
        case c @ Constant(_) =>
          if (c.tag == BooleanTag) writeLong(if (c.booleanValue) 1 else 0)
          else if (ByteTag <= c.tag && c.tag <= LongTag) writeLong(c.longValue)
          else if (c.tag == FloatTag) writeLong(Float.floatToIntBits(c.floatValue))
          else if (c.tag == DoubleTag) writeLong(Double.doubleToLongBits(c.doubleValue))
          else if (c.tag == StringTag) writeRef(newTermName(c.stringValue))
          else if (c.tag == ClassTag) writeRef(c.typeValue)
          LITERAL + c.tag
        case AnnotatedType(attribs, tp) =>
          writeBody(tp) // obviously, this should be improved
        case (target: Symbol, attr @ AnnotationInfo(atp, args, assocs)) =>
          writeRef(target)
          writeRef(atp)
          for (val c <- args) writeRef(c.asInstanceOf[Constant])
          for (val (name, c) <- assocs) { writeRef(name); writeRef(c.asInstanceOf[Constant]) }
          ATTRIBUTE
        case (target: Symbol, children: List[_]) =>
          writeRef(target)
          for (val c <- children) writeRef(c.asInstanceOf[Symbol])
          CHILDREN
        case _ =>
          throw new FatalError("bad entry: " + entry + " " + entry.getClass())//debug
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
      for (val i <- 0 until ep) writeEntry(entries(i));
      if (settings.Xshowcls.value == rootName.toString) {
        readIndex = 0
        ShowPickled.printFile(this, Console.out)
      }
    }

    override def toString() = "" + rootName + " in " + rootOwner
  }
}
