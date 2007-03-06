/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import scala.tools.nsc.util.Position
import scala.tools.util.UTF8Codec
import java.lang.{Float, Double}

import Flags._
import PickleFormat._
import collection.mutable.{HashMap, ListBuffer}
import java.io.IOException

/** This abstract class implements ..
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler {
  val global: Global
  import global._
  import RequiresIntsAsPositions._;

  /**
   *  @param bytes    bytearray from which we unpickle
   *  @param filename filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[byte], offset: int, classRoot: Symbol, moduleRoot: Symbol, filename: String): unit = try {
    new UnPickle(bytes, offset, classRoot, moduleRoot)
  } catch {
    case ex: IOException =>
      throw ex
    case ex: Throwable =>
      if (settings.debug.value) ex.printStackTrace()
      throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
  }

  private class UnPickle(bytes: Array[byte], offset: int, classRoot: Symbol, moduleRoot: Symbol) extends PickleBuffer(bytes, offset, -1) {
    if (settings.debug.value) global.log("unpickle " + classRoot + " and " + moduleRoot)
    checkVersion()
    private val index = createIndex
    private val entries = new Array[AnyRef](index.length)
    private val symScopes = new HashMap[Symbol, Scope]

    for (val i <- 0 until index.length) {
      if (isSymbolEntry(i)) { at(i, readSymbol); {} }
      else if (isAnnotationEntry(i)) { at(i, readAnnotation); {} }
    }

    if (settings.debug.value) global.log("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug

    private def checkVersion() = {
      val major = readNat()
      val minor = readNat()
      if (major != MajorVersion || minor > MinorVersion)
        throw new IOException("Scala signature " + classRoot.name +
                              " has wrong version\n expected: " +
                              MajorVersion + "." + MinorVersion +
                              "\n found: " + major + "." + minor)
    }

    /** The scope associated with given symbol */
    private def symScope(sym: Symbol) = symScopes.get(sym) match {
      case None => val s = newScope; symScopes(sym) = s; s
      case Some(s) => s
    }

    /** Does entry represent an (internal) symbol */
    private def isSymbolEntry(i: int): boolean = {
      val tag = bytes(index(i)) % PosOffset
      (firstSymTag <= tag && tag <= lastSymTag &&
       (tag != CLASSsym || !isRefinementSymbolEntry(i)))
    }

    /** Does entry represent an (internal or external) symbol */
    private def isSymbolRef(i: int): boolean = {
      val tag = bytes(index(i)) % PosOffset
      (firstSymTag <= tag && tag <= lastExtSymTag)
    }

    /** Does entry represent a name? */
    private def isNameEntry(i: int): boolean = {
      val tag = bytes(index(i))
      tag == TERMname || tag == TYPEname
    }

    /** Does entry represent a symbol attribute? */
    private def isAnnotationEntry(i: int): boolean = {
      val tag = bytes(index(i))
      tag == ATTRIBUTE || tag == CHILDREN
    }

    /** Does entry represent a refinement symbol?
     *  pre: Entry is a class symbol
     */
    private def isRefinementSymbolEntry(i: int): boolean = {
      val savedIndex = readIndex
      readIndex = index(i)
      val tag = readByte()
      if (tag % PosOffset != CLASSsym) assert(false)
      readNat(); // read length
      if (tag > PosOffset) readNat(); // read position
      val result = readNameRef() == nme.REFINE_CLASS_NAME.toTypeName
      readIndex = savedIndex
      result
    }

    /** If entry at `i' is undefined, define it by performing operation `op' with
     *  readIndex at start of i'th entry. Restore readIndex afterwards. */
    private def at[T <: AnyRef](i: int, op: () => T): T = {
      var r = entries(i)
      if (r eq null) {
        val savedIndex = readIndex
        readIndex = index(i)
        r = op()
        assert(entries(i) eq null, entries(i))
        entries(i) = r
        readIndex = savedIndex
      }
      r.asInstanceOf[T]
    }

    /** Read a name */
    private def readName(): Name = {
      val tag = readByte()
      val len = readNat()
      tag match {
        case TERMname => newTermName(bytes, readIndex, len)
        case TYPEname => newTypeName(bytes, readIndex, len)
        case _ => errorBadSignature("bad name tag: " + tag)
      }
    }

    /** Read a symbol */
    private def readSymbol(): Symbol = {
      val tag = readByte()
      val end = readNat() + readIndex
      var sym: Symbol = NoSymbol
      tag match {
        case EXTref | EXTMODCLASSref =>
          val name = readNameRef()
          val owner = if (readIndex == end) definitions.RootClass else readSymbolRef()
          sym = if (name.toTermName == nme.ROOT) definitions.RootClass
                else if (name == nme.ROOTPKG) definitions.RootPackage
                else if (tag == EXTref) owner.info.decl(name)
                else owner.info.decl(name).moduleClass
          if (sym == NoSymbol) {
            errorBadSignature(
              "reference " + (if (name.isTypeName) "type " else "value ") +
              name.decode + " of " + owner + " refers to nonexisting symbol.")
          }
        case NONEsym =>
          sym = NoSymbol
        case _ =>
          val unusedPos = {
            if (tag > PosOffset) readNat();
            else Position.NOPOS
          }
          val pos : PositionType = if (true) FirstPos else unusedPos;
          val name = readNameRef()
          val owner = readSymbolRef()
          val flags = readNat()
          var privateWithin: Symbol = NoSymbol
          var inforef = readNat()
          if (isSymbolRef(inforef)) {
            privateWithin = at(inforef, readSymbol)
            inforef = readNat()
          }
          (tag % PosOffset) match {
            case TYPEsym =>
              sym = owner.newAbstractType(pos, name)
            case ALIASsym =>
              sym = owner.newAliasType(pos, name)
            case CLASSsym =>
              sym =
                if (name == classRoot.name && owner == classRoot.owner)
                  (if ((flags & MODULE) != 0) moduleRoot.moduleClass
                   else classRoot).setPos(pos)
                else
                  if ((flags & MODULE) != 0) owner.newModuleClass(pos, name)
                  else owner.newClass(pos, name)
              if (readIndex != end) sym.typeOfThis = new LazyTypeRef(readNat())
            case MODULEsym =>
              val clazz = at(inforef, readType).symbol
              sym =
                if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot
                else {
                  assert(clazz.isInstanceOf[ModuleClassSymbol], clazz)
                  val mclazz = clazz.asInstanceOf[ModuleClassSymbol]
                  val m = owner.newModule(pos, name, mclazz)
                  mclazz.setSourceModule(m)
                  m
                }
            case VALsym =>
              sym = if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot.resetFlag(MODULE)
                    else owner.newValue(pos, name)
            case _ =>
              errorBadSignature("bad symbol tag: " + tag)
          }
          sym.setFlag(flags)
          sym.privateWithin = privateWithin
          if (readIndex != end) assert(sym hasFlag (SUPERACCESSOR | PARAMACCESSOR))
          if (sym hasFlag SUPERACCESSOR) assert(readIndex != end)
          sym.setInfo(
            if (readIndex != end) new LazyTypeRefAndAlias(inforef, readNat())
            else new LazyTypeRef(inforef))
          if (sym.owner.isClass && sym != classRoot && sym != moduleRoot &&
              !sym.isModuleClass && !sym.isRefinementClass && !sym.isTypeParameter)
            symScope(sym.owner) enter sym
      }
      sym
    }

    /** Read a type */
    private def readType(): Type = {
      val tag = readByte()
      val end = readNat() + readIndex
      tag match {
        case NOtpe =>
          NoType
        case NOPREFIXtpe =>
          NoPrefix
        case THIStpe =>
          mkThisType(readSymbolRef())
        case SINGLEtpe =>
          singleType(readTypeRef(), readSymbolRef())
        case CONSTANTtpe =>
          mkConstantType(readConstantRef())
        case TYPEREFtpe =>
          rawTypeRef(readTypeRef(), readSymbolRef(), until(end, readTypeRef))
        case TYPEBOUNDStpe =>
          mkTypeBounds(readTypeRef(), readTypeRef())
        case REFINEDtpe =>
          val clazz = readSymbolRef()
/*
          val ps = until(end, readTypeRef)
          val dcls = symScope(clazz)
          new RefinedType(ps, dcls) { override def symbol = clazz }
*/
          new RefinedType(until(end, readTypeRef), symScope(clazz)) { override def symbol = clazz }
        case CLASSINFOtpe =>
          val clazz = readSymbolRef()
          ClassInfoType(until(end, readTypeRef), symScope(clazz), clazz)
        case METHODtpe =>
          val restpe = readTypeRef()
          MethodType(until(end, readTypeRef), restpe)
        case IMPLICITMETHODtpe =>
          val restpe = readTypeRef()
          ImplicitMethodType(until(end, readTypeRef), restpe)
        case POLYtpe =>
          val restpe = readTypeRef()
          PolyType(until(end, readSymbolRef), restpe)
        case _ =>
          errorBadSignature("bad type tag: " + tag)
      }
    }

    /** Read a constant */
    private def readConstant(): Constant = {
      val tag = readByte()
      val len = readNat()
      tag match {
        case LITERALunit    => Constant(())
        case LITERALboolean => Constant(if (readLong(len) == 0) false else true)
        case LITERALbyte    => Constant(readLong(len).asInstanceOf[byte])
        case LITERALshort   => Constant(readLong(len).asInstanceOf[short])
        case LITERALchar    => Constant(readLong(len).asInstanceOf[char])
        case LITERALint     => Constant(readLong(len).asInstanceOf[int])
        case LITERALlong    => Constant(readLong(len))
        case LITERALfloat   => Constant(Float.intBitsToFloat(readLong(len).asInstanceOf[int]))
        case LITERALdouble  => Constant(Double.longBitsToDouble(readLong(len)))
        case LITERALstring  => Constant(readNameRef().toString())
        case LITERALnull    => Constant(null)
        case LITERALclass   => Constant(readTypeRef())
        case _              => errorBadSignature("bad constant tag: " + tag)
      }
    }

    /** Read an attribute and store in referenced symbol */
    private def readAnnotation(): AnyRef = {
      val tag = readByte()
      val end = readNat() + readIndex
      val target = readSymbolRef()
      if (tag == ATTRIBUTE) {
        val attrType = readTypeRef()
        val args = new ListBuffer[Constant]
        val assocs = new ListBuffer[(Name, Constant)]
        while (readIndex != end) {
          val argref = readNat()
          if (isNameEntry(argref)) assocs += (at(argref, readName), readConstantRef())
          else args += at(argref, readConstant)
        }
        val attr = AnnotationInfo(attrType, args.toList, assocs.toList)
        target.attributes = attr :: target.attributes
      } else if (tag == CHILDREN) {
        while (readIndex != end) target addChild readSymbolRef()
      }
      null
    }

    /** Read a reference to a name, symbol, type or constant */
    private def readNameRef(): Name = at(readNat(), readName)
    private def readSymbolRef(): Symbol = at(readNat(), readSymbol)
    private def readTypeRef(): Type = at(readNat(), readType)
    private def readConstantRef(): Constant = at(readNat(), readConstant)

    private def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    private class LazyTypeRef(i: int) extends LazyType {
      private val definedAtRunId = currentRunId
      override def complete(sym: Symbol): unit = {
        val tp = at(i, readType)
        sym setInfo tp
        if (currentRunId != definedAtRunId) sym.setInfo(adaptToNewRunMap(tp))
      }
      override def load(sym: Symbol): unit = complete(sym)
    }

    private class LazyTypeRefAndAlias(i: int, j: int) extends LazyTypeRef(i) {
      override def complete(sym: Symbol): unit = {
        super.complete(sym)
        var alias = at(j, readSymbol)
        if (alias hasFlag OVERLOADED)
          alias = alias suchThat (alt => sym.tpe =:= sym.owner.thisType.memberType(alt))
        sym.asInstanceOf[TermSymbol].setAlias(alias)
      }
    }
  }
}
