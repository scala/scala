/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import java.io.IOException
import java.lang.{Float, Double}

import scala.tools.nsc.util.{Position, NoPosition}
import scala.tools.util.UTF8Codec

import Flags._
import PickleFormat._
import collection.mutable.{HashMap, ListBuffer}

/** This abstract class implements ..
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler {
  val global: Global
  import global._

  /**
   *  @param bytes    bytearray from which we unpickle
   *  @param filename filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) {
    try {
      new UnPickle(bytes, offset, classRoot, moduleRoot)
    } catch {
      case ex: IOException =>
        throw ex
      case ex: Throwable =>
        if (settings.debug.value) ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  private class UnPickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol) extends PickleBuffer(bytes, offset, -1) {
    if (settings.debug.value) global.log("unpickle " + classRoot + " and " + moduleRoot)
    checkVersion()
    private val index = createIndex
    private val entries = new Array[AnyRef](index.length)
    private val symScopes = new HashMap[Symbol, Scope]

    for (i <- 0 until index.length) {
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
    private def isSymbolEntry(i: Int): boolean = {
      val tag = bytes(index(i)) % PosOffset
      (firstSymTag <= tag && tag <= lastSymTag &&
       (tag != CLASSsym || !isRefinementSymbolEntry(i)))
    }

    /** Does entry represent an (internal or external) symbol */
    private def isSymbolRef(i: Int): Boolean = {
      val tag = bytes(index(i)) % PosOffset
      (firstSymTag <= tag && tag <= lastExtSymTag)
    }

    /** Does entry represent a name? */
    private def isNameEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      tag == TERMname || tag == TYPEname
    }

    /** Does entry represent a symbol attribute? */
    private def isAnnotationEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      tag == ATTRIBUTE || tag == CHILDREN
    }

    /** Does entry represent a refinement symbol?
     *  pre: Entry is a class symbol
     */
    private def isRefinementSymbolEntry(i: Int): Boolean = {
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

    /** If entry at <code>i</code> is undefined, define it by performing
     *  operation <code>op</code> with <code>readIndex at start of i'th
     *  entry. Restore <code>readIndex</code> afterwards.
     */
    private def at[T <: AnyRef](i: Int, op: () => T): T = {
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
          val unusedPos : Int = {
            if (tag > PosOffset) readNat
            else -1
          }
          val pos: Position = NoPosition
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
          sym.setFlag(flags.toLong & PickledFlags)
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
        case EXISTENTIALtpe =>
          val restpe = readTypeRef()
          ExistentialType(until(end, readSymbolRef), restpe)
        case ANNOTATEDtpe =>
          val tp = readTypeRef()
          val attribs = until(end, readTreeAttribRef)
          if(global.settings.Xplugtypes.value)
            AnnotatedType(attribs, tp)
          else
            tp  // Drop the annotations unless -Xplugtypes.
                // This way, people can distribute classfiles
                // including annotated types without them much
                // affecting those who disable -Xplugtypes
        case DEBRUIJNINDEXtpe =>
          DeBruijnIndex(readNat(), readNat())
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
        case LITERALbyte    => Constant(readLong(len).asInstanceOf[Byte])
        case LITERALshort   => Constant(readLong(len).asInstanceOf[Short])
        case LITERALchar    => Constant(readLong(len).asInstanceOf[Char])
        case LITERALint     => Constant(readLong(len).asInstanceOf[Int])
        case LITERALlong    => Constant(readLong(len))
        case LITERALfloat   => Constant(Float.intBitsToFloat(readLong(len).asInstanceOf[Int]))
        case LITERALdouble  => Constant(Double.longBitsToDouble(readLong(len)))
        case LITERALstring  => Constant(readNameRef().toString())
        case LITERALnull    => Constant(null)
        case LITERALclass   => Constant(readTypeRef())
        case _              => errorBadSignature("bad constant tag: " + tag)
      }
    }

    /** Read an annotation argument.  It can be either a Constant or
     *  a reflect.Tree.
     */
    private def readAnnotationArg(): AnnotationArgument = {
      if (peekByte() == REFLTREE) {
        val tree = readReflTree()
        new AnnotationArgument(tree)
      } else {
        val const = readConstant()
        new AnnotationArgument(const)
      }
    }

    /** Read an attribute and store in referenced symbol */
    private def readAnnotation(): AnyRef = {
      val tag = readByte()
      val end = readNat() + readIndex
      val target = readSymbolRef()
      if (tag == ATTRIBUTE) {
        val attrType = readTypeRef()
        val args = new ListBuffer[AnnotationArgument]
        val assocs = new ListBuffer[(Name, AnnotationArgument)]
        while (readIndex != end) {
          val argref = readNat()
          if (isNameEntry(argref))
            assocs += (at(argref, readName), readAnnotationArgRef)
          else
            args += at(argref, readAnnotationArg)
        }
        val attr = AnnotationInfo(attrType, args.toList, assocs.toList)
        target.attributes = attr :: target.attributes
      } else if (tag == CHILDREN) {
        while (readIndex != end) target addChild readSymbolRef()
      }
      null
    }

    /** Read a reflect.Tree */
    private def readReflTree(): reflect.Tree = {
      val outerTag = readByte()
      if(outerTag != REFLTREE)
        errorBadSignature("reflection tree expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readByte()
      tag match {
         case IDENTtree =>
          val sym = readReflSymbolRef()
          reflect.Ident(sym)
        case SELECTtree =>
          val qual = readReflTreeRef()
          val sym = readReflSymbolRef()
          reflect.Select(qual, sym)
        case LITERALtree =>
          val value = readConstantRef()
          reflect.Literal(value)
        case APPLYtree =>
          val fun = readReflTreeRef()
          val args = until(end, readReflTreeRef)
          reflect.Apply(fun, args)
        case TYPEAPPLYtree =>
          val fun = readReflTreeRef()
          val args = until(end, readReflTypeRef)
          reflect.TypeApply(fun, args)
        case FUNCTIONtree =>
          val body = readReflTreeRef()
          val params = until(end, readReflSymbolRef)
          reflect.Function(params, body)
        case THIStree =>
          val sym = readReflSymbolRef()
          reflect.This(sym)
        case BLOCKtree =>
          val expr = readReflTreeRef()
          val stats = until(end, readReflTreeRef)
          reflect.Block(stats, expr)
        case NEWtree =>
          val clz = readReflTreeRef()
          reflect.New(clz)
        case IFtree =>
          val condition = readReflTreeRef()
          val trueCase = readReflTreeRef()
          val falseCase = readReflTreeRef()
          reflect.If(condition, trueCase, falseCase)
        case ASSIGNtree =>
          val destination = readReflTreeRef()
          val source = readReflTreeRef()
          reflect.Assign(destination, source)
        case TARGETtree =>
          val sym = readReflSymbolRef()
          val body = readReflTreeRef()
          sym match {
            case sym:reflect.LabelSymbol => reflect.Target(sym, body)
            case _ => errorBadSignature("bad label for target: " + sym)
          }
        case GOTOtree =>
          val target = readReflSymbolRef()
          target match {
            case target:reflect.LabelSymbol => reflect.Goto(target)
            case _ => errorBadSignature("bad target for goto: " + target)
          }
        case VALDEFtree =>
          val sym = readReflSymbolRef()
          val rhs = readReflTreeRef()
          reflect.ValDef(sym, rhs)
        case CLASSDEFtree =>
          val sym = readReflSymbolRef()
          val tpe = readReflTypeRef()
          val impl = readReflTreeRef()
          impl match {
            case impl:reflect.Template => reflect.ClassDef(sym, tpe, impl)
            case _ =>
              errorBadSignature("body of class not a template: " + impl)
          }
        case tag =>
          errorBadSignature("unknown reflection tree (" + tag + ")")
      }
    }

    /** Read a reflect.Symbol */
    private def readReflSymbol(): reflect.Symbol = {
      val outerTag = readByte()
      if(outerTag != REFLSYM)
        errorBadSignature("reflection symbol expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readNat()
      tag match {
        case CLASSrsym =>
          val fullname = readConstantRef().stringValue
          reflect.Class(fullname)
        case METHODrsym =>
          val fullname = readConstantRef().stringValue
          val tpe = readReflTypeRef()
          reflect.Method(fullname, tpe)
        case FIELDrsym =>
          val fullname = readConstantRef().stringValue
          val tpe = readReflTypeRef()
          reflect.Field(fullname, tpe)
        case TYPEFIELDrsym =>
          val fullname = readConstantRef().stringValue
          val tpe = readReflTypeRef()
          reflect.TypeField(fullname, tpe)
        case LOCALVALUErsym =>
          val owner = readReflSymbolRef()
          val name = readConstantRef().stringValue
          val tpe = readReflTypeRef()
          reflect.LocalValue(owner, name, tpe)
        case LOCALMETHODrsym =>
          val owner = readReflSymbolRef()
          val name = readConstantRef().stringValue
          val tpe = readReflTypeRef()
          reflect.LocalMethod(owner, name, tpe)
        case NOSYMBOLrsym =>
          reflect.NoSymbol
        case ROOTSYMBOLrsym =>
          reflect.RootSymbol
        case LABELSYMBOLrsym =>
          val name = readConstantRef().stringValue
          reflect.LabelSymbol(name)
        case tag =>
          errorBadSignature("unknown reflection symbol (" + tag + ")")
      }
    }

    /** Read a reflect.Type */
    private def readReflType(): reflect.Type = {
      val outerTag = readByte()
      if(outerTag != REFLTYPE)
        errorBadSignature("reflection type expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readNat()
      tag match {
        case NOPREFIXrtpe =>
          reflect.NoPrefix
        case NOrtpe =>
          reflect.NoType
        case NAMEDrtpe =>
          val fullname = readConstantRef()
          reflect.NamedType(fullname.stringValue)
        case PREFIXEDrtpe =>
          val pre = readReflTypeRef()
          val sym = readReflSymbolRef()
          reflect.PrefixedType(pre, sym)
        case SINGLErtpe =>
          val pre = readReflTypeRef()
          val sym = readReflSymbolRef()
          reflect.SingleType(pre, sym)
        case THISrtpe =>
          val clazz = readReflSymbolRef()
          reflect.ThisType(clazz)
        case APPLIEDrtpe =>
          val tpe = readReflTypeRef()
          val args = until(end, readReflTypeRef)
          reflect.AppliedType(tpe, args)
        case TYPEBOUNDSrtpe =>
          val lo = readReflTypeRef()
          val hi = readReflTypeRef()
          reflect.TypeBounds(lo, hi)
        case IMPLICITMETHODrtpe =>
          val restpe = readReflTypeRef()
          val formals = until(end, readReflTypeRef)
          new reflect.ImplicitMethodType(formals, restpe)
        case METHODrtpe =>
          val restpe = readReflTypeRef()
          val formals = until(end, readReflTypeRef)
          reflect.MethodType(formals, restpe)
        case POLYrtpe =>
          val resultType = readReflTypeRef()
          val numBounds = readNat()
          val typeBounds = times(numBounds, {() =>
            val lo = readReflTypeRef()
            val hi = readReflTypeRef()
            (lo,hi)
          })
          val typeParams = until(end, readReflSymbolRef)
          reflect.PolyType(typeParams, typeBounds, resultType)
        case tag =>
          errorBadSignature("unknown reflection type (" + tag + ")")
      }
    }

    /** Read an annotation with reflect.Tree's */
    private def readTreeAttrib(): AnnotationInfo = {
      val tag = readByte()
      if(tag != ATTRIBTREE)
        errorBadSignature("tree-based annotation expected (" + tag + ")")
      val end = readNat() + readIndex

      val target = readTypeRef()
      val numargs = readNat()
      val args = times(numargs, readAnnotationArgRef)
      val assocs =
        until(end, {() =>
          val name = readNameRef()
          val tree = readAnnotationArgRef()
          (name,tree)})
      AnnotationInfo(target, args, assocs)
    }

    /* Read a reference to a pickled item */
    private def readNameRef(): Name = at(readNat(), readName)
    private def readSymbolRef(): Symbol = at(readNat(), readSymbol)
    private def readTypeRef(): Type = at(readNat(), readType)
    private def readConstantRef(): Constant = at(readNat(), readConstant)
    private def readAnnotationArgRef(): AnnotationArgument =
      at(readNat(), readAnnotationArg)
    private def readReflTreeRef(): reflect.Tree =
      at(readNat(), readReflTree)
    private def readReflSymbolRef(): reflect.Symbol =
      at(readNat(), readReflSymbol)
    private def readReflTypeRef(): reflect.Type =
      at(readNat(), readReflType)
    private def readTreeAttribRef(): AnnotationInfo =
      at(readNat(), readTreeAttrib)

    private def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    private class LazyTypeRef(i: Int) extends LazyType {
      private val definedAtRunId = currentRunId
      override def complete(sym: Symbol) {
        val tp = at(i, readType)
        sym setInfo tp
        if (currentRunId != definedAtRunId) sym.setInfo(adaptToNewRunMap(tp))
      }
      override def load(sym: Symbol) { complete(sym) }
    }

    private class LazyTypeRefAndAlias(i: Int, j: Int) extends LazyTypeRef(i) {
      override def complete(sym: Symbol) {
        super.complete(sym)
        var alias = at(j, readSymbol)
        if (alias hasFlag OVERLOADED)
          alias = alias suchThat (alt => sym.tpe =:= sym.owner.thisType.memberType(alt))
        sym.asInstanceOf[TermSymbol].setAlias(alias)
      }
    }
  }
}
