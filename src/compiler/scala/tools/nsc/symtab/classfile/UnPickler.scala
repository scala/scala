/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import java.io.IOException
import java.lang.{Float, Double}

import scala.tools.nsc.util.{Position, NoPosition}
import scala.io.UTF8Codec

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

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param bytes      bytearray from which we unpickle
   *  @param offset     offset from which unpickling starts
   *  @param classroot  the top-level class which is unpickled, or NoSymbol if unapplicable
   *  @param moduleroot the top-level module which is unpickled, or NoSymbol if unapplicable
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) {
    try {
      new UnPickle(bytes, offset, classRoot, moduleRoot, filename)
    } catch {
      case ex: IOException =>
        throw ex
      case ex: Throwable =>
        if (settings.debug.value) ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  private class UnPickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) extends PickleBuffer(bytes, offset, -1) {
    if (settings.debug.value) global.log("unpickle " + classRoot + " and " + moduleRoot)
    checkVersion(filename)

    /** A map from entry numbers to array offsets */
    private val index = createIndex

    /** A map from entry numbers to symbols, types, or annotations */
    private val entries = new Array[AnyRef](index.length)

    /** A map from symbols to their associated `decls' scopes */
    private val symScopes = new HashMap[Symbol, Scope]

    for (i <- 0 until index.length) {
      if (isSymbolEntry(i))
        at(i, readSymbol)
      else if (isSymbolAnnotationEntry(i))
        at(i, {() => readSymbolAnnotation(); null})
      else if (isChildrenEntry(i))
        at(i, {() => readChildren(); null})
    }

    if (settings.debug.value) global.log("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug

    private def checkVersion(filename: String) {
      val major = readNat()
      val minor = readNat()
      if (major != MajorVersion || minor > MinorVersion)
        throw new IOException("Scala signature " + classRoot.name +
                              " has wrong version\n expected: " +
                              MajorVersion + "." + MinorVersion +
                              "\n found: " + major + "." + minor +
                              " in "+filename)
    }

    /** The `decls' scope associated with given symbol */
    private def symScope(sym: Symbol, isTemp : Boolean) = symScopes.get(sym) match {
      case None =>
        val s = if (isTemp) newTempScope
                else if (sym.isClass || sym.isModuleClass || sym.isModule) newClassScope(sym);
                else newScope

        symScopes(sym) = s; s
      case Some(s) => s
    }
    private def symScope(sym : Symbol) : Scope = symScope(sym, false)

    /** Does entry represent an (internal) symbol */
    private def isSymbolEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      (firstSymTag <= tag && tag <= lastSymTag &&
       (tag != CLASSsym || !isRefinementSymbolEntry(i)))
    }

    /** Does entry represent an (internal or external) symbol */
    private def isSymbolRef(i: Int): Boolean = {
      val tag = bytes(index(i))
      (firstSymTag <= tag && tag <= lastExtSymTag)
    }

    /** Does entry represent a name? */
    private def isNameEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      tag == TERMname || tag == TYPEname
    }

    /** Does entry represent a symbol annotation? */
    private def isSymbolAnnotationEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      tag == SYMANNOT
    }

    /** Does the entry represent children of a symbol? */
    private def isChildrenEntry(i: Int): Boolean = {
      val tag = bytes(index(i))
      tag == CHILDREN
    }

    /** Does entry represent a refinement symbol?
     *  pre: Entry is a class symbol
     */
    private def isRefinementSymbolEntry(i: Int): Boolean = {
      val savedIndex = readIndex
      readIndex = index(i)
      val tag = readByte()
      if (tag != CLASSsym) assert(false)
      readNat(); // read length
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

          // If the owner is overloaded (i.e. a method), it's not possible to select the
          // right member => return NoSymbol. This can only happen when unpickling a tree.
          // the "case Apply" in readTree() takes care of selecting the correct alternative
          //  after parsing the arguments.
          if (sym == NoSymbol && !owner.hasFlag(OVERLOADED)) {
            errorBadSignature(
              "reference " + (if (name.isTypeName) "type " else "value ") +
              name.decode + " of " + owner + " refers to nonexisting symbol.")
          }
        case NONEsym =>
          sym = NoSymbol
        case _ => // symbols that were pickled with Pickler.writeSymInfo
          var defaultGetter: Symbol = NoSymbol
          var nameref = readNat()
          if (tag == VALsym && isSymbolRef(nameref)) {
            defaultGetter = at(nameref, readSymbol)
            nameref = readNat()
          }
          val name = at(nameref, readName)
          val owner = readSymbolRef()
          val flags = pickledToRawFlags(readLongNat())
          var privateWithin: Symbol = NoSymbol
          var inforef = readNat()
          if (isSymbolRef(inforef)) {
            privateWithin = at(inforef, readSymbol)
            inforef = readNat()
          }
          tag match {
            case TYPEsym =>
              sym = owner.newAbstractType(NoPosition, name)
            case ALIASsym =>
              sym = owner.newAliasType(NoPosition, name)
            case CLASSsym =>
              sym =
                if (name == classRoot.name && owner == classRoot.owner)
                  (if ((flags & MODULE) != 0) moduleRoot.moduleClass
                   else classRoot)
                else
                  if ((flags & MODULE) != 0) owner.newModuleClass(NoPosition, name)
                  else owner.newClass(NoPosition, name)
              if (readIndex != end) sym.typeOfThis = new LazyTypeRef(readNat())
            case MODULEsym =>
              val clazz = at(inforef, readType).typeSymbol
              sym =
                if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot
                else {
                  assert(clazz.isInstanceOf[ModuleClassSymbol], clazz)
                  val mclazz = clazz.asInstanceOf[ModuleClassSymbol]
                  val m = owner.newModule(NoPosition, name, mclazz)
                  mclazz.setSourceModule(m)
                  m
                }
            case VALsym =>
              sym = if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot.resetFlag(MODULE)
                    else owner.newValue(NoPosition, name)
              sym.defaultGetter = defaultGetter
            case _ =>
              errorBadSignature("bad symbol tag: " + tag)
          }
          sym.setFlag(flags.toLong & PickledFlags)
          sym.privateWithin = privateWithin
          if (readIndex != end) assert(sym hasFlag (SUPERACCESSOR | PARAMACCESSOR), sym)
          if (sym hasFlag SUPERACCESSOR) assert(readIndex != end)
          sym.setInfo(
            if (readIndex != end) new LazyTypeRefAndAlias(inforef, readNat())
            else new LazyTypeRef(inforef))
          if (sym.owner.isClass && sym != classRoot && sym != moduleRoot &&
              !sym.isModuleClass && !sym.isRefinementClass && !sym.isTypeParameter && !sym.isExistential)
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
        case SUPERtpe =>
          val thistpe = readTypeRef()
          val supertpe = readTypeRef()
          SuperType(thistpe, supertpe)
        case CONSTANTtpe =>
          mkConstantType(readConstantRef())
        case TYPEREFtpe =>
          val pre = readTypeRef()
          val sym = readSymbolRef()
          var args = until(end, readTypeRef)
          rawTypeRef(pre, sym, args)
        case TYPEBOUNDStpe =>
          mkTypeBounds(readTypeRef(), readTypeRef())
        case REFINEDtpe =>
          val clazz = readSymbolRef()
/*
          val ps = until(end, readTypeRef)
          val dcls = symScope(clazz)
          new RefinedType(ps, dcls) { override def symbol = clazz }
*/
         new RefinedType(until(end, readTypeRef), symScope(clazz, true)) {
           override def typeSymbol = clazz
          }
        case CLASSINFOtpe =>
          val clazz = readSymbolRef()
          ClassInfoType(until(end, readTypeRef), symScope(clazz), clazz)
        case METHODtpe =>
          val restpe = readTypeRef()
          val params = until(end, readSymbolRef)
          // if the method is overloaded, the params cannot be determined (see readSymbol) => return NoType.
          // Only happen for trees, "case Apply" in readTree() takes care of selecting the correct
          // alternative after parsing the arguments.
          if (params.contains(NoSymbol) || restpe == NoType) NoType
          else MethodType(params, restpe)
        case IMPLICITMETHODtpe =>
          val restpe = readTypeRef()
          val params = until(end, readSymbolRef)
          ImplicitMethodType(params, restpe)
        case POLYtpe =>
          val restpe = readTypeRef()
          val typeParams = until(end, readSymbolRef)
          // see comment above in "case METHODtpe"
          if (typeParams.contains(NoSymbol) || restpe == NoType) NoType
          else PolyType(typeParams, restpe)
        case EXISTENTIALtpe =>
          val restpe = readTypeRef()
          ExistentialType(until(end, readSymbolRef), restpe)
        case ANNOTATEDtpe =>
          var typeRef = readNat()
          val selfsym = if (isSymbolRef(typeRef)) {
            val s = at(typeRef, readSymbol)
            typeRef = readNat()
            s
          } else NoSymbol
          val tp = at(typeRef, readType)
          val annots = until(end, readAnnotationRef)
          if (settings.selfInAnnots.value || (selfsym == NoSymbol))
            AnnotatedType(annots, tp, selfsym)
          else
            tp // drop annotations with a self symbol unless
               // -Yself-in-annots is on
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

    /** Read children and store them into the corresponding symbol.
     */
    private def readChildren() {
      val tag = readByte()
      assert(tag == CHILDREN)
      val end = readNat() + readIndex
      val target = readSymbolRef()
      while (readIndex != end) target addChild readSymbolRef()
    }

    /** Read an annotation argument. It can use either Constant's or
     *  Tree's for its arguments.
     */
    private def readAnnotationArg(): AnnotationArgument = {
      if (peekByte() == TREE) {
        val tree = readTree()
        new AnnotationArgument(tree)
      } else {
        val const = readConstant()
        new AnnotationArgument(const)
      }
    }

    /** Read a ConstantAnnotationArgument (argument to a java annotation)
     */
    private def readConstantAnnotationArg(): ConstantAnnotationArgument = {
      val b = peekByte()
      if (peekByte() == ANNOTINFO) {
        NestedAnnotationArgument(readAnnotation())
      } else if (peekByte() == ANNOTARGARRAY) {
        readByte()
        val end = readNat() + readIndex
        ArrayAnnotationArgument(until(end, readConstantAnnotationArgRef).toArray)
      } else {
        LiteralAnnotationArgument(readConstant())
      }
    }

    /** Read an AnnotationInfo. Not to be called directly, use
     *  readAnnotation or readSymbolAnnotation
     */
    private def readAnnotationInfo(end: Int): AnnotationInfo = {
      val atp = readTypeRef()
      //val args = until(end, readAnnotationArgRef)
      //AnnotationInfo(atp, args, List())
      val args = new ListBuffer[AnnotationArgument]
      val assocs = new ListBuffer[(Name, ConstantAnnotationArgument)]
      while (readIndex != end) {
        val argref = readNat()
        if (isNameEntry(argref))
          assocs += ((at(argref, readName), readConstantAnnotationArgRef))
        else
          args += at(argref, readAnnotationArg)
      }
      AnnotationInfo(atp, args.toList, assocs.toList)
    }


    /** Read an annotation and as a side effect store it into
     *  the symbol it requests. Called at top-level, for all
     *  (symbol, annotInfo) entries. */
    private def readSymbolAnnotation() {
      val tag = readByte()
      if (tag != SYMANNOT)
        errorBadSignature("symbol annotation expected ("+ tag +")")
      val end = readNat() + readIndex
      val target = readSymbolRef()
      target.addAnnotation(readAnnotationInfo(end))
    }

    /** Read an annotation and return it. Only called when
     *  unpickling an ANNOTATED(WSELF)tpe. */
    private def readAnnotation(): AnnotationInfo = {
      val tag = readByte()
      if (tag != ANNOTINFO)
        errorBadSignature("annotation expected (" + tag + ")")
      val end = readNat() + readIndex
      readAnnotationInfo(end)
    }

    /* Read an abstract syntax tree */
    private def readTree(): Tree = {
      val outerTag = readByte()
      if (outerTag != TREE)
        errorBadSignature("tree expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readByte()
      val tpe =
        if (tag != EMPTYtree)
          readTypeRef()
        else
          NoType

      tag match {
        case EMPTYtree =>
          EmptyTree

        case PACKAGEtree =>
          val symbol = readSymbolRef()
          val name = readNameRef()
          val stats = until(end, readTreeRef)
          PackageDef(name, stats) setType tpe

        case CLASStree =>
          val symbol = readSymbolRef()
          val mods = readModifiersRef()
          val name = readNameRef()
          val impl = readTemplateRef()
          val tparams = until(end, readTypeDefRef)
          (ClassDef(mods, name, tparams, impl).
           setSymbol(symbol).
           setType(tpe))

        case MODULEtree =>
          val symbol = readSymbolRef()
          val mods = readModifiersRef()
          val name = readNameRef()
          val impl = readTemplateRef()
          (ModuleDef(mods, name, impl).
           setSymbol(symbol).
           setType(tpe))

        case VALDEFtree =>
          val symbol = readSymbolRef()
          val mods = readModifiersRef()
          val name = readNameRef()
          val tpt = readTreeRef()
          val rhs = readTreeRef()

          (ValDef(mods, name, tpt, rhs).
           setSymbol(symbol).
           setType(tpe))

        case DEFDEFtree =>
          val symbol = readSymbolRef()
          val mods = readModifiersRef()
          val name = readNameRef()
          val numTparams = readNat()
          val tparams = times(numTparams, readTypeDefRef)
          val numVparamss = readNat
          val vparamss = times(numVparamss, () => {
            val len = readNat()
            times(len, readValDefRef)})
          val tpt = readTreeRef()
          val rhs = readTreeRef()

          (DefDef(mods, name, tparams, vparamss, tpt, rhs).
           setSymbol(symbol).
           setType(tpe))

        case TYPEDEFtree =>
          val symbol = readSymbolRef()
          val mods = readModifiersRef()
          val name = readNameRef()
          val rhs = readTreeRef()
          val tparams = until(end, readTypeDefRef)

          (TypeDef(mods, name, tparams, rhs).
           setSymbol(symbol).
           setType(tpe))

        case LABELtree =>
          val symbol = readSymbolRef()
          val name = readNameRef()
          val rhs = readTreeRef()
          val params = until(end, readIdentRef)
          (LabelDef(name, params, rhs).
           setSymbol(symbol).
           setType(tpe))

        case IMPORTtree =>
          val symbol = readSymbolRef()
          val expr = readTreeRef()
          val selectors = until(end, () => {
            val from = readNameRef()
            val to = readNameRef()
            (from, to)
          })
          (Import(expr, selectors).
           setSymbol(symbol).
           setType(tpe))

        case DOCDEFtree =>
          val comment = readConstantRef match {
            case Constant(com: String) => com
            case other =>
              errorBadSignature("Document comment not a string (" + other + ")")
          }
          val definition = readTreeRef()
          (DocDef(comment, definition).setType(tpe))

        case TEMPLATEtree =>
          val symbol = readSymbolRef()
          val numParents = readNat()
          val parents = times(numParents, readTreeRef)
          val self = readValDefRef()
          val body = until(end, readTreeRef)

          (Template(parents, self, body).
           setSymbol(symbol).
           setType(tpe))

        case BLOCKtree =>
          val expr = readTreeRef()
          val stats = until(end, readTreeRef)
          Block(stats, expr).setType(tpe)

        case CASEtree =>
          val pat = readTreeRef()
          val guard = readTreeRef()
          val body = readTreeRef()
          CaseDef(pat, guard, body).setType(tpe)

        case SEQUENCEtree =>
          val trees = until(end, readTreeRef)
          Sequence(trees).setType(tpe)

        case ALTERNATIVEtree =>
          val trees = until(end, readTreeRef)
          Alternative(trees).setType(tpe)

        case STARtree =>
          val elem = readTreeRef()
          Star(elem).setType(tpe)

        case BINDtree =>
          val symbol = readSymbolRef()
          val name = readNameRef()
          val body = readTreeRef()
          (Bind(name, body).
           setSymbol(symbol).
           setType(tpe))

        case UNAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          (UnApply(fun: Tree, args).setType(tpe))

        case ARRAYVALUEtree =>
          val elemtpt = readTreeRef()
          val trees = until(end, readTreeRef)
          (ArrayValue(elemtpt, trees).setType(tpe))

        case FUNCTIONtree =>
          val symbol = readSymbolRef()
          val body = readTreeRef()
          val vparams = until(end, readValDefRef)
          (Function(vparams, body).
           setSymbol(symbol).
           setType(tpe))

        case ASSIGNtree =>
          val lhs = readTreeRef()
          val rhs = readTreeRef()
          Assign(lhs, rhs).setType(tpe)

        case IFtree =>
          val cond = readTreeRef()
          val thenp = readTreeRef()
          val elsep = readTreeRef()
          If(cond, thenp, elsep).setType(tpe)

        case MATCHtree =>
          val selector = readTreeRef()
          val cases = until(end, readCaseDefRef)
          Match(selector, cases).setType(tpe)

        case RETURNtree =>
          val symbol = readSymbolRef()
          val expr = readTreeRef()
          (Return(expr).
           setSymbol(symbol).
           setType(tpe))

        case TREtree =>
          val block = readTreeRef()
          val finalizer = readTreeRef()
          val catches = until(end, readCaseDefRef)
          Try(block, catches, finalizer).setType(tpe)

        case THROWtree =>
          val expr = readTreeRef()
          Throw(expr).setType(tpe)

        case NEWtree =>
          val tpt = readTreeRef()
          New(tpt).setType(tpe)

        case TYPEDtree =>
          val expr = readTreeRef()
          val tpt = readTreeRef()
          Typed(expr, tpt).setType(tpe)

        case TYPEAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          TypeApply(fun, args).setType(tpe)

        case APPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          if (fun.symbol hasFlag OVERLOADED) {
            fun.setType(fun.symbol.info)
            typer.infer.inferMethodAlternative(fun, Nil, args map (_.tpe), tpe)
          }
          Apply(fun, args).setType(tpe)

        case APPLYDYNAMICtree =>
          val symbol = readSymbolRef()
          val qual = readTreeRef()
          val args = until(end, readTreeRef)
          ApplyDynamic(qual, args).setSymbol(symbol).setType(tpe)

        case SUPERtree =>
          val symbol = readSymbolRef()
          val qual = readNameRef()
          val mix = readNameRef()
          Super(qual, mix).setSymbol(symbol).setType(tpe)

        case THIStree =>
          val symbol = readSymbolRef()
          val qual = readNameRef()
          This(qual).setSymbol(symbol).setType(tpe)

        case SELECTtree =>
          val symbol = readSymbolRef()
          val qualifier = readTreeRef()
          val selector = readNameRef()
          Select(qualifier, selector).setSymbol(symbol).setType(tpe)

        case IDENTtree =>
          val symbol = readSymbolRef()
          val name = readNameRef()
          Ident(name).setSymbol(symbol).setType(tpe)

        case LITERALtree =>
          val value = readConstantRef()
          Literal(value).setType(tpe)

        case TYPEtree =>
          TypeTree().setType(tpe)

        case ANNOTATEDtree =>
          val annot = readTreeRef()
          val arg = readTreeRef()
          Annotated(annot, arg).setType(tpe)

        case SINGLETONTYPEtree =>
          val ref = readTreeRef()
          SingletonTypeTree(ref).setType(tpe)

        case SELECTFROMTYPEtree =>
          val qualifier = readTreeRef()
          val selector = readNameRef()
          SelectFromTypeTree(qualifier, selector).setType(tpe)

        case COMPOUNDTYPEtree =>
          val templ = readTemplateRef()
          CompoundTypeTree(templ: Template).setType(tpe)

        case APPLIEDTYPEtree =>
          val tpt = readTreeRef()
          val args = until(end, readTreeRef)
          AppliedTypeTree(tpt, args).setType(tpe)

        case TYPEBOUNDStree =>
          val lo = readTreeRef()
          val hi = readTreeRef()
          TypeBoundsTree(lo, hi).setType(tpe)

        case EXISTENTIALTYPEtree =>
          val tpt = readTreeRef()
          val whereClauses = until(end, readTreeRef)
          ExistentialTypeTree(tpt, whereClauses).setType(tpe)

        case _ =>
          errorBadSignature("unknown tree type (" + tag + ")")
      }
    }

    def readModifiers(): Modifiers = {
      val tag = readNat()
      if (tag != MODIFIERS)
        errorBadSignature("expected a modifiers tag (" + tag + ")")
      val end = readNat() + readIndex
      val pflagsHi = readNat()
      val pflagsLo = readNat()
      val pflags = (pflagsHi.toLong << 32) + pflagsLo
      val flags = pickledToRawFlags(pflags)
      val privateWithin = readNameRef()
      Modifiers(flags, privateWithin, Nil)
    }

    /* Read a reference to a pickled item */
    private def readNameRef(): Name = at(readNat(), readName)
    private def readSymbolRef(): Symbol = at(readNat(), readSymbol)
    private def readTypeRef(): Type = at(readNat(), readType)
    private def readConstantRef(): Constant = at(readNat(), readConstant)
    private def readAnnotationArgRef(): AnnotationArgument =
      at(readNat(), readAnnotationArg)
    private def readConstantAnnotationArgRef(): ConstantAnnotationArgument =
      at(readNat(), readConstantAnnotationArg)
    private def readAnnotationRef(): AnnotationInfo =
      at(readNat(), readAnnotation)
    private def readModifiersRef(): Modifiers =
      at(readNat(), readModifiers)
    private def readTreeRef(): Tree =
      at(readNat(), readTree)

    private def readTemplateRef(): Template =
      readTreeRef() match {
        case templ:Template => templ
        case other =>
          errorBadSignature("expected a template (" + other + ")")
      }
    private def readCaseDefRef(): CaseDef =
      readTreeRef() match {
        case tree:CaseDef => tree
        case other =>
          errorBadSignature("expected a case def (" + other + ")")
      }
    private def readValDefRef(): ValDef =
      readTreeRef() match {
        case tree:ValDef => tree
        case other =>
          errorBadSignature("expected a ValDef (" + other + ")")
      }
    private def readIdentRef(): Ident =
      readTreeRef() match {
        case tree:Ident => tree
        case other =>
          errorBadSignature("expected an Ident (" + other + ")")
      }
    private def readTypeDefRef(): TypeDef =
      readTreeRef() match {
        case tree:TypeDef => tree
        case other =>
          errorBadSignature("expected an TypeDef (" + other + ")")
      }


    private def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    private class LazyTypeRef(i: Int) extends LazyType {
      private val definedAtRunId = currentRunId
      // In IDE, captures class files dependencies so they can be reloaded when their dependencies change.
      private val ideHook = unpickleIDEHook
      override def complete(sym: Symbol) : Unit = {
        val tp = ideHook(at(i, readType))
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
