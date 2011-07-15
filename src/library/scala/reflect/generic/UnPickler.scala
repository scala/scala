/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package generic

import java.io.IOException
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import Flags._
import PickleFormat._
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import annotation.switch

/** @author Martin Odersky
 *  @version 1.0
 */
@deprecated("scala.reflect.generic will be removed", "2.9.1")
abstract class UnPickler {

  val global: Universe
  import global._

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param bytes      bytearray from which we unpickle
   *  @param offset     offset from which unpickling starts
   *  @param classroot  the top-level class which is unpickled, or NoSymbol if inapplicable
   *  @param moduleroot the top-level module which is unpickled, or NoSymbol if inapplicable
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) {
    try {
      scan(bytes, offset, classRoot, moduleRoot, filename)
    } catch {
      case ex: IOException =>
        throw ex
      case ex: Throwable =>
        /*if (settings.debug.value)*/ ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  /** To be implemented in subclasses. Like `unpickle` but without the catch-all error handling.
   */
  def scan(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String)

  abstract class Scan(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) extends PickleBuffer(bytes, offset, -1) {
    //println("unpickle " + classRoot + " and " + moduleRoot)//debug

    protected def debug = false

    checkVersion()

    /** A map from entry numbers to array offsets */
    private val index = createIndex

    /** A map from entry numbers to symbols, types, or annotations */
    private val entries = new Array[AnyRef](index.length)

    /** A map from symbols to their associated `decls` scopes */
    private val symScopes = new mutable.HashMap[Symbol, Scope]

    //println("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug

    def run() {
      // read children last, fix for #3951
      val queue = new ListBuffer[() => Unit]()
      def delay(i: Int, action: => Unit) {
        queue += (() => at(i, {() => action; null}))
      }

      for (i <- 0 until index.length) {
        if (isSymbolEntry(i))
          at(i, readSymbol)
        else if (isSymbolAnnotationEntry(i))
          delay(i, readSymbolAnnotation())
        else if (isChildrenEntry(i))
          delay(i, readChildren())
      }

      for (action <- queue)
        action()
    }

    private def checkVersion() {
      val major = readNat()
      val minor = readNat()
      if (major != MajorVersion || minor > MinorVersion)
        throw new IOException("Scala signature " + classRoot.decodedName +
                              " has wrong version\n expected: " +
                              MajorVersion + "." + MinorVersion +
                              "\n found: " + major + "." + minor +
                              " in "+filename)
    }

    /** The `decls` scope associated with given symbol */
    protected def symScope(sym: Symbol) = symScopes.getOrElseUpdate(sym, newScope)

    /** Does entry represent an (internal) symbol */
    protected def isSymbolEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      (firstSymTag <= tag && tag <= lastSymTag &&
       (tag != CLASSsym || !isRefinementSymbolEntry(i)))
    }

    /** Does entry represent an (internal or external) symbol */
    protected def isSymbolRef(i: Int): Boolean = {
      val tag = bytes(index(i))
      (firstSymTag <= tag && tag <= lastExtSymTag)
    }

    /** Does entry represent a name? */
    protected def isNameEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == TERMname || tag == TYPEname
    }

    /** Does entry represent a symbol annotation? */
    protected def isSymbolAnnotationEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == SYMANNOT
    }

    /** Does the entry represent children of a symbol? */
    protected def isChildrenEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == CHILDREN
    }

    /** Does entry represent a refinement symbol?
     *  pre: Entry is a class symbol
     */
    protected def isRefinementSymbolEntry(i: Int): Boolean = {
      val savedIndex = readIndex
      readIndex = index(i)
      val tag = readByte().toInt
      assert(tag == CLASSsym)

      readNat(); // read length
      val result = readNameRef() == tpnme.REFINE_CLASS_NAME
      readIndex = savedIndex
      result
    }

    /** If entry at `i` is undefined, define it by performing operation `op`
     *  with `readIndex` at start of `i`'th entry. Restore `readIndex`
     *  afterwards.
     */
    protected def at[T <: AnyRef](i: Int, op: () => T): T = {
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
    protected def readName(): Name = {
      val tag = readByte()
      val len = readNat()
      tag match {
        case TERMname => newTermName(bytes, readIndex, len)
        case TYPEname => newTypeName(bytes, readIndex, len)
        case _ => errorBadSignature("bad name tag: " + tag)
      }
    }

    /** Read a symbol */
    protected def readSymbol(): Symbol = {
      val tag   = readByte()
      val end   = readNat() + readIndex
      def atEnd = readIndex == end

      def readExtSymbol(): Symbol = {
        val name  = readNameRef()
        val owner = if (atEnd) definitions.RootClass else readSymbolRef()

        def fromName(name: Name) = mkTermName(name) match {
          case nme.ROOT     => definitions.RootClass
          case nme.ROOTPKG  => definitions.RootPackage
          case _            =>
            val s = owner.info.decl(name)
            if (tag == EXTref) s else s.moduleClass
        }
        def nestedObjectSymbol: Symbol = {
          // If the owner is overloaded (i.e. a method), it's not possible to select the
          // right member, so return NoSymbol. This can only happen when unpickling a tree.
          // the "case Apply" in readTree() takes care of selecting the correct alternative
          //  after parsing the arguments.
          if (owner.isOverloaded)
            return NoSymbol

          if (tag == EXTMODCLASSref) {
            val moduleVar = owner.info.decl(nme.moduleVarName(name))
            if (moduleVar.isLazyAccessor)
              return moduleVar.lazyAccessor.lazyAccessor
          }
          NoSymbol
        }

        // (1) Try name.
        fromName(name) orElse {
          // (2) Try with expanded name.  Can happen if references to private
          // symbols are read from outside: for instance when checking the children
          // of a class.  See #1722.
          fromName(nme.expandedName(name, owner)) orElse {
            // (3) Try as a nested object symbol.
            nestedObjectSymbol orElse {
              // (4) Otherwise, fail.
              errorMissingRequirement(name, owner)
            }
          }
        }
      }

      tag match {
        case NONEsym                 => return NoSymbol
        case EXTref | EXTMODCLASSref => return readExtSymbol()
        case _                       => ()
      }

      // symbols that were pickled with Pickler.writeSymInfo
      val nameref      = readNat()
      val name         = at(nameref, readName)
      val owner        = readSymbolRef()
      val flags        = pickledToRawFlags(readLongNat())
      var inforef      = readNat()
      val privateWithin =
        if (!isSymbolRef(inforef)) NoSymbol
        else {
          val pw = at(inforef, readSymbol)
          inforef = readNat()
          pw
        }

      def isModuleFlag = (flags & MODULE) != 0L
      def isMethodFlag = (flags & METHOD) != 0L
      def isClassRoot  = (name == classRoot.name) && (owner == classRoot.owner)
      def isModuleRoot = (name == moduleRoot.name) && (owner == moduleRoot.owner)

      def finishSym(sym: Symbol): Symbol = {
        sym.flags         = flags & PickledFlags
        sym.privateWithin = privateWithin
        sym.info = (
          if (atEnd) {
            assert(!sym.isSuperAccessor, sym)
            newLazyTypeRef(inforef)
          }
          else {
            assert(sym.isSuperAccessor || sym.isParamAccessor, sym)
            newLazyTypeRefAndAlias(inforef, readNat())
          }
        )
        if (sym.owner.isClass && sym != classRoot && sym != moduleRoot &&
            !sym.isModuleClass && !sym.isRefinementClass && !sym.isTypeParameter && !sym.isExistentiallyBound)
          symScope(sym.owner) enter sym

        sym
      }

      finishSym(tag match {
        case TYPEsym  => owner.newAbstractType(mkTypeName(name))
        case ALIASsym => owner.newAliasType(mkTypeName(name))
        case CLASSsym =>
          val sym = (isClassRoot, isModuleFlag) match {
            case (true, true)   => moduleRoot.moduleClass
            case (true, false)  => classRoot
            case (false, true)  => owner.newModuleClass(mkTypeName(name))
            case (false, false) => owner.newClass(mkTypeName(name))
          }
          if (!atEnd)
            sym.typeOfThis = newLazyTypeRef(readNat())

          sym
        case MODULEsym =>
          val clazz = at(inforef, () => readType()).typeSymbol // after the NMT_TRANSITION period, we can leave off the () => ... ()
          if (isModuleRoot) moduleRoot
          else {
            val m = owner.newModule(name, clazz)
            clazz.sourceModule = m
            m
          }
        case VALsym =>
          if (isModuleRoot) { assert(false); NoSymbol }
          else if (isMethodFlag) owner.newMethod(name)
          else owner.newValue(name)

        case _ =>
          errorBadSignature("bad symbol tag: " + tag)
      })
    }

    /** Read a type
     *
     * @param forceProperType is used to ease the transition to NullaryMethodTypes (commentmarker: NMT_TRANSITION)
     *        the flag say that a type of kind * is expected, so that PolyType(tps, restpe) can be disambiguated to PolyType(tps, NullaryMethodType(restpe))
     *        (if restpe is not a ClassInfoType, a MethodType or a NullaryMethodType, which leaves TypeRef/SingletonType -- the latter would make the polytype a type constructor)
     */
    protected def readType(forceProperType: Boolean = false): Type = {
      val tag = readByte()
      val end = readNat() + readIndex
      (tag: @switch) match {
        case NOtpe =>
          NoType
        case NOPREFIXtpe =>
          NoPrefix
        case THIStpe =>
          ThisType(readSymbolRef())
        case SINGLEtpe =>
          SingleType(readTypeRef(), readSymbolRef()) // !!! was singleType
        case SUPERtpe =>
          val thistpe = readTypeRef()
          val supertpe = readTypeRef()
          SuperType(thistpe, supertpe)
        case CONSTANTtpe =>
          ConstantType(readConstantRef())
        case TYPEREFtpe =>
          val pre = readTypeRef()
          val sym = readSymbolRef()
          var args = until(end, readTypeRef)
          TypeRef(pre, sym, args)
        case TYPEBOUNDStpe =>
          TypeBounds(readTypeRef(), readTypeRef())
        case REFINEDtpe =>
          val clazz = readSymbolRef()
          RefinedType(until(end, readTypeRef), symScope(clazz), clazz)
        case CLASSINFOtpe =>
          val clazz = readSymbolRef()
          ClassInfoType(until(end, readTypeRef), symScope(clazz), clazz)
        case METHODtpe | IMPLICITMETHODtpe =>
          val restpe = readTypeRef()
          val params = until(end, readSymbolRef)
          // if the method is overloaded, the params cannot be determined (see readSymbol) => return NoType.
          // Only happen for trees, "case Apply" in readTree() takes care of selecting the correct
          // alternative after parsing the arguments.
          if (params.contains(NoSymbol) || restpe == NoType) NoType
          else MethodType(params, restpe)
        case POLYtpe =>
          val restpe = readTypeRef()
          val typeParams = until(end, readSymbolRef)
          if(typeParams nonEmpty) {
            // NMT_TRANSITION: old class files denoted a polymorphic nullary method as PolyType(tps, restpe), we now require PolyType(tps, NullaryMethodType(restpe))
            // when a type of kind * is expected (forceProperType is true), we know restpe should be wrapped in a NullaryMethodType (if it wasn't suitably wrapped yet)
            def transitionNMT(restpe: Type) = {
              val resTpeCls = restpe.getClass.toString // what's uglier than isInstanceOf? right! -- isInstanceOf does not work since the concrete types are defined in the compiler (not in scope here)
              if(forceProperType /*&& pickleformat < 2.9 */ && !(resTpeCls.endsWith("MethodType"))) { assert(!resTpeCls.contains("ClassInfoType"))
                  NullaryMethodType(restpe) }
                else restpe
            }
            PolyType(typeParams, transitionNMT(restpe))
          }
          else
            NullaryMethodType(restpe)
        case EXISTENTIALtpe =>
          val restpe = readTypeRef()
          ExistentialType(until(end, readSymbolRef), restpe)
        case ANNOTATEDtpe =>
          var typeRef = readNat()
          val selfsym = if (isSymbolRef(typeRef)) {
            val s = at(typeRef, readSymbol)
            typeRef = readNat()
            s
          } else NoSymbol // selfsym can go.
          val tp = at(typeRef, () => readType(forceProperType)) // NMT_TRANSITION
          val annots = until(end, readAnnotationRef)
          if (selfsym == NoSymbol) AnnotatedType(annots, tp, selfsym)
          else tp
        case _ =>
          noSuchTypeTag(tag, end)
      }
    }

    def noSuchTypeTag(tag: Int, end: Int): Type =
      errorBadSignature("bad type tag: " + tag)

    /** Read a constant */
    protected def readConstant(): Constant = {
      val tag = readByte().toInt
      val len = readNat()
      (tag: @switch) match {
        case LITERALunit    => Constant(())
        case LITERALboolean => Constant(readLong(len) != 0L)
        case LITERALbyte    => Constant(readLong(len).toByte)
        case LITERALshort   => Constant(readLong(len).toShort)
        case LITERALchar    => Constant(readLong(len).toChar)
        case LITERALint     => Constant(readLong(len).toInt)
        case LITERALlong    => Constant(readLong(len))
        case LITERALfloat   => Constant(intBitsToFloat(readLong(len).toInt))
        case LITERALdouble  => Constant(longBitsToDouble(readLong(len)))
        case LITERALstring  => Constant(readNameRef().toString())
        case LITERALnull    => Constant(null)
        case LITERALclass   => Constant(readTypeRef())
        case LITERALenum    => Constant(readSymbolRef())
        case _              => noSuchConstantTag(tag, len)
      }
    }

    def noSuchConstantTag(tag: Int, len: Int): Constant =
      errorBadSignature("bad constant tag: " + tag)

    /** Read children and store them into the corresponding symbol.
     */
    protected def readChildren() {
      val tag = readByte()
      assert(tag == CHILDREN)
      val end = readNat() + readIndex
      val target = readSymbolRef()
      while (readIndex != end) target addChild readSymbolRef()
    }

    /** Read an annotation argument, which is pickled either
     *  as a Constant or a Tree.
     */
    protected def readAnnotArg(i: Int): Tree = bytes(index(i)) match {
      case TREE => at(i, readTree)
      case _    =>
        val const = at(i, readConstant)
        Literal(const) setType const.tpe
    }

    /** Read a ClassfileAnnotArg (argument to a classfile annotation)
     */
    private def readArrayAnnot() = {
      readByte() // skip the `annotargarray` tag
      val end = readNat() + readIndex
      until(end, () => readClassfileAnnotArg(readNat())).toArray(classfileAnnotArgManifest)
    }
    protected def readClassfileAnnotArg(i: Int): ClassfileAnnotArg = bytes(index(i)) match {
      case ANNOTINFO     => NestedAnnotArg(at(i, readAnnotation))
      case ANNOTARGARRAY => at(i, () => ArrayAnnotArg(readArrayAnnot()))
      case _             => LiteralAnnotArg(at(i, readConstant))
    }

    /** Read an AnnotationInfo. Not to be called directly, use
     *  readAnnotation or readSymbolAnnotation
     */
    protected def readAnnotationInfo(end: Int): AnnotationInfo = {
      val atp = readTypeRef()
      val args = new ListBuffer[Tree]
      val assocs = new ListBuffer[(Name, ClassfileAnnotArg)]
      while (readIndex != end) {
        val argref = readNat()
        if (isNameEntry(argref)) {
          val name = at(argref, readName)
          val arg = readClassfileAnnotArg(readNat())
          assocs += ((name, arg))
        }
        else
          args += readAnnotArg(argref)
      }
      AnnotationInfo(atp, args.toList, assocs.toList)
    }

    /** Read an annotation and as a side effect store it into
     *  the symbol it requests. Called at top-level, for all
     *  (symbol, annotInfo) entries. */
    protected def readSymbolAnnotation() {
      val tag = readByte()
      if (tag != SYMANNOT)
        errorBadSignature("symbol annotation expected ("+ tag +")")
      val end = readNat() + readIndex
      val target = readSymbolRef()
      target.addAnnotation(readAnnotationInfo(end))
    }

    /** Read an annotation and return it. Used when unpickling
     *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg */
    protected def readAnnotation(): AnnotationInfo = {
      val tag = readByte()
      if (tag != ANNOTINFO)
        errorBadSignature("annotation expected (" + tag + ")")
      val end = readNat() + readIndex
      readAnnotationInfo(end)
    }

    /* Read an abstract syntax tree */
    protected def readTree(): Tree = {
      val outerTag = readByte()
      if (outerTag != TREE)
        errorBadSignature("tree expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readByte()
      val tpe = if (tag == EMPTYtree) NoType else readTypeRef()

      // Set by the three functions to follow.  If symbol is non-null
      // after the the new tree 't' has been created, t has its Symbol
      // set to symbol; and it always has its Type set to tpe.
      var symbol: Symbol = null
      var mods: Modifiers = null
      var name: Name = null

      /** Read a Symbol, Modifiers, and a Name */
      def setSymModsName() {
        symbol = readSymbolRef()
        mods = readModifiersRef()
        name = readNameRef()
      }
      /** Read a Symbol and a Name */
      def setSymName() {
        symbol = readSymbolRef()
        name = readNameRef()
      }
      /** Read a Symbol */
      def setSym() {
        symbol = readSymbolRef()
      }

      val t = tag match {
        case EMPTYtree =>
          EmptyTree

        case PACKAGEtree =>
          setSym()
          val pid = readTreeRef().asInstanceOf[RefTree]
          val stats = until(end, readTreeRef)
          PackageDef(pid, stats)

        case CLASStree =>
          setSymModsName()
          val impl = readTemplateRef()
          val tparams = until(end, readTypeDefRef)
          ClassDef(mods, mkTypeName(name), tparams, impl)

        case MODULEtree =>
          setSymModsName()
          ModuleDef(mods, name, readTemplateRef())

        case VALDEFtree =>
          setSymModsName()
          val tpt = readTreeRef()
          val rhs = readTreeRef()
          ValDef(mods, name, tpt, rhs)

        case DEFDEFtree =>
          setSymModsName()
          val tparams = times(readNat(), readTypeDefRef)
          val vparamss = times(readNat(), () => times(readNat(), readValDefRef))
          val tpt = readTreeRef()
          val rhs = readTreeRef()
          DefDef(mods, name, tparams, vparamss, tpt, rhs)

        case TYPEDEFtree =>
          setSymModsName()
          val rhs = readTreeRef()
          val tparams = until(end, readTypeDefRef)
          TypeDef(mods, mkTypeName(name), tparams, rhs)

        case LABELtree =>
          setSymName()
          val rhs = readTreeRef()
          val params = until(end, readIdentRef)
          LabelDef(name, params, rhs)

        case IMPORTtree =>
          setSym()
          val expr = readTreeRef()
          val selectors = until(end, () => {
            val from = readNameRef()
            val to = readNameRef()
            ImportSelector(from, -1, to, -1)
          })

          Import(expr, selectors)

        case TEMPLATEtree =>
          setSym()
          val parents = times(readNat(), readTreeRef)
          val self = readValDefRef()
          val body = until(end, readTreeRef)

          Template(parents, self, body)

        case BLOCKtree =>
          val expr = readTreeRef()
          val stats = until(end, readTreeRef)
          Block(stats, expr)

        case CASEtree =>
          val pat = readTreeRef()
          val guard = readTreeRef()
          val body = readTreeRef()
          CaseDef(pat, guard, body)

        case ALTERNATIVEtree =>
          Alternative(until(end, readTreeRef))

        case STARtree =>
          Star(readTreeRef())

        case BINDtree =>
          setSymName()
          Bind(name, readTreeRef())

        case UNAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          UnApply(fun, args)

        case ARRAYVALUEtree =>
          val elemtpt = readTreeRef()
          val trees = until(end, readTreeRef)
          ArrayValue(elemtpt, trees)

        case FUNCTIONtree =>
          setSym()
          val body = readTreeRef()
          val vparams = until(end, readValDefRef)
          Function(vparams, body)

        case ASSIGNtree =>
          val lhs = readTreeRef()
          val rhs = readTreeRef()
          Assign(lhs, rhs)

        case IFtree =>
          val cond = readTreeRef()
          val thenp = readTreeRef()
          val elsep = readTreeRef()
          If(cond, thenp, elsep)

        case MATCHtree =>
          val selector = readTreeRef()
          val cases = until(end, readCaseDefRef)
          Match(selector, cases)

        case RETURNtree =>
          setSym()
          Return(readTreeRef())

        case TREtree =>
          val block = readTreeRef()
          val finalizer = readTreeRef()
          val catches = until(end, readCaseDefRef)
          Try(block, catches, finalizer)

        case THROWtree =>
          Throw(readTreeRef())

        case NEWtree =>
          New(readTreeRef())

        case TYPEDtree =>
          val expr = readTreeRef()
          val tpt = readTreeRef()
          Typed(expr, tpt)

        case TYPEAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          TypeApply(fun, args)

        case APPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          if (fun.symbol.isOverloaded) {
            fun.setType(fun.symbol.info)
            inferMethodAlternative(fun, args map (_.tpe), tpe)
          }
          Apply(fun, args)

        case APPLYDYNAMICtree =>
          setSym()
          val qual = readTreeRef()
          val args = until(end, readTreeRef)
          ApplyDynamic(qual, args)

        case SUPERtree =>
          setSym()
          val qual = readTreeRef()
          val mix = readTypeNameRef()
          Super(qual, mix)

        case THIStree =>
          setSym()
          This(readTypeNameRef())

        case SELECTtree =>
          setSym()
          val qualifier = readTreeRef()
          val selector = readNameRef()
          Select(qualifier, selector)

        case IDENTtree =>
          setSymName()
          Ident(name)

        case LITERALtree =>
          Literal(readConstantRef())

        case TYPEtree =>
          TypeTree()

        case ANNOTATEDtree =>
          val annot = readTreeRef()
          val arg = readTreeRef()
          Annotated(annot, arg)

        case SINGLETONTYPEtree =>
          SingletonTypeTree(readTreeRef())

        case SELECTFROMTYPEtree =>
          val qualifier = readTreeRef()
          val selector = readTypeNameRef()
          SelectFromTypeTree(qualifier, selector)

        case COMPOUNDTYPEtree =>
          CompoundTypeTree(readTemplateRef())

        case APPLIEDTYPEtree =>
          val tpt = readTreeRef()
          val args = until(end, readTreeRef)
          AppliedTypeTree(tpt, args)

        case TYPEBOUNDStree =>
          val lo = readTreeRef()
          val hi = readTreeRef()
          TypeBoundsTree(lo, hi)

        case EXISTENTIALTYPEtree =>
          val tpt = readTreeRef()
          val whereClauses = until(end, readTreeRef)
          ExistentialTypeTree(tpt, whereClauses)

        case _ =>
          noSuchTreeTag(tag, end)
      }

      if (symbol == null) t setType tpe
      else t setSymbol symbol setType tpe
    }

    def noSuchTreeTag(tag: Int, end: Int) =
      errorBadSignature("unknown tree type (" + tag + ")")

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
      Modifiers(flags, privateWithin, Nil, Map.empty)
    }

    /* Read a reference to a pickled item */
    protected def readNameRef(): Name                 = at(readNat(), readName)
    protected def readSymbolRef(): Symbol             = at(readNat(), readSymbol)
    protected def readTypeRef(): Type                 = at(readNat(), () => readType()) // after the NMT_TRANSITION period, we can leave off the () => ... ()
    protected def readConstantRef(): Constant         = at(readNat(), readConstant)
    protected def readAnnotationRef(): AnnotationInfo = at(readNat(), readAnnotation)
    protected def readModifiersRef(): Modifiers       = at(readNat(), readModifiers)
    protected def readTreeRef(): Tree                 = at(readNat(), readTree)

    protected def readTypeNameRef(): TypeName         = mkTypeName(readNameRef())
    protected def readTermNameRef(): TermName         = mkTermName(readNameRef())

    protected def readTemplateRef(): Template =
      readTreeRef() match {
        case templ:Template => templ
        case other =>
          errorBadSignature("expected a template (" + other + ")")
      }
    protected def readCaseDefRef(): CaseDef =
      readTreeRef() match {
        case tree:CaseDef => tree
        case other =>
          errorBadSignature("expected a case def (" + other + ")")
      }
    protected def readValDefRef(): ValDef =
      readTreeRef() match {
        case tree:ValDef => tree
        case other =>
          errorBadSignature("expected a ValDef (" + other + ")")
      }
    protected def readIdentRef(): Ident =
      readTreeRef() match {
        case tree:Ident => tree
        case other =>
          errorBadSignature("expected an Ident (" + other + ")")
      }
    protected def readTypeDefRef(): TypeDef =
      readTreeRef() match {
        case tree:TypeDef => tree
        case other =>
          errorBadSignature("expected an TypeDef (" + other + ")")
      }

    protected def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    protected def errorMissingRequirement(msg: String): Nothing =
      if (debug) errorBadSignature(msg)
      else throw new IOException("class file needed by "+classRoot.name+" is missing.\n"+msg)

    protected def errorMissingRequirement(name: Name, owner: Symbol): Nothing =
      errorMissingRequirement("reference " + NameTransformer.decode(name.toString) + " of " + owner.tpe + " refers to nonexisting symbol.")

    /** pre: `fun` points to a symbol with an overloaded type.
     *  Selects the overloaded alternative of `fun` which best matches given
     *  argument types `argtpes` and result type `restpe`. Stores this alternative as
     *  the symbol of `fun`.
     */
    def inferMethodAlternative(fun: Tree, argtpes: List[Type], restpe: Type)

    /** Create a lazy type which when completed returns type at index `i`. */
    def newLazyTypeRef(i: Int): LazyType

    /** Create a lazy type which when completed returns type at index `i` and sets alias
     *  of completed symbol to symbol at index `j`
     */
    def newLazyTypeRefAndAlias(i: Int, j: Int): LazyType
  }
}
