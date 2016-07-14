/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal
package pickling

import java.io.IOException
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import Flags._
import PickleFormat._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.annotation.switch

/** @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler {
  val symbolTable: SymbolTable
  import symbolTable._

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param bytes      bytearray from which we unpickle
   *  @param offset     offset from which unpickling starts
   *  @param classRoot  the top-level class which is unpickled, or NoSymbol if inapplicable
   *  @param moduleRoot the top-level module which is unpickled, or NoSymbol if inapplicable
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) {
    try {
      new Scan(bytes, offset, classRoot, moduleRoot, filename).run()
    } catch {
      case ex: IOException =>
        throw ex
      case ex: MissingRequirementError =>
        throw ex
      case ex: Throwable =>
        /*if (settings.debug.value)*/ ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  class Scan(_bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) extends PickleBuffer(_bytes, offset, -1) {
    //println("unpickle " + classRoot + " and " + moduleRoot)//debug

    protected def debug = settings.debug.value

    checkVersion()

    private val loadingMirror = mirrorThatLoaded(classRoot)

    /** A map from entry numbers to array offsets */
    private val index = createIndex

    /** A map from entry numbers to symbols, types, or annotations */
    private val entries = new Array[AnyRef](index.length)

    /** A map from symbols to their associated `decls` scopes */
    private val symScopes = mutable.HashMap[Symbol, Scope]()

    private def expect(expected: Int, msg: => String) {
      val tag = readByte()
      if (tag != expected)
        errorBadSignature(s"$msg ($tag)")
    }

    //println("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug

    @inline private def runAtIndex[T](i: Int)(body: => T): T = {
      val saved = readIndex
      readIndex = index(i)
      try body finally readIndex = saved
    }

    // Laboriously unrolled for performance.
    def run() {
      var i = 0
      while (i < index.length) {
        if (entries(i) == null && isSymbolEntry(i))
          runAtIndex(i)(entries(i) = readSymbol())

        i += 1
      }

      // read children last, fix for #3951
      i = 0
      while (i < index.length) {
        if (entries(i) == null) {
          if (isSymbolAnnotationEntry(i))
            runAtIndex(i)(readSymbolAnnotation())
          else if (isChildrenEntry(i))
            runAtIndex(i)(readChildren())
        }
        i += 1
      }
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

    private def maybeReadSymbol(): Either[Int, Symbol] = readNat() match {
      case index if isSymbolRef(index) => Right(at(index, readSymbol))
      case index                       => Left(index)
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

    /** If entry at `i` is undefined, define it by performing
     *  operation `op` with `readIndex` at start of i'th
     *  entry. Restore `readIndex` afterwards.
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
    private def readEnd() = readNat() + readIndex

    /** Read a symbol */
    protected def readSymbol(): Symbol = {
      val tag   = readByte()
      val end   = readEnd()
      def atEnd = readIndex == end

      def readExtSymbol(): Symbol = {
        val name  = readNameRef()
        val owner = if (atEnd) loadingMirror.RootClass else readSymbolRef()

        def adjust(sym: Symbol) = if (tag == EXTref) sym else sym.moduleClass

        def fromName(name: Name) = name.toTermName match {
          case nme.ROOT     => loadingMirror.RootClass
          case nme.ROOTPKG  => loadingMirror.RootPackage
          case _            =>
            val decl = owner match {
              case stub: StubSymbol => NoSymbol // SI-8502 Don't call .info and fail the stub
              case _ => owner.info.decl(name)
            }
            adjust(decl)
        }
        def nestedObjectSymbol: Symbol = {
          // If the owner is overloaded (i.e. a method), it's not possible to select the
          // right member, so return NoSymbol. This can only happen when unpickling a tree.
          // the "case Apply" in readTree() takes care of selecting the correct alternative
          //  after parsing the arguments.
          if (owner.isOverloaded)
            return NoSymbol

          if (tag == EXTMODCLASSref) {
            owner.info.decl(nme.moduleVarName(name.toTermName))
          }
          NoSymbol
        }

        def moduleAdvice(missing: String): String = {
          val module =
            if      (missing.startsWith("scala.xml"))                Some(("org.scala-lang.modules", "scala-xml"))
            else if (missing.startsWith("scala.util.parsing"))       Some(("org.scala-lang.modules", "scala-parser-combinators"))
            else if (missing.startsWith("scala.swing"))              Some(("org.scala-lang.modules", "scala-swing"))
            else None

          (module map { case (group, art) =>
            s"""\n(NOTE: It looks like the $art module is missing; try adding a dependency on "$group" : "$art".
               |       See http://docs.scala-lang.org/overviews/ for more information.)""".stripMargin
           } getOrElse "")
        }

        def localDummy = {
          if (nme.isLocalDummyName(name))
            owner.newLocalDummy(NoPosition)
          else NoSymbol
        }

        // (1) Try name.
        localDummy orElse fromName(name) orElse {
          // (2) Try with expanded name.  Can happen if references to private
          // symbols are read from outside: for instance when checking the children
          // of a class.  See #1722.
          fromName(nme.expandedName(name.toTermName, owner)) orElse {
            // (3) Try as a nested object symbol.
            nestedObjectSymbol orElse {
              // (4) Call the mirror's "missing" hook.
              adjust(mirrorThatLoaded(owner).missingHook(owner, name)) orElse {
                // (5) Create a stub symbol to defer hard failure a little longer.
                val advice = moduleAdvice(s"${owner.fullName}.$name")
                val missingMessage =
                  s"""|missing or invalid dependency detected while loading class file '$filename'.
                      |Could not access ${name.longString} in ${owner.kindString} ${owner.fullName},
                      |because it (or its dependencies) are missing. Check your build definition for
                      |missing or conflicting dependencies. (Re-run with `-Ylog-classpath` to see the problematic classpath.)
                      |A full rebuild may help if '$filename' was compiled against an incompatible version of ${owner.fullName}.$advice""".stripMargin
                owner.newStubSymbol(name, missingMessage)
              }
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

      val (privateWithin, inforef) = maybeReadSymbol() match {
        case Left(index) => NoSymbol -> index
        case Right(sym)  => sym -> readNat()
      }

      def isModuleFlag = (flags & MODULE) != 0L
      def isClassRoot  = (name == classRoot.name) && (owner == classRoot.owner)
      def isModuleRoot = (name == moduleRoot.name) && (owner == moduleRoot.owner)
      def pflags       = flags & PickledFlags

      def finishSym(sym: Symbol): Symbol = {
        /**
         * member symbols (symbols owned by a class) are added to the class's scope, with a number
         * of exceptions:
         *
         * (.) ...
         * (1) `local child` represents local child classes, see comment in Pickler.putSymbol.
         *     Since it is not a member, it should not be entered in the owner's scope.
         * (2) Similarly, we ignore local dummy symbols, as seen in SI-8868
         */
        def shouldEnterInOwnerScope = {
          sym.owner.isClass &&
            sym != classRoot &&
            sym != moduleRoot &&
            !sym.isModuleClass &&
            !sym.isRefinementClass &&
            !sym.isTypeParameter &&
            !sym.isExistentiallyBound &&
            sym.rawname != tpnme.LOCAL_CHILD && // (1)
            !nme.isLocalDummyName(sym.rawname)  // (2)
        }

        markFlagsCompleted(sym)(mask = AllFlags)
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
        if (shouldEnterInOwnerScope)
          symScope(sym.owner) enter sym

        sym
      }

      finishSym(tag match {
        case TYPEsym  | ALIASsym =>
          owner.newNonClassSymbol(name.toTypeName, NoPosition, pflags)
        case CLASSsym =>
          val sym = (
            if (isClassRoot) {
              if (isModuleFlag) moduleRoot.moduleClass setFlag pflags
              else classRoot setFlag pflags
            }
            else owner.newClassSymbol(name.toTypeName, NoPosition, pflags)
          )
          if (!atEnd)
            sym.typeOfThis = newLazyTypeRef(readNat())

          sym
        case MODULEsym =>
          val clazz = at(inforef, () => readType()).typeSymbol // after NMT_TRANSITION, we can leave off the () => ... ()
          if (isModuleRoot) moduleRoot setFlag pflags
          else owner.newLinkedModule(clazz, pflags)
        case VALsym =>
          if (isModuleRoot) { abort(s"VALsym at module root: owner = $owner, name = $name") }
          else owner.newTermSymbol(name.toTermName, NoPosition, pflags)

        case _ =>
          errorBadSignature("bad symbol tag: " + tag)
      })
    }

    protected def readType(forceProperType: Boolean = false): Type = {
      val tag = readByte()
      val end = readEnd()
      @inline def all[T](body: => T): List[T] = until(end, () => body)

      def readTypes()   = all(readTypeRef)
      def readSymbols() = all(readSymbolRef)
      def readAnnots()  = all(readAnnotationRef)

      // if the method is overloaded, the params cannot be determined (see readSymbol) => return NoType.
      // Only happen for trees, "case Apply" in readTree() takes care of selecting the correct
      // alternative after parsing the arguments.
      def MethodTypeRef(restpe: Type, params: List[Symbol]): Type = (
        if (restpe == NoType || (params contains NoSymbol)) NoType
        else MethodType(params, restpe)
      )
      def PolyOrNullaryType(restpe: Type, tparams: List[Symbol]): Type = tparams match {
        case Nil => NullaryMethodType(restpe)
        case _   => PolyType(tparams, restpe)
      }
      def CompoundType(clazz: Symbol, parents: List[Type]): Type = tag match {
        case REFINEDtpe   => RefinedType(parents, symScope(clazz), clazz)
        case CLASSINFOtpe => ClassInfoType(parents, symScope(clazz), clazz)
      }

      def readThisType(): Type = {
        val sym = readSymbolRef() match {
          case stub: StubSymbol if !stub.isClass =>
            // SI-8502 This allows us to create a stub for a unpickled reference to `missingPackage.Foo`.
            stub.owner.newStubSymbol(stub.name.toTypeName, stub.missingMessage, isPackage = true)
          case sym => sym
        }
        ThisType(sym)
      }

      // We're stuck with the order types are pickled in, but with judicious use
      // of named parameters we can recapture a declarative flavor in a few cases.
      // But it's still a rat's nest of adhockery.
      (tag: @switch) match {
        case NOtpe                     => NoType
        case NOPREFIXtpe               => NoPrefix
        case THIStpe                   => readThisType()
        case SINGLEtpe                 => SingleType(readTypeRef(), readSymbolRef().filter(_.isStable)) // SI-7596 account for overloading
        case SUPERtpe                  => SuperType(readTypeRef(), readTypeRef())
        case CONSTANTtpe               => ConstantType(readConstantRef())
        case TYPEREFtpe                => TypeRef(readTypeRef(), readSymbolRef(), readTypes())
        case TYPEBOUNDStpe             => TypeBounds(readTypeRef(), readTypeRef())
        case REFINEDtpe | CLASSINFOtpe => CompoundType(readSymbolRef(), readTypes())
        case METHODtpe                 => MethodTypeRef(readTypeRef(), readSymbols())
        case POLYtpe                   => PolyOrNullaryType(readTypeRef(), readSymbols())
        case EXISTENTIALtpe            => ExistentialType(underlying = readTypeRef(), quantified = readSymbols())
        case ANNOTATEDtpe              => AnnotatedType(underlying = readTypeRef(), annotations = readAnnots())
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
        case LITERALstring  => Constant(readNameRef().toString)
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
      val end = readEnd()
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
      val end = readEnd()
      until(end, () => readClassfileAnnotArg(readNat())).toArray(JavaArgumentTag)
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
      expect(SYMANNOT, "symbol annotation expected")
      val end = readEnd()
      val target = readSymbolRef()
      target.addAnnotation(readAnnotationInfo(end))
    }

    /** Read an annotation and return it. Used when unpickling
     *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg */
    protected def readAnnotation(): AnnotationInfo = {
      val tag = readByte()
      if (tag != ANNOTINFO)
        errorBadSignature("annotation expected (" + tag + ")")
      val end = readEnd()
      readAnnotationInfo(end)
    }

    private def readNonEmptyTree(tag: Int, end: Int): Tree = {
      @inline def all[T](body: => T): List[T] = until(end, () => body)
      @inline def rep[T](body: => T): List[T] = times(readNat(), () => body)

      // !!! What is this doing here?
      def fixApply(tree: Apply, tpe: Type): Apply = {
        val Apply(fun, args) = tree
        if (fun.symbol.isOverloaded) {
          fun setType fun.symbol.info
          inferMethodAlternative(fun, args map (_.tpe), tpe)
        }
        tree
      }
      def ref()         = readTreeRef()
      def caseRef()     = readCaseDefRef()
      def modsRef()     = readModifiersRef()
      def implRef()     = readTemplateRef()
      def nameRef()     = readNameRef()
      def tparamRef()   = readTypeDefRef()
      def vparamRef()   = readValDefRef()
      def memberRef()   = readMemberDefRef()
      def constRef()    = readConstantRef()
      def idRef()       = readIdentRef()
      def termNameRef() = readNameRef().toTermName
      def typeNameRef() = readNameRef().toTypeName
      def refTreeRef()  = ref() match {
        case t: RefTree => t
        case t          => errorBadSignature("RefTree expected, found " + t.shortClass)
      }
      def selectorsRef() = all(ImportSelector(nameRef(), -1, nameRef(), -1))

      /** A few of the most popular trees have been pulled to the top for
       *  switch efficiency purposes.
       */
      def readTree(tpe: Type): Tree = (tag: @switch) match {
        case IDENTtree           => Ident(nameRef)
        case SELECTtree          => Select(ref, nameRef)
        case APPLYtree           => fixApply(Apply(ref, all(ref)), tpe) // !!!
        case BINDtree            => Bind(nameRef, ref)
        case BLOCKtree           => all(ref) match { case stats :+ expr => Block(stats, expr) }
        case IFtree              => If(ref, ref, ref)
        case LITERALtree         => Literal(constRef)
        case TYPEAPPLYtree       => TypeApply(ref, all(ref))
        case TYPEDtree           => Typed(ref, ref)
        case ALTERNATIVEtree     => Alternative(all(ref))
        case ANNOTATEDtree       => Annotated(ref, ref)
        case APPLIEDTYPEtree     => AppliedTypeTree(ref, all(ref))
        case APPLYDYNAMICtree    => ApplyDynamic(ref, all(ref))
        case ARRAYVALUEtree      => ArrayValue(ref, all(ref))
        case ASSIGNtree          => Assign(ref, ref)
        case CASEtree            => CaseDef(ref, ref, ref)
        case CLASStree           => ClassDef(modsRef, typeNameRef, rep(tparamRef), implRef)
        case COMPOUNDTYPEtree    => CompoundTypeTree(implRef)
        case DEFDEFtree          => DefDef(modsRef, termNameRef, rep(tparamRef), rep(rep(vparamRef)), ref, ref)
        case EXISTENTIALTYPEtree => ExistentialTypeTree(ref, all(memberRef))
        case FUNCTIONtree        => Function(rep(vparamRef), ref)
        case IMPORTtree          => Import(ref, selectorsRef)
        case LABELtree           => LabelDef(termNameRef, rep(idRef), ref)
        case MATCHtree           => Match(ref, all(caseRef))
        case MODULEtree          => ModuleDef(modsRef, termNameRef, implRef)
        case NEWtree             => New(ref)
        case PACKAGEtree         => PackageDef(refTreeRef, all(ref))
        case RETURNtree          => Return(ref)
        case SELECTFROMTYPEtree  => SelectFromTypeTree(ref, typeNameRef)
        case SINGLETONTYPEtree   => SingletonTypeTree(ref)
        case STARtree            => Star(ref)
        case SUPERtree           => Super(ref, typeNameRef)
        case TEMPLATEtree        => Template(rep(ref), vparamRef, all(ref))
        case THIStree            => This(typeNameRef)
        case THROWtree           => Throw(ref)
        case TREtree             => Try(ref, rep(caseRef), ref)
        case TYPEBOUNDStree      => TypeBoundsTree(ref, ref)
        case TYPEDEFtree         => TypeDef(modsRef, typeNameRef, rep(tparamRef), ref)
        case TYPEtree            => TypeTree()
        case UNAPPLYtree         => UnApply(ref, all(ref))
        case VALDEFtree          => ValDef(modsRef, termNameRef, ref, ref)
        case _                   => noSuchTreeTag(tag, end)
      }

      val tpe    = readTypeRef()
      val sym    = if (isTreeSymbolPickled(tag)) readSymbolRef() else null
      val result = readTree(tpe)

      if (sym ne null) result setSymbol sym
      result setType tpe
    }

    /* Read an abstract syntax tree */
    protected def readTree(): Tree = {
      expect(TREE, "tree expected")
      val end = readEnd()
      readByte() match {
        case EMPTYtree => EmptyTree
        case tag       => readNonEmptyTree(tag, end)
      }
    }

    def noSuchTreeTag(tag: Int, end: Int) =
      errorBadSignature("unknown tree type (" + tag + ")")

    def readModifiers(): Modifiers = {
      val tag = readNat()
      if (tag != MODIFIERS)
        errorBadSignature("expected a modifiers tag (" + tag + ")")

      readEnd()
      val pflagsHi = readNat()
      val pflagsLo = readNat()
      val pflags = (pflagsHi.toLong << 32) + pflagsLo
      val flags = pickledToRawFlags(pflags)
      val privateWithin = readNameRef()
      Modifiers(flags, privateWithin, Nil)
    }

    /* Read a reference to a pickled item */
    protected def readSymbolRef(): Symbol             = {//OPT inlined from: at(readNat(), readSymbol) to save on closure creation
      val i = readNat()
      var r = entries(i)
      if (r eq null) {
        val savedIndex = readIndex
        readIndex = index(i)
        r = readSymbol()
        assert(entries(i) eq null, entries(i))
        entries(i) = r
        readIndex = savedIndex
      }
      r.asInstanceOf[Symbol]
    }

    protected def readNameRef(): Name                 = at(readNat(), readName)
    protected def readTypeRef(): Type                 = at(readNat(), () => readType()) // after the NMT_TRANSITION period, we can leave off the () => ... ()
    protected def readConstantRef(): Constant         = at(readNat(), readConstant)
    protected def readAnnotationRef(): AnnotationInfo = at(readNat(), readAnnotation)
    protected def readModifiersRef(): Modifiers       = at(readNat(), readModifiers)
    protected def readTreeRef(): Tree                 = at(readNat(), readTree)

    protected def readTypeNameRef(): TypeName         = readNameRef().toTypeName

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
    protected def readMemberDefRef(): MemberDef =
      readTreeRef() match {
        case tree:MemberDef => tree
        case other =>
          errorBadSignature("expected an MemberDef (" + other + ")")
      }

    protected def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    def inferMethodAlternative(fun: Tree, argtpes: List[Type], restpe: Type) {} // can't do it; need a compiler for that.

    def newLazyTypeRef(i: Int): LazyType = new LazyTypeRef(i)
    def newLazyTypeRefAndAlias(i: Int, j: Int): LazyType = new LazyTypeRefAndAlias(i, j)

    /** Convert to a type error, that is printed gracefully instead of crashing.
     *
     *  Similar in intent to what SymbolLoader does (but here we don't have access to
     *  error reporting, so we rely on the typechecker to report the error).
     */
    def toTypeError(e: MissingRequirementError) = {
      new TypeError(e.msg)
    }

    /** A lazy type which when completed returns type at index `i`. */
    private class LazyTypeRef(i: Int) extends LazyType with FlagAgnosticCompleter {
      private val definedAtRunId = currentRunId
      private val p = phase
      protected def completeInternal(sym: Symbol) : Unit = try {
        val tp = at(i, () => readType(sym.isTerm)) // after NMT_TRANSITION, revert `() => readType(sym.isTerm)` to `readType`

        // This is a temporary fix allowing to read classes generated by an older, buggy pickler.
        // See the generation of the LOCAL_CHILD class in Pickler.scala. In an earlier version, the
        // pickler did not add the ObjectTpe superclass, it used a trait as the first parent. This
        // tripped an assertion in AddInterfaces which checks that the first parent is not a trait.
        // This workaround can probably be removed in 2.12, because the 2.12 compiler is supposed
        // to only read classfiles generated by 2.12.
        val fixLocalChildTp = if (sym.rawname == tpnme.LOCAL_CHILD) tp match {
            case ClassInfoType(superClass :: traits, decls, typeSymbol) if superClass.typeSymbol.isTrait =>
              ClassInfoType(definitions.ObjectTpe :: superClass :: traits, decls, typeSymbol)
            case _ => tp
          } else tp

        if (p ne null) {
          slowButSafeEnteringPhase(p)(sym setInfo fixLocalChildTp)
        }
        if (currentRunId != definedAtRunId)
          sym.setInfo(adaptToNewRunMap(fixLocalChildTp))
      }
      catch {
        case e: MissingRequirementError => throw toTypeError(e)
      }
      override def complete(sym: Symbol) : Unit = {
        completeInternal(sym)
        if (!isCompilerUniverse) markAllCompleted(sym)
      }
      override def load(sym: Symbol) { complete(sym) }
    }

    /** A lazy type which when completed returns type at index `i` and sets alias
     *  of completed symbol to symbol at index `j`.
     */
    private class LazyTypeRefAndAlias(i: Int, j: Int) extends LazyTypeRef(i) {
      override def completeInternal(sym: Symbol) = try {
        super.completeInternal(sym)

        var alias = at(j, readSymbol)
        if (alias.isOverloaded)
          alias = slowButSafeEnteringPhase(picklerPhase)((alias suchThat (alt => sym.tpe =:= sym.owner.thisType.memberType(alt))))

        sym.asInstanceOf[TermSymbol].setAlias(alias)
      }
      catch {
        case e: MissingRequirementError => throw toTypeError(e)
      }
    }
  }
}
