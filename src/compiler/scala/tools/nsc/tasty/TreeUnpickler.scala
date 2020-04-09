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

package scala.tools.nsc.tasty

import scala.tools.tasty.{TastyRefs, TastyReader, TastyName, TastyFormat}, TastyRefs._

import scala.annotation.switch
import scala.collection.mutable
import scala.reflect.io.AbstractFile
import scala.reflect.internal.Variance
import scala.util.chaining._

/** Unpickler for typed trees
 *  @param reader              the reader from which to unpickle
 *  @param splices
 */
class TreeUnpickler[Tasty <: TastyUniverse](
    reader: TastyReader,
    nameAtRef: NameRef => TastyName,
    splices: Seq[Any])(implicit
    val tasty: Tasty) { self =>
  import tasty._, FlagSets._
  import TastyFormat._
  import TreeUnpickler._
  import MaybeCycle._
  import TastyFlags._
  import TastyModes._

  @inline
  final protected def unsupportedWhen(cond: Boolean, msg: => String)(implicit ctx: Context): Unit =
    if (cond) unsupportedError(msg)

  /** A map from addresses of definition entries to the symbols they define */
  private val symAtAddr = new mutable.HashMap[Addr, Symbol]

  /** A temporary map from addresses of definition entries to the trees they define.
   *  Used to remember trees of symbols that are created by a completion. Emptied
   *  once the tree is inlined into a larger tree.
   */
  private val cycleAtAddr = new mutable.HashMap[Addr, MaybeCycle]

  /** A map from addresses of type entries to the types they define.
   *  Currently only populated for types that might be recursively referenced
   *  from within themselves (i.e. RecTypes, LambdaTypes).
   */
  private val typeAtAddr = new mutable.HashMap[Addr, Type]

  /** The root symbol denotation which are defined by the Tasty file associated with this
   *  TreeUnpickler. Set by `enterTopLevel`.
   */
  private[this] var roots: Set[Symbol] = _

  /** The root symbols that are defined in this Tasty file. This
   *  is a subset of `roots.map(_.symbol)`.
   */
  private[this] var seenRoots: Set[Symbol] = Set()

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private[this] var ownerTree: OwnerTree = _

  //---------------- unpickling trees ----------------------------------------------------------------------------------

  private def registerSym(addr: Addr, sym: Symbol)(implicit ctx: Context) = {
    ctx.log(s"registered ${showSym(sym)} in ${sym.owner} at $addr")
    symAtAddr(addr) = sym
  }

  /** Enter all toplevel classes and objects into their scopes
   */
  def enter(classRoot: Symbol, moduleRoot: Symbol)(implicit ctx: Context): Unit = {
    this.roots = Set(moduleRoot, classRoot)
    val rdr = new TreeReader(reader).fork
    ownerTree = new OwnerTree(NoAddr, 0, rdr.fork, reader.endAddr)
    if (rdr.isTopLevel)
      rdr.indexStats(reader.endAddr)
  }

  class Completer(reader: TastyReader, tastyFlagSet: TastyFlagSet)(implicit ctx: Context) extends TastyLazyType { self =>
    import reader._

    self.withTastyFlagSet(tastyFlagSet)

    override def complete(sym: Symbol): Unit = {
      // implicit assertion that the completion is done by the same mirror that loaded owner
      cycleAtAddr(currentAddr) = ctx.withPhaseNoLater("pickler") { ctx0 =>
        new TreeReader(reader).readIndexedMember()(ctx0)
      }
    }
  }

  class TreeReader(val reader: TastyReader) {
    import reader._

    def forkAt(start: Addr): TreeReader = new TreeReader(subReader(start, endAddr))
    def fork: TreeReader = forkAt(currentAddr)

    def skipTree(tag: Int): Unit =
      if (tag >= firstLengthTreeTag) goto(readEnd())
      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
      else if (tag >= firstASTTreeTag) skipTree()
      else if (tag >= firstNatTreeTag) readNat()

    def skipTree(): Unit = skipTree(readByte())

    def skipParams(): Unit =
      while ({
        val tag = nextByte
        tag == PARAM || tag == TYPEPARAM || tag == PARAMEND
      }) skipTree()

    def skipTypeParams(): Unit =
      while (nextByte === TYPEPARAM) skipTree()

    /** Record all directly nested definitions and templates in current tree
     *  as `OwnerTree`s in `buf`.
     *  A complication concerns member definitions. These are lexically nested in a
     *  Template node, but need to be listed separately in the OwnerTree of the enclosing class
     *  in order not to confuse owner chains.
     */
    def scanTree(buf: mutable.ListBuffer[OwnerTree], mode: MemberDefMode = AllDefs): Unit = {
      val start = currentAddr
      val tag = readByte()
      tag match {
        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | TEMPLATE =>
          val end = readEnd()
          for (i <- 0 until numRefs(tag)) readNat()
          if (tag === TEMPLATE) {
            // Read all member definitions now, whereas non-members are children of
            // template's owner tree.
            val nonMemberReader = fork
            scanTrees(buf, end, MemberDefsOnly)
            buf += new OwnerTree(start, tag, nonMemberReader, end)
          }
          else if (mode != NoMemberDefs)
            buf += new OwnerTree(start, tag, fork, end)
          goto(end)
        case tag =>
          if (mode === MemberDefsOnly) skipTree(tag)
          else if (tag >= firstLengthTreeTag) {
            val end = readEnd()
            val nrefs = numRefs(tag)
            if (nrefs < 0) {
              for (i <- nrefs until 0) scanTree(buf)
              goto(end)
            }
            else {
              for (_ <- 0 until nrefs) readNat()
              if (tag === BIND) {
                // a Bind is never the owner of anything, so we set `end = start`
                buf += new OwnerTree(start, tag, fork, end = start)
              }

              scanTrees(buf, end)
            }
          }
          else if (tag >= firstNatASTTreeTag) { readNat(); scanTree(buf) }
          else if (tag >= firstASTTreeTag) scanTree(buf)
          else if (tag >= firstNatTreeTag) readNat()
      }
    }

    /** Record all directly nested definitions and templates between current address and `end`
     *  as `OwnerTree`s in `buf`
     */
    def scanTrees(buf: mutable.ListBuffer[OwnerTree], end: Addr, mode: MemberDefMode = AllDefs): Unit = {
      while (currentAddr.index < end.index) scanTree(buf, mode)
      assert(currentAddr.index === end.index)
    }

    /** The next tag, following through SHARED tags */
    def nextUnsharedTag: Int = {
      val tag = nextByte
      if (tag === SHAREDtype || tag === SHAREDterm) {
        val lookAhead = fork
        lookAhead.reader.readByte()
        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
      }
      else tag
    }

    def readTastyName(): TastyName = nameAtRef(readNameRef())

// ------ Reading types -----------------------------------------------------

    /** Read names in an interleaved sequence of (parameter) names and types/bounds */
    def readParamNames(end: Addr): List[TastyName] =
      until(end) {
        val name = readTastyName()
        skipTree()
        name
      }

    /** Read types or bounds in an interleaved sequence of (parameter) names and types/bounds */
    def readParamTypes[T <: Type](end: Addr)(implicit ctx: Context): List[T] =
      until(end) { readNat(); readType().asInstanceOf[T] }

    /** Read reference to definition and return symbol created at that definition */
    def readSymRef()(implicit ctx: Context): Symbol = symbolAt(readAddr())

    /** The symbol at given address; create a new one if none exists yet */
    def symbolAt(addr: Addr)(implicit ctx: Context): Symbol = symAtAddr.get(addr) match {
      case Some(sym) =>
        sym
      case None =>
        ctx.log(s"No symbol at $addr")
        val sym = forkAt(addr).createSymbol()(ctx.withOwner(ownerTree.findOwner(addr)))
        ctx.log(s"forward reference to $sym")
        sym
    }

    /** The symbol defined by current definition */
    def symbolAtCurrent()(implicit ctx: Context): Symbol = symAtAddr.get(currentAddr) match {
      case Some(sym) =>
        assert(ctx.owner === sym.owner, s"owner discrepancy for ${showSym(sym)}, expected: ${showSym(ctx.owner)}, found: ${showSym(sym.owner)}")
        sym
      case None =>
        createSymbol()
    }

    def readConstant(tag: Int)(implicit ctx: Context): Constant = (tag: @switch) match {
      case UNITconst =>
        tpd.Constant(())
      case TRUEconst =>
        tpd.Constant(true)
      case FALSEconst =>
        tpd.Constant(false)
      case BYTEconst =>
        tpd.Constant(readInt().toByte)
      case SHORTconst =>
        tpd.Constant(readInt().toShort)
      case CHARconst =>
        tpd.Constant(readNat().toChar)
      case INTconst =>
        tpd.Constant(readInt())
      case LONGconst =>
        tpd.Constant(readLongInt())
      case FLOATconst =>
        tpd.Constant(java.lang.Float.intBitsToFloat(readInt()))
      case DOUBLEconst =>
        tpd.Constant(java.lang.Double.longBitsToDouble(readLongInt()))
      case STRINGconst =>
        tpd.Constant(readTastyName().asSimpleName.raw)
      case NULLconst =>
        tpd.Constant(null)
      case CLASSconst =>
        tpd.Constant(readType())
      case ENUMconst =>
        tpd.Constant(readTypeRef().termSymbol)
    }

    /** Read a type */
    def readType()(implicit ctx: Context): Type = {
      val start = currentAddr
      val tag = readByte()
      ctx.log(s"reading type ${astTagToString(tag)} at $start")

      def registeringTypeWith[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }

      def readLengthType(): Type = {
        val end = readEnd()

        def readMethodic[N <: TastyName, PInfo <: Type]
            (companion: LambdaTypeCompanion[N, PInfo], nameMap: TastyName => N)(implicit ctx: Context): Type = {
          val result = typeAtAddr.getOrElse(start, {
            val nameReader = fork
            nameReader.skipTree() // skip result
            val paramReader = nameReader.fork
            val paramNames: List[N] = map(nameReader.readParamNames(end), nameMap)
            companion(paramNames)(
              pt => typeAtAddr(start) = pt,
              () => paramReader.readParamTypes[PInfo](end),
              () => readType()
            ).tap(typeAtAddr(start) = _)
          })
          goto(end)
          result
        }

        def readVariances(tp: Type): Type = tp match {
          case tp: LambdaPolyType if currentAddr != end =>
            val vs = until(end) {
              readByte() match {
                case STABLE => Variance.Invariant
                case COVARIANT => Variance.Covariant
                case CONTRAVARIANT => Variance.Contravariant
              }
            }
            tp.withVariances(vs)
          case _ => tp
        }

        val result =
          (tag: @switch) match {
            case TERMREFin => selectTerm(readTastyName(), readType(), readType())
            case TYPEREFin => selectType(readTastyName().toTypeName, readType(), readType())
            case REFINEDtype =>
              var name   = readTastyName()
              val parent = readType()
              if (nextUnsharedTag === TYPEBOUNDS) name = name.toTypeName
              ctx.enterRefinement(parent)(defn.RefinedType(_, _, name, readType()))
            case APPLIEDtype => defn.AppliedType(readType(), until(end)(readType()))
            case TYPEBOUNDS =>
              val lo = readType()
              if (nothingButMods(end))
                typeRef(readVariances(lo))
              else defn.TypeBounds(lo, readVariances(readType()))
            case ANNOTATEDtype => defn.AnnotatedType(readType(), mkAnnotation(readTerm()(ctx.addMode(ReadAnnotation))))
            case ANDtype => defn.IntersectionType(readType(), readType())
            case ORtype => unionIsUnsupported
            case SUPERtype => defn.SuperType(readType(), readType())
            case MATCHtype => matchTypeIsUnsupported
            case POLYtype => readMethodic(PolyType, _.toTypeName)
            case METHODtype => readMethodic(MethodType, id)
            case ERASEDMETHODtype | ERASEDGIVENMETHODtype => erasedRefinementIsUnsupported
            case IMPLICITMETHODtype | GIVENMETHODtype => readMethodic(ImplicitMethodType, id)
            case TYPELAMBDAtype => readMethodic(HKTypeLambda, _.toTypeName)
            case PARAMtype => // reference to a type parameter within a LambdaType
              readTypeRef().typeParams(readNat()).ref
          }
        assert(currentAddr === end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      def readSimpleType(): Type = {
        (tag: @switch) match {
          case TYPEREFdirect => readSymRef().termRef
          case TERMREFdirect => readSymRef().singleRef
          case TYPEREFsymbol | TERMREFsymbol => readSymNameRef()
          case TYPEREFpkg => readPackageRef().moduleClass.ref
          case TERMREFpkg => readPackageRef().termRef
          case TYPEREF => selectType(readTastyName().toTypeName, readType())
          case TERMREF => selectTerm(readTastyName(), readType())
          case THIS => defn.ThisType(singletonLike(readType()))
          case RECtype =>
            typeAtAddr.get(start) match {
              case Some(tp) =>
                skipTree(tag)
                tp
              case None =>
                defn.RecType(rt =>
                  registeringTypeWith(rt, readType()(ctx.withOwner(rt.refinementClass)))
                ).tap(typeAtAddr(start) = _)
            }
          case RECthis => recThis(readTypeRef())
          case SHAREDtype =>
            val ref = readAddr()
            typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
          case BYNAMEtype => defn.ByNameType(readType())
          case _ => defn.ConstantType(readConstant(tag))
        }
      }
      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }

    private def readSymNameRef()(implicit ctx: Context): Type = {
      val sym    = readSymRef()
      val prefix = readType()
      prefixedRef(prefix, sym)
    }

    private def readPackageRef()(implicit ctx: Context): Symbol = {
      ctx.requiredPackage(readTastyName())
    }

    def readTypeRef(): Type = typeAtAddr(readAddr())

// ------ Reading definitions -----------------------------------------------------

    private def nothingButMods(end: Addr): Boolean =
      currentAddr === end || isModifierTag(nextByte)

    private def normalizeFlags(tag: Int, owner: Symbol, givenFlags: FlagSet, tastyFlags: TastyFlagSet, name: TastyName, isAbsType: Boolean, isClass: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): FlagSet = {
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !givenFlags.isOneOf(TermParamOrAccessor) ||
        isAbsType ||
        tastyFlags.is(Opaque) && !isClass
      var flags = givenFlags
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (tag === DEFDEF) flags |= Method
      if (tag === VALDEF) {
        if (flags.not(Mutable)) flags |= Stable
        if (owner.flags.is(Trait)) flags |= Accessor
      }
      if (givenFlags.is(Module))
        flags = flags | (if (tag === VALDEF) ModuleCreationFlags else ModuleClassCreationFlags)
      if (ctx.owner.isClass) {
        if (tag === TYPEPARAM) flags |= Param
        else if (tag === PARAM) {
          flags |= ParamAccessor | Accessor | Stable
          if (!rhsIsEmpty) // param alias
            flags |= Method
        }
      }
      else if (isParamTag(tag)) flags |= Param
      if (name.isDefaultName || flags.is(Param) && owner.isMethod && owner.is(DefaultParameterized)) {
        flags |= DefaultParameterized
      }
      flags
    }

    def isAbstractType(ttag: Int)(implicit ctx: Context): Boolean = nextUnsharedTag match {
      case LAMBDAtpt =>
        val rdr = fork
        rdr.reader.readByte()  // tag
        rdr.reader.readNat()   // length
        rdr.skipParams()       // tparams
        rdr.isAbstractType(rdr.nextUnsharedTag)
      case TYPEBOUNDS | TYPEBOUNDStpt => true
      case _ => false
    }

    /** Create symbol of definition node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createSymbol()(implicit ctx: Context): Symbol = nextByte match {
      case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
        createMemberSymbol()
      case BIND =>
        createBindSymbol()
      case TEMPLATE =>
        val localDummy = ctx.newLocalDummy
        registerSym(currentAddr, localDummy)
        localDummy
      case tag =>
        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
    }

    private def createBindSymbol()(implicit ctx: Context): Symbol = {
      val start = currentAddr
      readByte() // tag
      readEnd()  // end
      var name: TastyName = readTastyName()
      if (nextUnsharedTag === TYPEBOUNDS) name = name.toTypeName
      val typeReader = fork
      val completer = new TastyLazyType {
        override def complete(sym: Symbol): Unit = ctx.setInfo(sym, typeReader.readType())
      }
      val sym = ctx.newSymbol(ctx.owner, name, FlagSets.Case, completer)
      registerSym(start, sym)
      sym
    }

    /** Create symbol of member definition or parameter node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createMemberSymbol()(implicit ctx: Context): Symbol = {
      val start = currentAddr
      val tag = readByte()
      def isTypeTag = tag === TYPEDEF || tag === TYPEPARAM
      val end = readEnd()
      var name: TastyName = readTastyName()
      if (isTypeTag) name = name.toTypeName
      skipParams()
      val ttag = nextUnsharedTag
      val isAbsType = isAbstractType(ttag)
      val isClass = ttag === TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsIsEmpty = nothingButMods(end)
      if (!rhsIsEmpty) skipTree()
      val (givenFlags, tastyFlagSet, annotFns, privateWithin) =
        readModifiers(end, readTypedAnnot, readTypedWithin, noSymbol)
      val flags = normalizeFlags(tag, ctx.owner, givenFlags, tastyFlagSet, name, isAbsType, isClass, rhsIsEmpty)
      def showFlags = {
        if (!tastyFlagSet)
          show(flags)
        else if (isEmpty(givenFlags))
          tastyFlagSet.debug
        else
          show(flags) + " | " + tastyFlagSet.debug
      }
      def isModuleClass   = flags.is(Module) && isClass
      def isTypeParameter = flags.is(Param) && isTypeTag
      def canEnterInClass = !isModuleClass && !isTypeParameter
      ctx.log {
        val msg = if (isSymbol(privateWithin)) s" private within $privateWithin" else ""
        s"""creating symbol ${name}${msg} at $start with flags $showFlags"""
      }
      def adjustIfModule(completer: TastyLazyType) =
        if (isModuleClass) ctx.adjustModuleClassCompleter(completer, name) else completer
      val sym = {
        if (tag === TYPEPARAM && ctx.owner.isClassConstructor) {
          ctx.findOuterClassTypeParameter(name.toTypeName)
        }
        else {
          val completer = adjustIfModule(new Completer(subReader(start, end), tastyFlagSet))
          roots.find(ctx.isSameRoot(_,name)) match {
            case Some(found) =>
              val rootd   = if (isModuleClass) found.linkedClassOfClass else found
              ctx.adjustSymbol(rootd, flags, completer, privateWithin) // dotty "removes one completion" here from the flags, which is not possible in nsc
              seenRoots += rootd
              ctx.log(s"replaced info of ${showSym(rootd)}")
              rootd
            case _ =>
              if (isModuleClass)
                ctx.adjustSymbol(completer.sourceModule.moduleClass, flags, completer, privateWithin)
              else if (isClass)
                ctx.newClassSymbol(ctx.owner, name.toTypeName, flags, completer, privateWithin)
              else
                ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin)
          }
        }
      }
      sym.setAnnotations(annotFns.map(_(sym)))
      ctx.owner match {
        case cls: ClassSymbol if canEnterInClass =>
          val decls = cls.rawInfo.decls
          if (allowsOverload(sym)) decls.enter(sym)
          else decls.enterIfNew(sym)
        case _ =>
      }
      registerSym(start, sym)
      if (isClass) {
        sym.completer.withDecls(ctx.mkScope)
        val localCtx = ctx.withOwner(sym)
        forkAt(templateStart).indexTemplateParams()(localCtx)
      }
      goto(start)
      sym
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin
     *  boundary symbol.
     */
    def readModifiers[WithinType, AnnotType]
        (end: Addr, readAnnot: Context => Symbol => AnnotType, readWithin: Context => WithinType, defaultWithin: WithinType)
        (implicit ctx: Context): (FlagSet, TastyFlagSet, List[Symbol => AnnotType], WithinType) = {
      var tastyFlagSet = emptyTastyFlags
      var flags = emptyFlags
      var annotFns: List[Symbol => AnnotType] = Nil
      var privateWithin = defaultWithin
      while (currentAddr.index != end.index) {
        def addFlag(flag: FlagSet) = {
          flags |= flag
          readByte()
        }
        def addTastyFlag(flag: TastyFlagSet) = {
          tastyFlagSet |= flag
          readByte()
        }
        nextByte match {
          case PRIVATE => addFlag(Private)
          case INTERNAL => addTastyFlag(Internal)
          case PROTECTED => addFlag(Protected)
          case ABSTRACT =>
            readByte()
            nextByte match {
              case OVERRIDE => addFlag(AbsOverride)
              case _ => flags |= Abstract
            }
          case FINAL => addFlag(Final)
          case SEALED => addFlag(Sealed)
          case CASE => addFlag(Case)
          case IMPLICIT => addFlag(Implicit)
          case ERASED => addTastyFlag(Erased)
          case LAZY => addFlag(Lazy)
          case OVERRIDE => addFlag(Override)
          case INLINE => addTastyFlag(Inline)
          case INLINEPROXY => addTastyFlag(InlineProxy)
          case MACRO => addTastyFlag(TastyMacro)
          case OPAQUE => addTastyFlag(Opaque)
          case STATIC => addFlag(JavaStatic)
          case OBJECT => addFlag(Module)
          case TRAIT => addFlag(Trait)
          case ENUM => addTastyFlag(Enum)
          case LOCAL => addFlag(Local)
          case SYNTHETIC => addFlag(Synthetic)
          case ARTIFACT => addFlag(Artifact)
          case MUTABLE => addFlag(Mutable)
          case FIELDaccessor => addFlag(Accessor)
          case CASEaccessor => addFlag(CaseAccessor)
          case COVARIANT => addFlag(Covariant)
          case CONTRAVARIANT => addFlag(Contravariant)
          case SCALA2X => addTastyFlag(Scala2x)
          case DEFAULTparameterized => addFlag(DefaultParameterized)
          case STABLE => addFlag(Stable)
          case EXTENSION => addTastyFlag(Extension)
          case GIVEN => addFlag(Implicit)
          case PARAMsetter => addFlag(ParamAccessor)
          case PARAMalias => addTastyFlag(SuperParamAlias)
          case EXPORTED => addTastyFlag(Exported)
          case OPEN => addTastyFlag(Open)
          case PRIVATEqualified =>
            readByte()
            privateWithin = readWithin(ctx)
          case PROTECTEDqualified =>
            addFlag(Protected)
            privateWithin = readWithin(ctx)
          case ANNOTATION =>
            annotFns = readAnnot(ctx) :: annotFns
          case tag =>
            assert(assertion = false, s"illegal modifier tag ${astTagToString(tag)} at $currentAddr, end = $end")
        }
      }
      (flags, tastyFlagSet, if (ctx.ignoreAnnotations) Nil else annotFns.reverse, privateWithin)
    }

    private val readTypedWithin: Context => Symbol = implicit ctx => readType().typeSymbolDirect

    private val readTypedAnnot: Context => Symbol => Annotation = { implicit ctx =>
      readByte()
      val end = readEnd()
      val tp = readType()
      val lazyAnnotTree = readLaterWithOwner(end, rdr => ctx => rdr.readTerm()(ctx))
      owner => Annotation.deferredSymAndTree(owner)(_ => tp.typeSymbolDirect)(lazyAnnotTree(owner))
    }

    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     */
    def indexStats(end: Addr)(implicit ctx: Context): Unit = {
      while (currentAddr.index < end.index) {
        nextByte match {
          case tag @ (VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM) =>
            symbolAtCurrent()
            skipTree()
          case IMPORT =>
            skipTree()
          case PACKAGE =>
            processPackage(end => implicit ctx => indexStats(end))
          case _ =>
            skipTree()
        }
      }
      assert(currentAddr.index === end.index)
    }

    /** Process package with given operation `op`. The operation takes as arguments
     *   - a `RefTree` representing the `pid` of the package,
     *   - an end address,
     *   - a context which has the processed package as owner
     */
    def processPackage[T](op: Addr => Context => T)(implicit ctx: Context): T = {
      readByte()
      val end = readEnd()
      val tpe = readType()
      op(end)(ctx.withOwner(tpe.typeSymbolDirect.moduleClass))
    }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag` starting at current address.
     */
    def indexParams(tag: Int)(implicit ctx: Context): Unit =
      while (nextByte === tag) {
        symbolAtCurrent()
        skipTree()
      }

    /** Create symbols for all type and value parameters of template starting
     *  at current address.
     */
    def indexTemplateParams()(implicit ctx: Context): Unit = {
      assert(readByte() === TEMPLATE)
      readEnd()
      indexParams(TYPEPARAM)
      indexParams(PARAM)
    }

    def readIndexedMember()(implicit ctx: Context): NoCycle = cycleAtAddr.remove(currentAddr) match {
      case Some(maybeCycle) =>
        assert(maybeCycle ne Tombstone, s"Cyclic reference while unpickling definition at address ${currentAddr.index} in file ${ctx.source}")
        skipTree()
        maybeCycle.asInstanceOf[NoCycle]
      case _ =>
        val start = currentAddr
        cycleAtAddr(start) = Tombstone
        val noCycle = readNewMember()
        cycleAtAddr.remove(start)
        noCycle
    }

    private def readNewMember()(implicit ctx: Context): NoCycle = {
      val symAddr = currentAddr
      val sym     = symAtAddr(symAddr)
      val tag     = readByte()
      val end     = readEnd()
      val tname   = readTastyName()

      ctx.log(s"completing member $tname at $symAddr. ${showSym(sym)}")

      val completer = sym.completer

      def readParamss(implicit ctx: Context): List[List[NoCycle/*ValDef*/]] = nextByte match {
        case PARAM | PARAMEND =>
          readParams[NoCycle](PARAM) ::
            (if (nextByte == PARAMEND) { readByte(); readParamss } else Nil)

        case _ => Nil
      }

      try {
        val localCtx = ctx.withOwner(sym)
        tag match {
          case DEFDEF =>
            val unsupported = completer.tastyFlagSet &~ (Extension | Inline | TastyMacro | Exported)
            unsupportedWhen(unsupported.hasFlags, s"flags on $sym: ${show(unsupported)}")
            if (completer.tastyFlagSet.is(Extension)) ctx.log(s"$tname is a Scala 3 extension method.")
            unsupportedWhen(completer.tastyFlagSet.is(Inline, butNot = TastyMacro), s"inline $sym")
            unsupportedWhen(completer.tastyFlagSet.is(Inline | TastyMacro), s"macro $sym")
            val isCtor = sym.isClassConstructor
            val typeParams = {
              if (isCtor) {
                skipTypeParams()
                sym.owner.typeParams
              }
              else {
                readParams[NoCycle](TYPEPARAM)(localCtx).map(symFromNoCycle)
              }
            }
            val vparamss = readParamss(localCtx)
            val tpt = readTpt()(localCtx)
            val valueParamss = normalizeIfConstructor(vparamss.map(_.map(symFromNoCycle)), isCtor)
            val resType = effectiveResultType(sym, typeParams, tpt.tpe)
            ctx.setInfo(sym, defn.DefDefType(if (isCtor) Nil else typeParams, valueParamss, resType))
          case VALDEF => // valdef in TASTy is either a module value or a method forwarder to a local value.
            val isInline = completer.tastyFlagSet.is(Inline)
            val unsupported = completer.tastyFlagSet &~ (Inline | Enum | Extension | Exported)
            unsupportedWhen(unsupported.hasFlags, s"flags on $sym: ${show(unsupported)}")
            val tpe = readTpt()(localCtx).tpe
            if (isInline) unsupportedWhen(!isConstantType(tpe), s"inline val ${sym.nameString} with non-constant type $tpe")
            ctx.setInfo(sym,
              if (completer.tastyFlagSet.is(Enum)) defn.ConstantType(tpd.Constant((sym, tpe))).tap(_.typeSymbol.setFlag(Final))
              else if (sym.isMethod) defn.ExprType(tpe)
              else tpe
            )
          case TYPEDEF | TYPEPARAM =>
            val unsupported = completer.tastyFlagSet &~ (Enum | Open | Opaque | Exported)
            unsupportedWhen(unsupported.hasFlags, s"flags on $sym: ${show(unsupported)}")
            if (sym.isClass) {
              sym.owner.ensureCompleted()
              readTemplate(symAddr)(localCtx)
            }
            else {
              // sym.setFlag(Provisional) // TODO [tasty]: is there an equivalent in scala 2?
              val rhs = readTpt()(localCtx)
              // TODO [tasty]: if opaque type alias will be supported, unwrap `type bounds with alias` to bounds and then
              //               refine self type of the owner to be aware of the alias.
              ctx.setInfo(sym, defn.NormalisedBounds(rhs.tpe, sym))
              if (sym.is(Param)) sym.resetFlag(Private | Protected)
              // if sym.isOpaqueAlias then sym.typeRef.recomputeDenot() // make sure we see the new bounds from now on
              // sym.resetFlag(Provisional)
            }
          case PARAM =>
            val unsupported = completer.tastyFlagSet &~ (SuperParamAlias | Exported)
            unsupportedWhen(unsupported.hasFlags, s"flags on parameter $sym: ${show(unsupported)}")
            val tpt = readTpt()(localCtx)
            ctx.setInfo(sym,
              if (nothingButMods(end) && sym.not(ParamAccessor)) tpt.tpe
              else defn.ExprType(tpt.tpe))
        }
        ctx.log(s"typed ${showSym(sym)} : ${if (sym.isClass) sym.tpe else sym.info} in owner ${showSym(sym.owner)}")
        goto(end)
        NoCycle(at = symAddr)
      } catch {
        case err: TypeError =>
          ctx.setInfo(sym, errorType)
          throw err
      }
    }

    private def readTemplate(symAddr: Addr)(implicit ctx: Context): Unit = {
      val cls = ctx.enterClassCompletion()
      val localDummy = symbolAtCurrent()
      assert(readByte() === TEMPLATE)
      val end = readEnd()

      // ** PARAMETERS **
      ctx.log(s"Template: reading parameters of $cls")
      val tparams = readIndexedParams[NoCycle](TYPEPARAM)
      if (tparams.nonEmpty) {
        cls.info = defn.PolyType(tparams.map(symFromNoCycle), cls.info)
      }
      readIndexedParams[NoCycle](PARAM) // skip value parameters

      // ** MEMBERS **
      ctx.log(s"Template: indexing members of $cls")
      val bodyIndexer = fork
      while (bodyIndexer.reader.nextByte != DEFDEF) bodyIndexer.skipTree() // skip until primary ctor
      bodyIndexer.indexStats(end)

      // ** PARENTS **
      ctx.log(s"Template: adding parents of $cls")
      val parents = {
        val parentCtx = ctx.withOwner(localDummy).addMode(ReadParents)
        val parentWithOuter = parentCtx.addMode(OuterTerm)
        collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
          nextUnsharedTag match {
            case APPLY | TYPEAPPLY | BLOCK => readTerm()(parentWithOuter).tpe
            case _ => readTpt()(parentCtx).tpe
          }
        }
      }

      if (nextByte === SELFDEF) {
        ctx.log(s"Template: adding self-type of $cls")
        readByte() // read SELFDEF tag
        readLongNat() // skip Name
        val selfTpe = readTpt().tpe
        ctx.log(s"Template: self-type is $selfTpe")
        cls.typeOfThis = selfTpe
      }

      val parentTypes = ctx.adjustParents(cls, parents)

      ctx.setInfo(cls, {
        val classInfo = defn.ClassInfoType(parentTypes, cls.rawInfo.decls, cls.asType)
        // TODO [tasty]: if support opaque types, refine the self type with any opaque members here
        if (tparams.isEmpty) classInfo
        else defn.PolyType(tparams.map(symFromNoCycle), classInfo)
      })

      ctx.log(s"Template: Updated info of $cls with parents $parentTypes.")
    }

    def isTopLevel: Boolean = nextByte === IMPORT || nextByte === PACKAGE

    def readIndexedStatAsSym(exprOwner: Symbol)(implicit ctx: Context): NoCycle = nextByte match {
      case TYPEDEF | VALDEF | DEFDEF =>
        readIndexedMember()
      case IMPORT =>
        unsupportedTermTreeError("import statement")
      case PACKAGE =>
        unsupportedTermTreeError("package statement")
      case _ =>
        skipTree() // readTerm()(ctx.withOwner(exprOwner))
        NoCycle(at = NoAddr)
    }

    def readIndexedStatsAsSyms(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[NoCycle] =
      until(end)(readIndexedStatAsSym(exprOwner))

    def readStatsAsSyms(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[NoCycle] = {
      fork.indexStats(end)
      readIndexedStatsAsSyms(exprOwner, end)
    }

    def readIndexedParams[T <: MaybeCycle /*MemberDef*/](tag: Int)(implicit ctx: Context): List[T] =
      collectWhile(nextByte === tag) { readIndexedMember().asInstanceOf[T] }

    def readParams[T <: MaybeCycle /*MemberDef*/](tag: Int)(implicit ctx: Context): List[T] = {
      if (nextByte == tag) {
        fork.indexParams(tag)
        readIndexedParams(tag)
      }
      else {
        Nil
      }
    }

// ------ Reading trees -----------------------------------------------------

    def readTerm()(implicit ctx: Context): Tree = {  // TODO: rename to readTree
      val start = currentAddr
      val tag = readByte()
      ctx.log(s"reading term ${astTagToString(tag)} at $start")

      def inParentCtor = ctx.mode.is(ReadParents | OuterTerm)

      def readPathTerm(): Tree = {
        goto(start)
        tpd.PathTree(readType())
      }

      def readQualId(): (Ident, Type) = {
        val qual = readTerm().asInstanceOf[Ident]
        (qual, defn.ThisType(symOfTypeRef(qual.tpe)))
      }

      def completeSelectType(name: TastyName.TypeName)(implicit ctx: Context): Tree = completeSelect(name)

      def completeSelect(name: TastyName)(implicit ctx: Context): Tree = {
        val localCtx = ctx.selectionCtx(name)
        val qual     = readTerm()(localCtx)
        val qualType = qual.tpe
        tpd.Select(qual, name)(namedMemberOfPrefix(qualType, name)(localCtx))
      }

      def completeSelectionParent(name: TastyName)(implicit ctx: Context): Tree = {
        assert(name.isSignedConstructor, s"Parent of ${ctx.owner} is not a constructor.")
        readTerm() // just need the type of the parent
      }

      def readSimpleTerm(): Tree = tag match {
        case SHAREDterm => forkAt(readAddr()).readTerm()
        case IDENT => tpd.Ident(readTastyName())(readType())
        case IDENTtpt => tpd.Ident(readTastyName().toTypeName)(readType())
        case SELECT =>
          if (inParentCtor) completeSelectionParent(readTastyName())
          else completeSelect(readTastyName())
        case SELECTtpt => completeSelectType(readTastyName().toTypeName)
        case QUALTHIS =>
          val (qual, tref) = readQualId()
          tpd.This(qual)(tref)
        case NEW => tpd.New(readTpt())
        case SINGLETONtpt => tpd.SingletonTypeTree(readTerm())
        case BYNAMEtpt => tpd.ByNameTypeTree(readTpt())
        case NAMEDARG => tpd.NamedArg(readTastyName(), readTerm())
        case THROW => unsupportedTermTreeError("throw clause")
        case _     => readPathTerm()
      }

      def readLengthTerm(): Tree = {
        val end = readEnd()
        val result =
          (tag: @switch) match {
            case SUPER =>
              val qual = readTerm()
              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (untpd.EmptyTypeIdent, noType))
              tpd.Super(qual, mixId)(mixTpe)
            case APPLY =>
              val fn = readTerm()
              if (inParentCtor) {
                until(end)(skipTree())
                tpd.TypeTree(fnResult(fn.tpe))
              } else {
                tpd.Apply(fn, until(end)(readTerm()))
              }
            case TYPEAPPLY => tpd.TypeApply(readTerm(), until(end)(readTpt()))
            case TYPED => tpd.Typed(readTerm(), readTpt())
            case IF =>
              if (nextByte === INLINE) unsupportedTermTreeError("inline conditional expression")
              else tpd.If(readTerm(), readTerm(), readTerm()) // if is ok if its parts are made of constants/paths
            case REPEATED =>
              val elemtpt = readTpt()
              tpd.SeqLiteral(until(end)(readTerm()), elemtpt)
            case REFINEDtpt =>
              val refineCls = symAtAddr.getOrElse(start, ctx.newRefinementClassSymbol)
              registerSym(start, refineCls)
              typeAtAddr(start) = refineCls.ref
              val refinement = defn.RefinedType(readTpt().tpe :: Nil, refineCls)
              readStatsAsSyms(refineCls, end)(ctx.withOwner(refineCls))
              tpd.TypeTree(refinement)
            case APPLIEDtpt =>
              // If we do directly a tpd.AppliedType tree we might get a
              // wrong number of arguments in some scenarios reading F-bounded
              // types. This came up in #137 of collection strawman.
              tpd.AppliedTypeTree(readTpt(), until(end)(readTpt()))
            case ANNOTATEDtpt => tpd.Annotated(readTpt(), readTerm()(ctx.addMode(ReadAnnotation)))
            case LAMBDAtpt => tpd.LambdaTypeTree(readParams[NoCycle](TYPEPARAM).map(symFromNoCycle), readTpt())
            case MATCHtpt => matchTypeIsUnsupported
            case TYPEBOUNDStpt =>
              val lo    = readTpt()
              val hi    = if (currentAddr == end) lo else readTpt()
              val alias = if (currentAddr == end) untpd.EmptyTree else readTpt()
              if (alias != untpd.EmptyTree) alias // only for opaque type alias
              else tpd.TypeBoundsTree(lo, hi)
            case BLOCK =>
              if (inParentCtor) {
                val exprReader = fork
                skipTree()
                until(end)(skipTree()) //val stats = readStats(ctx.owner, end)
                exprReader.readTerm()
              }
              else unsupportedTermTreeError("block expression")
            case ASSIGN      => unsupportedTermTreeError("assignment expression")
            case LAMBDA      => unsupportedTermTreeError("anonymous function literal")
            case MATCH       => unsupportedTermTreeError("match expression")
            case RETURN      => unsupportedTermTreeError("return statement")
            case WHILE       => unsupportedTermTreeError("loop statement")
            case TRY         => unsupportedTermTreeError("try expression")
            case BIND        => unsupportedTermTreeError("bind pattern")
            case ALTERNATIVE => unsupportedTermTreeError("pattern alternative")
            case UNAPPLY     => unsupportedTermTreeError("unapply pattern")
            case INLINED     => unsupportedTermTreeError("inlined expression")
            case SELECTouter => metaprogrammingIsUnsupported // only within inline
            case HOLE        => assertNoMacroHole
            case _           => readPathTerm()
          }
        assert(currentAddr === end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm() // dotty sets span of tree to start
    }

    def readTpt()(implicit ctx: Context): Tree = {
      val tpt: Tree = nextByte match {
        case SHAREDterm =>
          readByte()
          forkAt(readAddr()).readTpt()
        case BLOCK => // BLOCK appears in type position when quoting a type, but only in the body of a method
          metaprogrammingIsUnsupported
        case HOLE => assertNoMacroHole
        case tag  =>
          if (isTypeTreeTag(tag)) readTerm()(ctx.retractMode(OuterTerm))
          else {
            val tp = readType()
            if (!(isNoType(tp) || isError(tp))) tpd.TypeTree(tp) else untpd.EmptyTree
          }
      }
      tpt
    }

    /**
      * A HOLE should never appear in TASTy for a top level class, only in quotes.
      */
    private def assertNoMacroHole[T]: T = assertError("Scala 3 macro hole in pickled TASTy")

    private def metaprogrammingIsUnsupported[T](implicit ctx: Context): T =
      unsupportedError("Scala 3 metaprogramming features")

    def readLaterWithOwner[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Symbol => Context => Either[String, T] = {
      val localReader = fork
      goto(end)
      owner => ctx0 => readWith(localReader, owner, ctx.mode | ReadAnnotation , ctx.source, op)(ctx0)
    }

  }

  private def handleTypeError(implicit ctx: Context): PartialFunction[Throwable, String] = { case err: TypeError =>
    ctx.owner.info = errorType
    err.getMessage()
  }

  def readWith[T <: AnyRef](
    reader: TreeReader,
    owner: Symbol,
    mode: TastyMode,
    source: AbstractFile,
    op: TreeReader => Context => T)(
    implicit ctx: Context
  ): Either[String, T] =
    ctx.withSafePhaseNoLater("pickler")(handleTypeError){ ctx0 =>
      ctx0.log(s"starting to read at ${reader.reader.currentAddr} with owner $owner")
      op(reader)(ctx0
        .withOwner(owner)
        .withMode(mode)
        .withSource(source)
      )
    }

  /** A lazy datastructure that records how definitions are nested in TASTY data.
   *  The structure is lazy because it needs to be computed only for forward references
   *  to symbols that happen before the referenced symbol is created (see `symbolAt`).
   *  Such forward references are rare.
   *
   *  @param   addr    The address of tree representing an owning definition, NoAddr for root tree
   *  @param   tag     The tag at `addr`. Used to determine which subtrees to scan for children
   *                   (i.e. if `tag` is template, don't scan member defs, as these belong already
   *                    to enclosing class).
   *  @param   reader  The reader to be used for scanning for children
   *  @param   end     The end of the owning definition
   */
  class OwnerTree(val addr: Addr, tag: Int, reader: TreeReader, val end: Addr) {

    private var myChildren: List[OwnerTree] = _

    /** All definitions that have the definition at `addr` as closest enclosing definition */
    def children: List[OwnerTree] = {
      if (myChildren === null) myChildren = {
        val buf = new mutable.ListBuffer[OwnerTree]
        reader.scanTrees(buf, end, if (tag === TEMPLATE) NoMemberDefs else AllDefs)
        buf.toList
      }
      myChildren
    }

    /** Find the owner of definition at `addr` */
    def findOwner(addr: Addr)(implicit ctx: Context): Symbol = {
      def search(cs: List[OwnerTree], current: Symbol): Symbol =
        try cs match {
        case ot :: cs1 =>
          if (ot.addr.index === addr.index) {
            assert(current.exists, s"no symbol at $addr")
            current
          }
          else if (ot.addr.index < addr.index && addr.index < ot.end.index)
            search(ot.children, reader.symbolAt(ot.addr))
          else
            search(cs1, current)
        case Nil =>
          throw new TreeWithoutOwner
      }
      catch {
        case ex: TreeWithoutOwner =>
          ctx.log(s"no owner for $addr among $cs%, %") // pickling.println
          throw ex
      }
      try search(children, noSymbol)
      catch {
        case ex: TreeWithoutOwner =>
          ctx.log(s"ownerTree = $ownerTree") // pickling.println
          throw ex
      }
    }

    override def toString: String =
      s"OwnerTree(${addr.index}, ${end.index}, ${if (myChildren === null) "?" else myChildren.mkString(" ")})"
  }

  def symFromNoCycle(noCycle: NoCycle): Symbol = symAtAddr(noCycle.at)
}

object TreeUnpickler {

  sealed trait MaybeCycle
  object MaybeCycle {
    case class  NoCycle(at: Addr) extends MaybeCycle
    case object Tombstone         extends MaybeCycle
  }

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}
