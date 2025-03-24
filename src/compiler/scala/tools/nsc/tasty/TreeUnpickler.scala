/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty

import scala.tools.tasty.{TastyRefs, TastyReader, TastyName, TastyFormat, TastyFlags}
import TastyRefs._, TastyFlags._, TastyFormat._
import ForceKinds._

import scala.annotation.{switch, unused}
import scala.collection.mutable
import scala.reflect.io.AbstractFile
import scala.reflect.internal.Variance
import scala.util.chaining._
import scala.collection.immutable.ArraySeq

/**`TreeUnpickler` is responsible for traversing all trees in the "ASTs" section of a TASTy file, which represent the
 * definitions inside the classfile associated with the root class/module. `TreeUnpickler` will enter the public api
 * of the TASTy file into the symbolTable of `TastyUniverse`. "Public API" includes annotations when they are
 * simple trees.
 *
 * Where possible, `TreeUnpickler` should not directly manipulate values created by the symbolTable, but use
 * operations provided by `TastyUniverse`
 *  @param reader    the reader from which to unpickle
 *  @param nameAtRef an index of names from the tasty file of this unpickler
 *  @param tasty     the handle on the `TastyUniverse`
 */
class TreeUnpickler[Tasty <: TastyUniverse](
    reader: TastyReader,
    nameAtRef: NameRef => TastyName)(implicit
    val tasty: Tasty) { self =>
  import tasty._
  import TreeUnpickler._
  import MaybeCycle._
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

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private[this] var ownerTree: OwnerTree = _

  //---------------- unpickling trees ----------------------------------------------------------------------------------

  private def registerSym(addr: Addr, sym: Symbol, rejected: Boolean)(implicit ctx: Context) = {
    assert(!(rejected && isSymbol(sym)), "expected no symbol when rejected")
    ctx.log(
      if (isSymbol(sym)) s"$addr registered ${showSym(sym)}"
      else s"$addr registering symbol was rejected"
    )
    symAtAddr(addr) = sym
  }

  /** Enter all toplevel classes and objects into their scopes
   */
  def enterTopLevel(classRoot: Symbol, objectRoot: Symbol)(implicit ctx: Context): Unit = {
    this.roots = Set(objectRoot, classRoot)
    val rdr = new TreeReader(reader).fork
    ownerTree = new OwnerTree(NoAddr, 0, rdr.fork, reader.endAddr)
    def indexTopLevel()(implicit ctx: Context): Unit = rdr.indexStats(reader.endAddr)
    if (rdr.isTopLevel) {
      inIndexScopedStatsContext { ctx0 =>
        ctx0.trace(traceTopLevel(classRoot, objectRoot)) {
          indexTopLevel()(ctx0)
        }
      }
    }
  }

  private def traceTopLevel(classRoot: Symbol, objectRoot: Symbol) = TraceInfo[Unit](
    query = s"reading top level roots",
    qual = s"${showSym(classRoot)}, ${showSym(objectRoot)}",
    res = _ => "entered top level roots"
  )

  /** A completer that captures the current position and context, which then uses the position to discover the symbol
   *  to compute the info for.
   */
  class Completer(
      isClass: Boolean,
      reader: TastyReader,
      tflags: TastyFlagSet
  )(implicit ctx: Context)
      extends TastyCompleter(isClass, tflags) {

    private val symAddr = reader.currentAddr

    private def fork(reader: TastyReader): TastyReader = reader.subReader(reader.startAddr, reader.endAddr)

    def computeInfo(sym: Symbol)(implicit ctx: Context): Unit = {
      // implicit assertion that the completion is done by the same mirror that loaded owner
      require(symAtAddr(symAddr) eq sym)
      cycleAtAddr(symAddr) = ctx.withPhaseNoLater("pickler") { ctx0 =>
        new TreeReader(fork(reader)).readIndexedMember()(ctx0) // fork here so that cycles start at the same address
      }
    }

  }

  class TreeReader(val reader: TastyReader) {
    import reader._

    def forkAt(start: Addr): TreeReader = new TreeReader(subReader(start, endAddr))
    def fork: TreeReader = forkAt(currentAddr)

    def skipParentTree(tag: Int): Unit = if (tag != SPLITCLAUSE) skipTree(tag)
    def skipParentTree(): Unit = skipParentTree(readByte())

    def skipTree(tag: Int): Unit =
      if (tag >= firstLengthTreeTag) goto(readEnd())
      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
      else if (tag >= firstASTTreeTag) skipTree()
      else if (tag >= firstNatTreeTag) readNat()

    def skipTree(): Unit = skipTree(readByte())

    def skipParams(): Unit =
      while ({
        val tag = nextByte
        tag == PARAM || tag == TYPEPARAM || tag == EMPTYCLAUSE || tag == SPLITCLAUSE
      }) skipTree()

    /** Record all directly nested definitions and templates in current tree
     *  as `OwnerTree`s in `buf`.
     *  A complication concerns member definitions. These are lexically nested in a
     *  Template node, but need to be listed separately in the OwnerTree of the enclosing class
     *  in order not to confuse owner chains.
     */
    def scanTree(buf: mutable.ListBuffer[OwnerTree], mode: MemberDefMode): Unit = {
      val start = currentAddr
      val tag = readByte()
      tag match {
        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | TEMPLATE =>
          val end = readEnd()
          for (_ <- 0 until numRefs(tag)) readNat()
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
              for (_ <- nrefs until 0) scanTree(buf, AllDefs)
              goto(end)
            }
            else {
              for (_ <- 0 until nrefs) readNat()
              if (tag === BIND) {
                // a Bind is never the owner of anything, so we set `end = start`
                buf += new OwnerTree(start, tag, fork, end = start)
              }

              scanTrees(buf, end, AllDefs)
            }
          }
          else if (tag >= firstNatASTTreeTag) { readNat(); scanTree(buf, AllDefs) }
          else if (tag >= firstASTTreeTag) scanTree(buf, AllDefs)
          else if (tag >= firstNatTreeTag) readNat()
      }
    }

    /** Record all directly nested definitions and templates between current address and `end`
     *  as `OwnerTree`s in `buf`
     */
    def scanTrees(buf: mutable.ListBuffer[OwnerTree], end: Addr, mode: MemberDefMode): Unit = {
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

    /** Read names in an interleaved sequence of types/bounds and (parameter) names,
      *  possibly followed by a sequence of modifiers.
      */
    def readParamNamesAndMods(end: Addr): (ArraySeq[TastyName], TastyFlagSet) = {
      val names =
        collectWhile(currentAddr != end && !isModifierTag(nextByte)) {
          skipTree()
          readTastyName()
        }
      var mods = EmptyTastyFlags
      while (currentAddr != end) { // avoid boxing the mods
        readByte() match {
          case IMPLICIT => mods |= Implicit
          case ERASED   => mods |= Erased
          case GIVEN    => mods |= Given
        }
      }
      (names.to(ArraySeq), mods)
    }

    /** Read `n` parameter types or bounds which are interleaved with names */
    def readParamTypes(ps: ArraySeq[Symbol])(implicit ctx: Context): ArraySeq[Type] = {
      def inner(ps1: Iterator[Symbol], buf: mutable.ArrayBuffer[Type]): ArraySeq[Type] = {
        if (ps1.isEmpty) buf.to(ArraySeq)
        else {
          val p = ps1.next()
          val rest = ps1
          val localCtx = ctx.withOwner(p)
          val t = readType()(localCtx)
          readNat() // skip name
          inner(rest, buf += t)
        }
      }
      inner(ps.iterator, new mutable.ArrayBuffer)
    }

    /** Read reference to definition and return symbol created at that definition */
    def readSymRef()(implicit ctx: Context): Symbol = symbolAt(readAddr())

    /** The symbol at given address; create a new one if none exists yet */
    def symbolAt(addr: Addr)(implicit ctx: Context): Symbol = symAtAddr.get(addr) match {
      case Some(sym) =>
        sym
      case None =>
        ctx.trace(traceForwardReference(addr)) {
          val ctxAtOwner = ctx.withOwner(ownerTree.findOwner(addr))
          forkAt(addr).createSymbol()(ctxAtOwner)
        }
    }

    private def traceForwardReference(addr: Addr) = TraceInfo[Symbol](
      query = s"creating forward reference",
      qual = s"at $addr",
      res = sym => s"$addr forward reference to ${showSym(sym)}"
    )

    /** The symbol defined by current definition */
    def symbolAtCurrent()(implicit ctx: Context): Symbol = symAtAddr.get(currentAddr) match {
      case Some(sym) =>
        assert(ctx.owner === sym.owner, s"owner discrepancy for ${showSym(sym)}, expected: ${showSym(ctx.owner)}, found: ${showSym(sym.owner)}")
        sym
      case None =>
        ctx.trace(traceCurrentSymbol(currentAddr)) {
          createSymbol()
        }
    }

    private def traceCurrentSymbol(addr: Addr) = TraceInfo[Symbol](
      query = "create symbol at current address",
      qual = s"$addr",
      res = sym => if (!isSymbol(sym)) s"evicted symbol at $addr" else s"created ${showSym(sym)} at $addr"
    )

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
    }

    /** Read a type */
    def readType()(implicit ctx: Context): Type = {
      val start = currentAddr
      val tag = readByte()

      def traceReadType = TraceInfo[Type](
        query = "reading type",
        qual = s"${astTagToString(tag)} $start",
        res = tpe => s"exit ${showType(tpe)} ${astTagToString(tag)} $start"
      )

      def registeringTypeWith[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }

      def readLengthType(): Type = {
        val end = readEnd()

        def readMethodic[N <: TastyName](
            factory: LambdaFactory[N],
            parseFlags: FlagSets.FlagParser,
            nameMap: TastyName => N
        )(implicit ctx: Context): Type = {
          val result = typeAtAddr.getOrElse(start, {
            // TODO [tasty]: can we share LambdaTypes/RecType/RefinedType safely
            // under a new context owner? (aka when referenced by a `SHAREDtype`).
            // So far this has been safe to do, but perhaps with macros comparing the
            // owners of the symbols of PolyTypes maybe not?
            // one concrete example where TypeLambdaType is shared between two unrelated classes:
            // - test/tasty/run/src-3/tastytest/issue12420/ShareLambda.scala
            val nameReader = fork
            nameReader.skipTree() // skip result
            val paramReader = nameReader.fork
            val (paramNames, mods) = nameReader.readParamNamesAndMods(end)
            LambdaFactory.parse(factory, paramNames.map(nameMap), parseFlags(mods)(ctx))(
              ps => paramReader.readParamTypes(ps),
              () => readType(),
              pt => typeAtAddr(start) = pt, // register the lambda so that we can access its parameters
            )
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
            case TERMREFin =>
              defn.TermRefIn(name = readTastyName(), prefix = readType(), space = readType())
            case TYPEREFin =>
              defn.TypeRefIn(
                name = readTastyName().toTypeName, prefix = readType(), space = readType())
            case REFINEDtype =>
              var name   = readTastyName()
              val parent = readType()
              if (nextUnsharedTag === TYPEBOUNDS) name = name.toTypeName
              ctx.enterRefinement(parent)(refinedCtx =>
                defn.RefinedType(parent, name, refinedCtx.owner, readType())
              )
            case APPLIEDtype => defn.AppliedType(readType(), until(end)(readType()))
            case TYPEBOUNDS =>
              val lo = readType()
              if (nothingButMods(end)) readVariances(lo)
              else defn.TypeBounds(lo, readVariances(readType()))
            case ANNOTATEDtype => defn.AnnotatedType(readType(), readTerm()(ctx.addMode(ReadAnnotTopLevel)))
            case ANDtype => defn.IntersectionType(readType(), readType())
            case ORtype => unionIsUnsupported
            case SUPERtype => defn.SuperType(readType(), readType())
            case MATCHtype | MATCHCASEtype => matchTypeIsUnsupported
            case POLYtype => readMethodic(PolyTypeLambda, FlagSets.addDeferred, _.toTypeName)
            case METHODtype => readMethodic(MethodTermLambda, FlagSets.parseMethod, id)
            case TYPELAMBDAtype => readMethodic(HKTypeLambda, FlagSets.addDeferred, _.toTypeName)
            case PARAMtype => defn.ParamRef(readTypeRef(), readNat()) // reference to a parameter within a LambdaType
            case FLEXIBLEtype =>
              // dotty would wrap the inner type in FlexibleType (with lower bound >: tpe | Null),
              // but we can leave as-is - as Scala 2 does not have explicit nulls.
              readType()
          }
        assert(currentAddr === end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      def readSimpleType(): Type = {
        (tag: @switch) match {
          case TYPEREFdirect => defn.NamedType(defn.NoPrefix, readSymRef())
          case TERMREFdirect => defn.NamedType(defn.NoPrefix, readSymRef())
          case TYPEREFsymbol | TERMREFsymbol => defn.NamedType(sym = readSymRef(), prefix = readType())
          case TYPEREFpkg => defn.NamedType(defn.NoPrefix, sym = readPackageRef().objectImplementation)
          case TERMREFpkg => defn.NamedType(defn.NoPrefix, sym = readPackageRef())
          case TYPEREF => defn.TypeRef(name = readTastyName().toTypeName, prefix = readType())
          case TERMREF => defn.TermRef(name = readTastyName(), prefix = readType())
          case THIS => defn.ThisType(readType())
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
          case RECthis => defn.RecThis(readTypeRef())
          case SHAREDtype =>
            val ref = readAddr()
            // TODO [tasty]: there are enough cases now to suggest that ultimately
            // nsc is not designed around cached types, e.g.
            // - unique symbols for wildcard type arguments
            // - context-sensitive substitutions of types we need to make,
            //   which may propagate incorrect semantics to other sites if the type is shared
            // so we should probably remove this,
            // however as of writing removing caching breaks `TastyTestJUnit.run`,
            // so further investigation is needed.
            typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
          case BYNAMEtype => defn.ByNameType(readType())
          case _ => defn.ConstantType(readConstant(tag))
        }
      }
      ctx.traceV(traceReadType) {
        if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
      }
    }

    private def readPackageRef()(implicit ctx: Context): Symbol = {
      ctx.requiredPackage(readTastyName())
    }

    def readTypeRef(): Type = typeAtAddr(readAddr())

// ------ Reading definitions -----------------------------------------------------

    private def nothingButMods(end: Addr): Boolean =
      currentAddr === end || isModifierTag(nextByte)

    private def normalizeName(isType: Boolean, name: TastyName)(implicit ctx: Context): TastyName = {
      val prior = if (ctx.owner.isTrait && name === TastyName.Constructor) TastyName.MixinConstructor else name
      if (isType) prior.toTypeName else prior
    }

    private def addInferredFlags(tag: Int, tastyFlags: TastyFlagSet, name: TastyName, isAbsType: Boolean, isClass: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): TastyFlagSet = {
      var flags = tastyFlags
      if (flags.is(Given))
        flags |= Implicit
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !flags.isOneOf(FlagSets.TermParamOrAccessor) ||
        isAbsType ||
        flags.is(Opaque) && !isClass
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (isClass && flags.is(Trait)) flags |= Abstract
      if (tag === DEFDEF) {
        flags |= Method
        if (name.isDefaultName)
          flags |= HasDefault // this corresponds to DEFAULTPARAM
        if (ctx.isJava && !lacksDefinition && ctx.owner.is(Trait) && !name.isConstructorName)
          flags |= HasDefault // will be replaced by JAVA_DEFAULTMETHOD
      }
      if (tag === VALDEF) {
        if (flags.is(Inline) || ctx.owner.is(Trait))
          flags |= FieldAccessor
        if (flags.not(Mutable))
          flags |= Stable
        if (flags.is(Case | Enum)) // singleton enum case
          flags |= Object | Stable // encode as a module (this needs to be corrected in bytecode)
      }
      if (ctx.owner.isClass) {
        if (tag === TYPEPARAM) flags |= Param
        else if (tag === PARAM) {
          flags |= ParamSetter | FieldAccessor | Stable
          if (!rhsIsEmpty) // param alias
            flags |= Method
        }
      }
      else if (isParamTag(tag)) flags |= Param
      if (flags.is(Object)) flags |= (if (tag === VALDEF) FlagSets.Creation.ObjectDef else FlagSets.Creation.ObjectClassDef)
      flags
    }

    def isAbstractType(@unused ttag: Int)(implicit ctx: Context): Boolean = nextUnsharedTag match {
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
      case TEMPLATE =>
        val localDummy = ctx.newLocalDummy
        registerSym(currentAddr, localDummy, rejected = false)
        localDummy
      case tag =>
        assert(tag != BIND, "bind pattern symbol creation from TASTy")
        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
    }

    /** Create symbol of member definition or parameter node and enter in symAtAddr map
     *  @return  the created symbol
     */
    def createMemberSymbol()(implicit ctx: Context): Symbol = {

      def rejectSymbol(owner: Symbol, name: TastyName, flags: TastyFlagSet): Boolean = {
        def isPureMixinCtor =
          name == TastyName.MixinConstructor && owner.isTrait && flags.is(Stable)
        def isInvisible =
          flags.is(Invisible)

        isPureMixinCtor || isInvisible
      }

      val start = currentAddr
      val tag = readByte()
      def isTypeTag = tag === TYPEDEF || tag === TYPEPARAM
      val end = readEnd()
      val parsedName: TastyName = readTastyName()
      ctx.log(s"${astTagToString(tag)} ${parsedName.debug} in ${location(ctx.owner)}")
      skipParams()
      val ttag = nextUnsharedTag
      val isAbsType = isAbstractType(ttag)
      val isClass = ttag === TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsIsEmpty = nothingButMods(end)
      if (!rhsIsEmpty) skipTree()
      val (parsedFlags0, annotations, privateWithin) =
        readModifiers(end, readTypedAnnot, readTypedWithin, noSymbol)
      val name = normalizeName(isTypeTag, parsedName)
      val flags = addInferredFlags(tag, parsedFlags0, name, isAbsType, isClass, rhsIsEmpty)
      def mkCompleter = new Completer(isClass, subReader(start, end), flags)(ctx.retractMode(IndexScopedStats))
      def isTypeParameter = flags.is(Param) && isTypeTag
      def canEnterInClass = !isTypeParameter
      ctx.log {
        val privateFlag = {
          if (isSymbol(privateWithin)) {
            if (flags.is(Protected)) s"Protected[$privateWithin]"
            else s"Private[$privateWithin]"
          }
          else {
            ""
          }
        }
        val debugFlags = {
          if (privateFlag.nonEmpty) {
            val flags0 = flags &~ Protected
            val rest = if (!flags0) "" else s" ${flags0.debug}"
            privateFlag + rest
          }
          else flags.debug
        }
        s"""$start parsed flags $debugFlags"""
      }
      val rejected = rejectSymbol(ctx.owner, name, flags)
      val sym = {
        if (tag === TYPEPARAM && ctx.owner.isConstructor) {
          // TASTy encodes type parameters for constructors
          // nsc only has class type parameters
          val tparam = ctx.findOuterClassTypeParameter(name.toTypeName)
          ctx.log(s"$start reusing class type param ${showSym(tparam)}")
          tparam
        }
        else {
          ctx.findRootSymbol(roots, name) match {
            case Some(rootd) =>
              roots -= rootd
              if (rejected) {
                ctx.evict(rootd)
                noSymbol
              }
              else {
                ctx.redefineSymbol(rootd, mkCompleter, privateWithin)
                ctx.log(s"$start replaced info of root ${showSym(rootd)}")
                rootd
              }
            case _ =>
              if (rejected) noSymbol
              else if (isClass) ctx.delayClassCompletion(ctx.owner, name.toTypeName, mkCompleter, privateWithin)
              else ctx.delayCompletion(ctx.owner, name, mkCompleter, privateWithin)
          }
        }
      }
      registerSym(start, sym, rejected)
      if (isSymbol(sym)) {
        if (tag == VALDEF && flags.is(FlagSets.SingletonEnum))
          ctx.markAsEnumSingleton(sym)
        if (canEnterInClass && ctx.owner.isClass)
          ctx.enterIfUnseen(sym)
        if (isClass) {
          ctx.log(s"$templateStart indexing params (may be empty):")
          val localCtx = ctx.withOwner(sym)
          forkAt(templateStart).indexTemplateParams()(localCtx)
        }
        ctx.adjustAnnotations(sym, annotations)
      }
      goto(start)
      sym
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin
     *  boundary symbol.
     */
    def readModifiers[WithinType]
        (end: Addr, readAnnot: Context => DeferredAnnotation, readWithin: Context => WithinType, defaultWithin: WithinType)
        (implicit ctx: Context): (TastyFlagSet, List[DeferredAnnotation], WithinType) = {
      var flags = EmptyTastyFlags
      var annotFns: List[DeferredAnnotation] = Nil
      var privateWithin = defaultWithin
      while (currentAddr.index != end.index) {
        def addFlag(flag: TastyFlagSet) = {
          flags |= flag
          readByte()
        }
        nextByte match {
          case PRIVATE => addFlag(Private)
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
          case ERASED => addFlag(Erased)
          case LAZY => addFlag(Lazy)
          case OVERRIDE => addFlag(Override)
          case INLINE => addFlag(Inline)
          case INLINEPROXY => addFlag(InlineProxy)
          case MACRO => addFlag(Macro)
          case OPAQUE => addFlag(Opaque)
          case STATIC => addFlag(Static)
          case OBJECT => addFlag(Object)
          case TRAIT => addFlag(Trait)
          case TRANSPARENT => addFlag(Transparent)
          case INFIX => addFlag(Infix)
          case ENUM => addFlag(Enum)
          case LOCAL => addFlag(Local)
          case SYNTHETIC => addFlag(Synthetic)
          case ARTIFACT => addFlag(Artifact)
          case MUTABLE => addFlag(Mutable)
          case FIELDaccessor => addFlag(FieldAccessor)
          case CASEaccessor => addFlag(CaseAccessor)
          case COVARIANT => addFlag(Covariant)
          case CONTRAVARIANT => addFlag(Contravariant)
          case HASDEFAULT => addFlag(HasDefault)
          case STABLE => addFlag(Stable)
          case EXTENSION => addFlag(Extension)
          case GIVEN => addFlag(Given)
          case PARAMsetter => addFlag(ParamSetter)
          case PARAMalias => addFlag(ParamAlias)
          case EXPORTED => addFlag(Exported)
          case OPEN => addFlag(Open)
          case INVISIBLE => addFlag(Invisible)
          case TRACKED => addFlag(Tracked)
          case PRIVATEqualified =>
            readByte()
            privateWithin = readWithin(ctx)
          case PROTECTEDqualified =>
            addFlag(Protected)
            privateWithin = readWithin(ctx)
          case ANNOTATION =>
            annotFns = readAnnot(ctx) :: annotFns
          case tag =>
            assert(false, s"illegal modifier tag ${astTagToString(tag)} at $currentAddr, end = $end")
        }
      }
      (flags, if (ctx.ignoreAnnotations) Nil else annotFns.reverse, privateWithin)
    }

    private val readTypedWithin: Context => Symbol = implicit ctx => readType().typeSymbolDirect

    private val readTypedAnnot: Context => DeferredAnnotation = { implicit ctx =>
      val annotCtx = ctx.addMode(ReadAnnotTopLevel)
      val start = currentAddr
      readByte() // tag
      val end      = readEnd()
      val annotSym = readType()(annotCtx).typeSymbolDirect
      val annotStart = currentAddr
      ctx.log(s"$annotStart collected annotation ${showSym(annotSym)}, starting at $start, ending at $end")
      val mkTree = readLaterWithOwner(end, rdr => ctx =>
        ctx.trace(traceAnnotation(annotStart, annotSym, ctx.owner)(ctx)) {
          rdr.readTerm()(ctx)
        }
      )(annotCtx.retractMode(IndexScopedStats))
      DeferredAnnotation.fromTree(annotSym)(mkTree)
    }

    private def traceAnnotation(annotStart: Addr, annotSym: Symbol, annotee: Symbol)(implicit ctx: Context) = TraceInfo[Tree](
      query = s"reading annotation tree",
      qual = s"${showSym(annotSym)} at $annotStart",
      res = atree => s"annotation of ${showSym(annotee)} = ${showTree(atree)}"
    )

    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     */
    def indexStats(end: Addr)(implicit ctx: Context): Unit = {
      while (currentAddr.index < end.index) {
        nextByte match {
          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
            symbolAtCurrent()
            skipTree()
          case IMPORT | EXPORT =>
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
     *   - an end address,
     *   - a context which has the processed package as owner
     */
    def processPackage[T](op: Addr => Context => T)(implicit ctx: Context): T = {
      readByte() // tag
      val end = readEnd()
      val tpe = readType()
      op(end)(ctx.withOwner(tpe.typeSymbolDirect.objectImplementation))
    }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag` starting at current address.
     */
    def indexParams(tag: Int)(implicit ctx: Context): Unit = {
      while (nextByte === tag) {
        symbolAtCurrent()
        skipTree()
      }
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
        val noCycle = initializeMember()
        cycleAtAddr.remove(start)
        noCycle
    }

    private def initializeMember()(implicit ctx: Context): NoCycle = {
      val symAddr = currentAddr
      val tag     = readByte()
      val end     = readEnd()
      val tname   = readTastyName()
      val sym     = symAtAddr(symAddr)

      def readParamss()(implicit ctx: Context): List[List[NoCycle]] = {
        def readRest() = {
          if (nextByte == SPLITCLAUSE) readByte()
          readParamss()
        }
        nextByte match {
          case PARAM => readParams[NoCycle](PARAM) :: readRest()
          case TYPEPARAM => readParams[NoCycle](TYPEPARAM) :: readRest()
          case EMPTYCLAUSE => readByte(); Nil :: readRest()
          case _ => Nil
        }
      }

      def checkUnsupportedFlags(unsupported: TastyFlagSet)(implicit ctx: Context): Unit = {
        unsupportedWhen(unsupported.hasFlags, s"${showTasty(unsupported)} ${sym.kindString} $tname")
      }

      def DefDef(repr: TastyRepr, localCtx: Context)(implicit ctx: Context): Unit = {
        val isMacro = repr.tflags.is(Erased | Macro)
        val supportedFlags = Extension | Exported | Infix | Given | optFlag(isMacro)(Erased)
        checkUnsupportedFlags(repr.unsupportedFlags &~ supportedFlags)
        val isCtor = sym.isConstructor
        val paramss = readParamss()(localCtx)
        val typeClause = {
          // A type parameter list must be non-empty and with type symbols
          val first = paramss.take(1)
          if (first.exists(_.headOption.exists(nc => symFromNoCycle(nc).isType))) first.head else Nil
        }
        val valueClauses = paramss.drop(if (typeClause.isEmpty) 0 else 1)
        val typeParams = typeClause.map(symFromNoCycle)
        val vparamss = {
          val vparamSymss = valueClauses.map(_.map(symFromNoCycle))
          // A value parameter list may be empty, or filled with term symbols
          val hasTypeParams = vparamSymss.exists(_.headOption.exists(_.isType))
          unsupportedWhen(hasTypeParams, {
            val noun = (
              if (isCtor) "constructor"
              else if (repr.unsupportedFlags.is(Extension)) "extension method"
              else "method"
            )
            s"$noun with unmergeable type parameters: $tname"
          })
          vparamSymss
        }
        val tpt = readTpt()(localCtx)
        if (isMacro) {
          val impl  = tpd.Macro(readTerm()(ctx.addMode(ReadMacro)))
          val annot = symbolTable.AnnotationInfo(
            atp    = symbolTable.definitions.MacroImplLocationAnnotation.tpe,
            args   = List(impl),
            assocs = Nil
          )
          sym.addAnnotation(annot)
        }
        val valueParamss = normalizeIfConstructor(sym.enclClass, vparamss, valueClauses, isCtor)
        val resType = effectiveResultType(sym, tpt.tpe)
        ctx.setInfo(sym, defn.DefDefType(if (isCtor) Nil else typeParams, valueParamss, resType))
      }

      def ValDef(repr: TastyRepr, localCtx: Context)(implicit ctx: Context): Unit = {
        // valdef in TASTy is either a singleton object or a method forwarder to a local value.
        checkUnsupportedFlags(repr.unsupportedFlags &~ (Enum | Extension | Exported | Given))
        val tpe = readTpt()(localCtx).tpe
        ctx.setInfo(sym,
          if (repr.tflags.is(FlagSets.SingletonEnum)) {
            ctx.completeEnumSingleton(sym, tpe)
            defn.NamedType(sym.owner.thisPrefix, sym.objectImplementation)
          }
          else if (ctx.isJava && repr.tflags.is(FlagSets.JavaEnumCase)) defn.ConstantType(tpd.Constant(sym))
          else if (!ctx.isJava && sym.isFinal && isConstantType(tpe)) defn.InlineExprType(tpe)
          else if (sym.isMethod) defn.ExprType(tpe)
          else tpe
        )
      }

      def TypeDef(repr: TastyRepr, localCtx: Context)(implicit ctx: Context): Unit = {
        val allowedShared = Enum | Opaque | Infix | Given
        val allowedTypeFlags = allowedShared | Exported
        val allowedClassFlags = allowedShared | Open | Transparent | Tracked
        if (sym.isClass) {
          checkUnsupportedFlags(repr.unsupportedFlags &~ allowedClassFlags)
          sym.owner.ensureCompleted(CompleteOwner)
          readTemplate()(localCtx)
        }
        else {
          checkUnsupportedFlags(repr.unsupportedFlags &~ allowedTypeFlags)
          sym.info = defn.InitialTypeInfo // needed to avoid cyclic references when unpickling rhs, see dotty_i3816.scala
          val rhs = readTpt()(if (repr.tflags.is(Opaque)) localCtx.addMode(OpaqueTypeDef) else localCtx)
          val info =
            if (repr.tflags.is(Opaque)) {
              val (info, alias) = defn.OpaqueTypeToBounds(rhs.tpe)
              ctx.markAsOpaqueType(sym, alias)
              info
            }
            else rhs.tpe
          ctx.setInfo(sym, defn.NormalisedBounds(info, sym))
          if (sym.is(Param)) sym.reset(Private | Protected)
        }
      }

      def TermParam(repr: TastyRepr, localCtx: Context)(implicit ctx: Context): Unit = {
        checkUnsupportedFlags(repr.unsupportedFlags &~ (ParamAlias | Exported | Given | Tracked))
        val tpt = readTpt()(localCtx)
        ctx.setInfo(sym,
          if (nothingButMods(end) && sym.not(ParamSetter)) tpt.tpe
          else defn.ExprType(tpt.tpe))
      }

      def initialize(localCtx: Context)(implicit ctx: Context) = ctx.trace(traceCompletion(symAddr, sym)) {
        sym.rawInfo match {
          case repr: TastyRepr =>
            tag match {
              case DEFDEF              => DefDef(repr, localCtx)
              case VALDEF              => ValDef(repr, localCtx)
              case TYPEDEF | TYPEPARAM => TypeDef(repr, localCtx)
              case PARAM               => TermParam(repr, localCtx)
            }
            repr.tflags
          case _ => // nothing to do here (assume correctly initalised)
            ctx.log(s"${showSym(sym)} is already initialised, in owner ${showSym(sym.owner)}")
            EmptyTastyFlags
        }
      }

      try {
        val localCtx = ctx.withOwner(sym)
        val tflags = {
          if (sym.isClass) {
            inIndexScopedStatsContext(localCtx0 => initialize(localCtx0)(ctx))(localCtx)
          }
          else {
            initialize(localCtx)
          }
        }
        NoCycle(at = symAddr, tflags)
      }
      catch ctx.onCompletionError(sym)
      finally goto(end)
    }

    private def traceCompletion(addr: Addr, sym: Symbol)(implicit ctx: Context) = TraceInfo[TastyFlagSet](
      query = "begin completion",
      qual = s"${showSym(sym)} in context ${showSym(ctx.owner)} $addr",
      res = _ => s"completed ${showSym(sym)}: ${showType(sym.info)}"
    )

    private def readTemplate()(implicit ctx: Context): Unit = {
      val start = currentAddr
      val cls = ctx.enterClassCompletion()
      val localDummy = symbolAtCurrent()
      assert(readByte() === TEMPLATE)
      val end = readEnd()

      def traceCompleteParams = TraceInfo[List[Symbol]](
        query = "force template parameters",
        qual = s"${showSym(cls)} $currentAddr",
        res = _ => "forced template parameters"
      )

      def traceIndexMembers = TraceInfo[Unit](
        query = "index template body",
        qual = s"${showSym(cls)} $currentAddr",
        res = _ => "indexed template body"
      )

      def traceCollectParents = TraceInfo[List[Type]](
        query = "collect template parents",
        qual = s"${showSym(cls)} $currentAddr",
        res = { parentTypes =>
          val addendum = parentTypes.map(lzyShow).mkString(s"`${cls.fullName} extends ", " with ", "`")
          s"collected template parents $addendum"
        }
      )

      def traceReadSelf = TraceInfo[Type](
        query = "reading template self-type",
        qual = s"${showSym(cls)} $currentAddr",
        res = tpe => s"template self-type is $tpe"
      )

      def completeParameters()(implicit ctx: Context): List[Symbol] = ctx.trace(traceCompleteParams) {
        val tparams = readIndexedParams[NoCycle](TYPEPARAM).map(symFromNoCycle)
        if (tparams.nonEmpty) {
          cls.info = defn.PolyType(tparams, cls.info)
        }
        readIndexedParams[NoCycle](PARAM) // skip value parameters
        tparams
      }

      def indexMembers()(implicit ctx: Context): Unit = ctx.trace(traceIndexMembers) {
        val bodyIndexer = fork
        while ({val tag = bodyIndexer.reader.nextByte; tag != DEFDEF && tag != SPLITCLAUSE})
          bodyIndexer.skipParentTree() // skip until primary ctor
        bodyIndexer.indexStats(end)
      }

      def collectParents()(implicit ctx: Context): List[Type] = ctx.trace(traceCollectParents) {
        val parentCtx = ctx.withOwner(localDummy).addMode(ReadParents)
        val parentWithOuter = parentCtx.addMode(OuterTerm)
        collectWhile({val tag = nextByte; tag != SELFDEF && tag != DEFDEF && tag != SPLITCLAUSE}) {
          defn.adjustParent(
            nextUnsharedTag match {
              case APPLY | TYPEAPPLY | BLOCK => readTerm()(parentWithOuter).tpe
              case _ => readTpt()(parentCtx).tpe
            }
          )
        }
      }

      def addSelfDef()(implicit ctx: Context): Unit = {
        val selfTpe = ctx.trace(traceReadSelf) {
          readByte() // read SELFDEF tag
          readLongNat() // skip Name
          readTpt().tpe
        }
        cls.typeOfThis = selfTpe
      }

      def setInfoWithParents(tparams: List[Symbol], parentTypes: List[Type])(implicit ctx: Context): Unit = {
        val info = {
          val classInfo = defn.ClassInfoType(parentTypes, cls)
          // TODO [tasty]: if support opaque types, refine the self type with any opaque members here
          if (tparams.isEmpty) classInfo
          else defn.PolyType(tparams, classInfo)
        }
        ctx.setInfo(cls, info)
      }

      def traverseTemplate()(implicit ctx: Context): Unit = {
        val tparams = completeParameters()
        indexMembers()
        val parents = collectParents()
        if (nextByte === SELFDEF) {
          addSelfDef()
        }
        if (nextByte === SPLITCLAUSE) {
          assert(ctx.isJava,  s"unexpected SPLITCLAUSE at $start")
        }
        setInfoWithParents(tparams, ctx.processParents(cls, parents))
      }

      traverseTemplate()

    }

    def isTopLevel: Boolean = nextByte === IMPORT || nextByte === PACKAGE

    def readIndexedStatAsSym(@unused exprOwner: Symbol)(implicit ctx: Context): NoCycle = nextByte match {
      case TYPEDEF | VALDEF | DEFDEF =>
        readIndexedMember()
      case IMPORT =>
        unsupportedTermTreeError("import statement")
      case EXPORT =>
        unsupportedTermTreeError("export statement")
      case PACKAGE =>
        unsupportedTermTreeError("package statement")
      case _ =>
        skipTree() // readTerm()(ctx.withOwner(exprOwner))
        NoCycle(at = NoAddr, tflags = EmptyTastyFlags)
    }

    def readIndexedStatsAsSyms(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[NoCycle] =
      until(end)(readIndexedStatAsSym(exprOwner))

    def readStatsAsSyms(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[NoCycle] = {
      def forkAndIndexStats(implicit ctx: Context): Unit = fork.indexStats(end)
      inIndexStatsContext(forkAndIndexStats(_))
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

    def readTerm()(implicit ctx: Context): Tree = {
      val start = currentAddr
      val tag = readByte()

      def traceReadTerm = TraceInfo[Tree](
        query = "reading term",
        qual = s"${astTagToString(tag)} $start",
        res = tree => s"exit term (`${showTree(tree)}`: ${showType(tree.tpe)}) ${astTagToString(tag)} $start"
      )

      def inParentCtor = ctx.mode.is(ReadParents | OuterTerm)

      def readPathTerm(): Tree = {
        goto(start)
        tpd.PathTree(readType())
      }

      def readQualId(): (TastyName.TypeName, Type) = {
        val qual = readTerm()
        (qual.typeIdent, defn.ThisType(qual.tpe))
      }

      def completeSelectType(name: TastyName.TypeName)(implicit ctx: Context): Tree =
        completeSelect(name)

      def completeSelect(name: TastyName)(implicit ctx: Context): Tree =
        tpd.Select(readTerm(), name)

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
            case SELECTin =>
              val name = readTastyName()
              val qual = readTerm()
              if (inParentCtor) {
                assert(name.isSignedConstructor, s"Parent of ${ctx.owner} is not a constructor.")
                skipTree()
                qual
              }
              else {
                tpd.Select(readType())(qual, name)
              }
            case SUPER =>
              val qual = readTerm()
              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (TastyName.EmptyTpe, defn.NoType))
              tpd.Super(qual, mixId)(mixTpe)
            case APPLY =>
              val fn = readTerm()
              if (inParentCtor) {
                until(end)(skipTree())
                tpd.TypeTree(fnResult(fn.tpe))
              } else {
                val argsCtx = ctx.argumentCtx(fn)
                tpd.Apply(fn, until(end)(readTerm()(argsCtx)))
              }
            case TYPEAPPLY => tpd.TypeApply(readTerm(), until(end)(readTpt()))
            case APPLYsigpoly =>
              // this is skipped if it appears in parents, so only affects forced annotation trees
              signaturePolymorphicIsUnsupported
            case TYPED => tpd.Typed(readTerm(), readTpt())
            case IF =>
              if (nextByte === INLINE) unsupportedTermTreeError("inline conditional expression")
              else tpd.If(readTerm(), readTerm(), readTerm()) // if is ok if its parts are made of constants/paths
            case REPEATED =>
              val elemtpt = readTpt()
              tpd.SeqLiteral(until(end)(readTerm()), elemtpt)
            case REFINEDtpt =>
              val refineCls = symAtAddr.getOrElse(start, ctx.newRefinementClassSymbol)
              registerSym(start, refineCls, rejected = false)
              typeAtAddr(start) = refineCls.ref
              val parent = readTpt()
              ctx.withOwner(refineCls).enterRefinement(parent.tpe) { refinedCtx =>
                readStatsAsSyms(refineCls, end)(refinedCtx)
                tpd.RefinedTypeTree(parent, Nil, refineCls)
              }
            case APPLIEDtpt =>
              // If we do directly a tpd.AppliedType tree we might get a
              // wrong number of arguments in some scenarios reading F-bounded
              // types. This came up in #137 of collection strawman.
              tpd.AppliedTypeTree(readTpt(), until(end)(readTpt()))
            case ANNOTATEDtpt => tpd.Annotated(readTpt(), readTerm()(ctx.addMode(ReadAnnotTopLevel)))
            case LAMBDAtpt => tpd.LambdaTypeTree(readParams[NoCycle](TYPEPARAM).map(symFromNoCycle), readTpt())
            case MATCHtpt => matchTypeIsUnsupported
            case TYPEBOUNDStpt =>
              val lo = readTpt()
              val hi = if (currentAddr == end) lo else readTpt()

              val alias = {
                if (currentAddr == end) {
                  untpd.EmptyTree
                }
                else {
                  assert(ctx.mode.is(OpaqueTypeDef))
                  readTpt()(ctx.retractMode(OpaqueTypeDef))
                }
              }

              tpd.TypeBoundsTree(lo, hi, alias)
            case BLOCK =>
              if (inParentCtor | ctx.mode.is(ReadMacro)) {
                val exprReader = fork
                skipTree()
                until(end)(skipTree()) //val stats = readStats(ctx.owner, end)
                exprReader.readTerm()
              }
              else unsupportedTermTreeError("block expression")
            case ASSIGN        => unsupportedTermTreeError("assignment expression")
            case LAMBDA        => unsupportedTermTreeError("anonymous function literal")
            case MATCH         => unsupportedTermTreeError("match expression")
            case RETURN        => unsupportedTermTreeError("return statement")
            case WHILE         => unsupportedTermTreeError("loop statement")
            case TRY           => unsupportedTermTreeError("try expression")
            case BIND          => unsupportedTermTreeError("bind pattern")
            case ALTERNATIVE   => unsupportedTermTreeError("pattern alternative")
            case UNAPPLY       => unsupportedTermTreeError("unapply pattern")
            case INLINED       => unsupportedTermTreeError("inlined expression")
            case SELECTouter   => metaprogrammingIsUnsupported // only within inline
            case QUOTE         => abortQuote
            case SPLICE        => abortSplice
            case QUOTEPATTERN  => abortQuotePattern
            case SPLICEPATTERN => abortSplicePattern
            case HOLE          => abortMacroHole
            case _             => readPathTerm()
          }
        assert(currentAddr === end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      ctx.traceV(traceReadTerm) {
        if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm() // dotty sets span of tree to start
      }
    }

    def readTpt()(implicit ctx: Context): Tree = {
      val tpt: Tree = nextByte match {
        case SHAREDterm =>
          readByte()
          forkAt(readAddr()).readTpt()
        case BLOCK => // BLOCK appears in type position when quoting a type, but only in the body of a method
          metaprogrammingIsUnsupported
        case HOLE => abortMacroHole
        case tag  =>
          if (isTypeTreeTag(tag)) readTerm()(ctx.retractMode(OuterTerm))
          else {
            val tp = readType()
            if (isTypeType(tp)) tpd.TypeTree(tp) else untpd.EmptyTree
          }
      }
      tpt
    }

    /**
      * A HOLE should never appear in TASTy for a top level class, only in quotes.
      */
    private def abortMacroHole[T]: T = abortWith(msg = "Scala 3 macro hole in pickled TASTy")
    private def abortQuote[T]: T = abortWith(msg = "Scala 3 quoted expression in pickled TASTy")
    private def abortSplice[T]: T = abortWith(msg = "Scala 3 quoted splice in pickled TASTy")
    private def abortQuotePattern[T]: T = abortWith(msg = "Scala 3 quoted pattern in pickled TASTy")
    private def abortSplicePattern[T]: T = abortWith(msg = "Scala 3 quoted pattern splice in pickled TASTy")

    private def signaturePolymorphicIsUnsupported[T](implicit ctx: Context): T =
      unsupportedTermTreeError("signature polymorphic application")

    private def metaprogrammingIsUnsupported[T](implicit ctx: Context): T =
      unsupportedError("Scala 3 metaprogramming features")

    def readLaterWithOwner[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Symbol => Context => T = {
      val localReader = fork
      goto(end)
      owner => ctx0 => readWith(localReader, owner, ctx.mode, ctx.source, op)(ctx0)
    }

  }

  def readWith[T <: AnyRef](
    treader: TreeReader,
    owner: Symbol,
    mode: TastyMode,
    source: AbstractFile,
    op: TreeReader => Context => T)(
    implicit ctx: Context
  ): T = ctx.trace[T](traceReadWith(treader, mode, owner)) {
    ctx.withPhaseNoLater("pickler") { ctx0 =>
      op(treader)(ctx0
        .withOwner(owner)
        .withMode(mode)
        .withSource(source)
      )
    }
  }

  private def traceReadWith[T](treader: TreeReader, mode: TastyMode, owner: Symbol) = TraceInfo[T](
    query = "read within owner",
    qual = s"${showSym(owner)} with modes `${mode.debug}` at ${treader.reader.currentAddr}",
    res = _ => s"exiting sub reader"
  )

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
              assert(isSymbol(current), s"no symbol at $addr")
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
      try search(children, noSymbol).tap(owner => ctx.log(s"$addr within owner ${showSym(owner)} do:"))
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
    case class  NoCycle(at: Addr, tflags: TastyFlagSet) extends MaybeCycle
    case object Tombstone                               extends MaybeCycle
  }

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}
