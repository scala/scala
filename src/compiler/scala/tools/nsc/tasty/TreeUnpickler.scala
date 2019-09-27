package scala.tools.nsc
package tasty

import TastyRefs._
import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.reflect.io.AbstractFile

/** Unpickler for typed trees
 *  @param reader              the reader from which to unpickle
 *  @param posUnpicklerOpt     the unpickler for positions, if it exists
 *  @param commentUnpicklerOpt the unpickler for comments, if it exists
 *  @param splices
 */
abstract class TreeUnpickler(reader: TastyReader,
                             posUnpicklerOpt: Option[PositionUnpickler],
                             commentUnpicklerOpt: Option[CommentUnpickler],
                             splices: Seq[Any]) extends TastyUniverse with TastyNameTable { self =>
  import symbolTable._
  import TastyFormat._
  import FlagSets._
  import NameOps._
  import SymbolOps._
  import TreeUnpickler._
  import MaybeCycle._
  import TastyFlags._
  import Signature._
  import Contexts._

  @inline
  final protected def assertTasty(cond: Boolean, msg: => String)(implicit ctx: Context): Unit =
    if (!cond) {
      errorTasty(msg)
    }

  @inline
  final protected def errorTasty(msg: String)(implicit ctx: Context): Unit =
    reporter.error(NoPosition, s"Scala 2 incompatible TASTy signature of ${ctx.source.name} in ${ctx.owner}: $msg")

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
    ctx.log(s"registered $sym in ${sym.owner} at $addr")
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

//  /** The unpickled trees */
//  def unpickle(mode: UnpickleMode)(implicit ctx: Context): List[Tree] = {
//    assert(roots != null, "unpickle without previous enterTopLevel")
//    val rdr = new TreeReader(reader)
//    mode match {
//      case UnpickleMode.TopLevel => rdr.readTopLevel()
//      case UnpickleMode.Term => rdr.readTerm() :: Nil
//      case UnpickleMode.TypeTree => rdr.readTpt() :: Nil
//    }
//  }

  private def completeClassTpe1(implicit ctx: Context): ClassSymbol = {
    val cls = ctx.owner.asClass
    val assumedSelfType =
      if (cls.is(Module) && cls.owner.isClass) TypeRef(cls.owner.thisType, cls.name)
      else NoType
    cls.info = new ClassInfoType(cls.completer.parents, cls.completer.decls, assumedSelfType.typeSymbolDirect)
    cls
  }

  class Completer(reader: TastyReader, tastyFlagSet: TastyFlagSet)(implicit ctx: Context) extends TastyLazyType { self =>
    import reader._

    //    val owner = ctx.owner
    //    val source = ctx.source

    self.withTastyFlagSet(tastyFlagSet)

    override def complete(sym: Symbol): Unit = {
      cycleAtAddr(currentAddr) =
        Contexts.withPhaseNoLater(ctx.picklerPhase) { implicit ctx => // TODO really this needs to construct a new Context from the current symbolTable that this is completed from
          new TreeReader(reader).readIndexedMember()//(ctx.withOwner(owner).withSource(source))
        }
    }
  }

  /** A missing completer - from dotc */
  trait NoCompleter extends TastyLazyType {
    override def complete(sym: Symbol): Unit = throw new UnsupportedOperationException("complete")
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
      while (nextByte == PARAMS || nextByte == TYPEPARAM) skipTree()

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
          if (tag == TEMPLATE) {
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
          if (mode == MemberDefsOnly) skipTree(tag)
          else if (tag >= firstLengthTreeTag) {
            val end = readEnd()
            var nrefs = numRefs(tag)
            if (nrefs < 0) {
              for (i <- nrefs until 0) scanTree(buf)
              goto(end)
            }
            else {
              for (_ <- 0 until nrefs) readNat()
              if (tag == BIND) {
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
      assert(currentAddr.index == end.index)
    }

    /** The next tag, following through SHARED tags */
    def nextUnsharedTag: Int = {
      val tag = nextByte
      if (tag == SHAREDtype || tag == SHAREDterm) {
        val lookAhead = fork
        lookAhead.reader.readByte()
        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
      }
      else tag
    }

    def readName(): TermName = nameAtRef(readNameRef())
    def readSigName(): Either[SigName, TermName] = signedNameAtRef(readNameRef())

// ------ Reading types -----------------------------------------------------

   /** Read names in an interleaved sequence of (parameter) names and types/bounds */
   def readParamNames(end: Addr): List[Name] =
     until(end) {
       val name = readName()
       skipTree()
       name
     }

   /** Read types or bounds in an interleaved sequence of (parameter) names and types/bounds */
   def readParamTypes[T <: Type](end: Addr)(implicit ctx: Context): List[T] =
     until(end) { readNat(); readType().asInstanceOf[T] }

    /** Read reference to definition and return symbol created at that definition */
    def readSymRef()(implicit ctx: Context): Symbol = symbolAt(readAddr())

    /** The symbol at given address; createa new one if none exists yet */
    def symbolAt(addr: Addr)(implicit ctx: Context): Symbol = symAtAddr.get(addr) match {
      case Some(sym) =>
        sym
      case None =>
        val sym = forkAt(addr).createSymbol()(ctx.withOwner(ownerTree.findOwner(addr)))
        ctx.log(s"forward reference to $sym")
        sym
    }

    /** The symbol defined by current definition */
    def symbolAtCurrent()(implicit ctx: Context): Symbol = symAtAddr.get(currentAddr) match {
      case Some(sym) =>
        assert(ctx.owner == sym.owner, s"owner discrepancy for $sym, expected: ${ctx.owner}, found: ${sym.owner}")
        sym
      case None =>
        createSymbol()
    }

    def readConstant(tag: Int)(implicit ctx: Context): Constant = (tag: @switch) match {
      case UNITconst =>
        Constant(())
      case TRUEconst =>
        Constant(true)
      case FALSEconst =>
        Constant(false)
      case BYTEconst =>
        Constant(readInt().toByte)
      case SHORTconst =>
        Constant(readInt().toShort)
      case CHARconst =>
        Constant(readNat().toChar)
      case INTconst =>
        Constant(readInt())
      case LONGconst =>
        Constant(readLongInt())
      case FLOATconst =>
        Constant(java.lang.Float.intBitsToFloat(readInt()))
      case DOUBLEconst =>
        Constant(java.lang.Double.longBitsToDouble(readLongInt()))
      case STRINGconst =>
        Constant(readName().toString)
      case NULLconst =>
        Constant(null)
      case CLASSconst =>
        Constant(readType())
    }

    /** Read a type */
    def readType()(implicit ctx: Context): Type = {
      val start = currentAddr
      val tag = readByte()
      ctx.log(s"reading type ${astTagToString(tag)} at $start, ${ctx.source}")

      def registeringType[T](tp: Type, op: => T): T = {
        typeAtAddr(start) = tp
        op
      }

      def readLengthType(): Type = {
        val end = readEnd()

        def readMethodic[N <: Name, PInfo <: Type, LT <: LambdaType]
            (companion: LambdaTypeCompanion[N, PInfo, LT], nameMap: Name => N)(implicit ctx: Context): Type = {
          val result = typeAtAddr.getOrElse(start, {
            val nameReader = fork
            nameReader.skipTree() // skip result
            val paramReader = nameReader.fork
            val paramNames = nameReader.readParamNames(end).map(nameMap)
            companion(paramNames)(
              pt => registeringType(pt, paramReader.readParamTypes[PInfo](end)),
              pt => readType())
          })
          goto(end)
          result.asInstanceOf[LT].asReflectType(ctx.owner)
        }

        val result =
          (tag: @switch) match {
            // case TERMREFin =>
            //   var sname = readName()
            //   val prefix = readType()
            //   val space = readType()
            //   sname match {
            //     case SignedName(name, sig) =>
            //       TermRef(prefix, name, space.decl(name).asSeenFrom(prefix).atSignature(sig))
            //     case name =>
            //       TermRef(prefix, name, space.decl(name).asSeenFrom(prefix))
            //   }
            // case TYPEREFin =>
            //   val name = readName().toTypeName
            //   val prefix = readType()
            //   val space = readType()
            //   space.decl(name) match {
            //     case symd: SymDenotation if prefix.isArgPrefixOf(symd.symbol) => TypeRef(prefix, symd.symbol)
            //     case _ => TypeRef(prefix, name, space.decl(name).asSeenFrom(prefix))
            //   }
            // case REFINEDtype =>
            //   var name: Name = readName()
            //   val parent = readType()
            //   val ttag = nextUnsharedTag
            //   if (ttag == TYPEBOUNDS || ttag == TYPEALIAS) name = name.toTypeName
            //   RefinedType(parent, name, readType())
            //     // Note that the lambda "rt => ..." is not equivalent to a wildcard closure!
            //     // Eta expansion of the latter puts readType() out of the expression.
            case APPLIEDtype =>
              val tpe = readType()
              typeRef(tpe.typeSymbolDirect.owner.tpe, tpe.typeSymbolDirect, until(end)(readType()))
            case TYPEBOUNDS =>
              val lo = readType()
              val hi = readType()
              TypeBounds(lo, hi) // if (lo.isMatch && (lo `eq` hi)) MatchAlias(lo) else TypeBounds(lo, hi)
            // case ANNOTATEDtype =>
            //   AnnotatedType(readType(), Annotation(readTerm()))
            // case ANDtype =>
            //   AndType(readType(), readType())
            // case ORtype =>
            //   OrType(readType(), readType())
            // case SUPERtype =>
            //   SuperType(readType(), readType())
            // case MATCHtype =>
            //   MatchType(readType(), readType(), until(end)(readType()))
            // case POLYtype =>
            //   readMethodic(PolyType, _.toTypeName)
            // case METHODtype =>
            //   readMethodic(MethodType, _.toTermName)
            // case ERASEDMETHODtype =>
            //   readMethodic(ErasedMethodType, _.toTermName)
            // case GIVENMETHODtype =>
            //   readMethodic(ContextualMethodType, _.toTermName)
            // case ERASEDGIVENMETHODtype =>
            //   readMethodic(ErasedContextualMethodType, _.toTermName)
            // case IMPLICITMETHODtype =>
            //   readMethodic(ImplicitMethodType, _.toTermName)
            case TYPELAMBDAtype =>
              readMethodic(HKTypeLambda, _.toTypeName)
            case PARAMtype =>
              readTypeRef() match {
                case binder: LambdaType => binder.paramRefs(readNat())
              }
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      def readSimpleType(): Type = {
        (tag: @switch) match {
          case TYPEREFdirect | TERMREFdirect =>
            typeRef(NoPrefix, readSymRef(), Nil)
          case TYPEREFsymbol | TERMREFsymbol =>
            readSymNameRef()
          case TYPEREFpkg =>
            readPackageRef().moduleClass.typeRef
          case TERMREFpkg =>
            readPackageRef().termRef
          case TYPEREF =>
            val name = readName().toTypeName
            TypeRef(readType(), name)
          case TERMREF =>
            val sname = readName()
            val prefix = readType()
            TypeRef(prefix, sname)
  //          sname match {
  //            case SignedName(name, sig) =>
  //              TermRef(prefix, name, prefix.member(name).atSignature(sig))
  //            case name =>
  //              TermRef(prefix, name)
  //          }
          case THIS =>
            ThisType(readType().asInstanceOf[TypeRef].sym)
          case RECtype =>
            typeAtAddr.get(start) match {
              case Some(tp) =>
                skipTree(tag)
                tp
              case None =>
                sys.error("RECtype")//RecType(rt => registeringType(rt, readType()))
            }
          case RECthis =>
            sys.error("RECthis")//readTypeRef().asInstanceOf[RecType].recThis
          case TYPEALIAS =>
             readType()// TypeAlias(readType())
          case SHAREDtype =>
            val ref = readAddr()
            typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
          case BYNAMEtype =>
            appliedType(definitions.ByNameParamClass, readType()) // ExprType(readType())
          case ENUMconst =>
            errorTasty("Enum Constant") //Constant(readTypeRef().termSymbol)
            ErrorType
          case _ =>
            ConstantType(readConstant(tag))
        }
      }

      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
    }

    private def readSymNameRef()(implicit ctx: Context): Type = {
      val sym = readSymRef()
      val prefix = readType()
      prefix match {
        case prefix: ThisType if prefix.sym eq sym.owner /*&& !sym.is(Opaque)*/ =>
          typeRef(NoPrefix, sym, Nil) //res.withDenot(sym.denot)
          // without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
          // the problem arises when a self type of a trait is a type parameter of the same trait.
        case _ => typeRef(prefix, sym, Nil)
      }
    }

    private def readPackageRef()(implicit ctx: Context): TermSymbol = {
      val name = readName()
      if (name == nme.ROOT || name == nme.ROOTPKG) ctx.RootPackage
      else if (name == nme.EMPTY_PACKAGE_NAME) ctx.EmptyPackage
      else ctx.requiredPackage(name)
    }

   def readTypeRef(): Type =
     typeAtAddr(readAddr())

    def readTypeAsTypeRef()(implicit ctx: Context): TypeRef =
      readType().asInstanceOf[TypeRef]

// ------ Reading definitions -----------------------------------------------------

    private def nothingButMods(end: Addr): Boolean =
      currentAddr == end || isModifierTag(nextByte)

    private def localContext(owner: Symbol)(implicit ctx: Context): Context =
      ctx.fresh.setOwner(owner)

    private def normalizeFlags(tag: Int, givenFlags: FlagSet, name: Name, isAbsType: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): FlagSet = {
      val lacksDefinition =
        rhsIsEmpty &&
          name.isTermName && !name.isConstructorName && !givenFlags.isOneOf(TermParamOrAccessor) ||
        isAbsType
      var flags = givenFlags
      if (lacksDefinition && tag != PARAM) flags |= Deferred
      if (tag == DEFDEF) flags |= Method
      if (givenFlags.is(Module))
        flags = flags | (if (tag == VALDEF) ModuleValCreationFlags else ModuleClassCreationFlags)
      if (ctx.owner.isClass) {
        if (tag == TYPEPARAM) flags |= Param
        else if (tag == PARAM) {
          flags |= ParamAccessor
          if (!rhsIsEmpty) // param alias
            flags |= Method
        }
      }
      else if (isParamTag(tag)) flags |= Param
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
        val localDummy = ctx.newLocalDummy(ctx.owner)
        registerSym(currentAddr, localDummy)
        localDummy
      case tag =>
        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
    }

    private def createBindSymbol()(implicit ctx: Context): Symbol = {
      val start = currentAddr
      readByte() // tag
      readEnd()  // end
      var name: Name = readName()
      nextUnsharedTag match {
        case TYPEBOUNDS | TYPEALIAS => name = name.toTypeName
        case _ =>
      }
      val typeReader = fork
      val completer = new TastyLazyType {
        override def complete(sym: Symbol): Unit =
          sym.info = typeReader.readType()
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
      val end = readEnd()
      var name: Name = readName()
      if (tag == TYPEDEF || tag == TYPEPARAM) name = name.toTypeName
      skipParams()
      val ttag = nextUnsharedTag
      val isAbsType = isAbstractType(ttag)
      val isClass = ttag == TEMPLATE
      val templateStart = currentAddr
      skipTree() // tpt
      val rhsStart = currentAddr
      val rhsIsEmpty = nothingButMods(end)
      if (!rhsIsEmpty) skipTree()
      val (givenFlags, tastyFlagSet, annotFns, privateWithin) = readModifiers(end, readTypedAnnot, readTypedWithin, NoSymbol)
      val flags = normalizeFlags(tag, givenFlags, name, isAbsType, rhsIsEmpty)
      def showFlags = {
        if (!tastyFlagSet)
          show(flags)
        else if (givenFlags == NoFlags)
          show(tastyFlagSet)
        else
          show(flags) + " | " + show(tastyFlagSet)
      }
      ctx.log(s"""creating symbol $name${if (privateWithin ne NoSymbol) s" private within $privateWithin" else ""} at $start with flags $showFlags""")
      def adjustIfModule(completer: TastyLazyType) = {
        if (flags.is(Module)) ctx.adjustModuleCompleter(completer, name) else completer
      }
//      val coord = coordAt(start)
      val sym =
        roots.find(root => (root.owner eq ctx.owner) && root.name == name) match {
          case Some(found) =>
//            rootd.coord = coord
            val rootd = if (flags.is(ModuleClassCreationFlags) && tag == TYPEDEF) found.linkedClassOfClass else found
            rootd.info = adjustIfModule(new Completer(subReader(start, end), tastyFlagSet))
            rootd.flags = flags // rootd.flags = flags &~ Touched // allow one more completion
            rootd.setPrivateWithin(privateWithin)
            seenRoots += rootd
            ctx.log(s"replaced info of ${showSym(rootd)}")
            rootd
          case _ =>
            val completer = adjustIfModule(new Completer(subReader(start, end), tastyFlagSet))
            if (isClass)
              ctx.newClassSymbol(ctx.owner, name.toTypeName, flags, completer, privateWithin)
            else
              ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin)
        }
      sym.setAnnotations(annotFns.map(_(sym)))
      ctx.owner match {
        case cls: ClassSymbol =>
          if (!cls.rawInfo.decls.containsName(name)) {
            cls.rawInfo.decls.enter(sym)
          }
        case _ =>
      }
      registerSym(start, sym)
      if (isClass) {
        sym.completer.withDecls(newScope)
        forkAt(templateStart).indexTemplateParams()(localContext(sym))
      }
//      else if (sym.isInlineMethod)
//        sym.addAnnotation(LazyBodyAnnotation { ctx0 =>
//          val ctx1 = localContext(sym)(ctx0).addMode(Mode.ReadPositions)
//          implicit val ctx: Context = sourceChangeContext(Addr(0))(ctx1)
//            // avoids space leaks by not capturing the current context
//          forkAt(rhsStart).readTerm()
//        })
      goto(start)
      sym
    }

    /** Read modifier list into triplet of flags, annotations and a privateWithin
     *  boundary symbol.
     */
    def readModifiers[WithinType, AnnotType]
        (end: Addr, readAnnot: Context => Symbol => AnnotType, readWithin: Context => WithinType, defaultWithin: WithinType)
        (implicit ctx: Context): (FlagSet, TastyFlagSet, List[Symbol => AnnotType], WithinType) = {
      var tastyFlagSet = EmptyFlags
      var flags: FlagSet = NoFlags
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
          case MACRO => addFlag(Macro)
          case OPAQUE => addTastyFlag(Opaque)
          case STATIC => addFlag(JavaStatic)
          case OBJECT => addFlag(Module)
          case TRAIT => addFlag(Trait)
          case ENUM => addFlag(Enum)
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
          case STABLE => addFlag(StableRealizable)
          case EXTENSION => addTastyFlag(Extension)
          case GIVEN => addTastyFlag(Given)
          case PARAMsetter => addFlag(ParamAccessor)
          case EXPORTED => addTastyFlag(Exported)
          case PRIVATEqualified =>
            readByte()
            privateWithin = readWithin(ctx)
          case PROTECTEDqualified =>
            addFlag(Protected)
            privateWithin = readWithin(ctx)
          case ANNOTATION =>
            annotFns = readAnnot(ctx) :: annotFns
          case tag =>
            assert(assertion = false, s"illegal modifier tag $tag at $currentAddr, end = $end")
        }
      }
      (flags, tastyFlagSet, /*annotFns.reverse,*/ Nil, privateWithin)
    }

    private val readTypedWithin: Context => Symbol =
      implicit ctx => readType().typeSymbolDirect

    private val readTypedAnnot: Context => Symbol => Annotation = {
      implicit ctx =>
        readByte()
        val end = readEnd()
        val tp = readType()
        val lazyAnnotTree = readLaterWithOwner(end, rdr => ctx => rdr.readTerm()(ctx))
        owner => { tp.typeSymbolDirect; Annotation(lazyAnnotTree(owner).complete) }  //Annotation.deferredSymAndTree(tp.typeSymbol)(lazyAnnotTree(owner).complete)
    }

    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     *  @return  the largest subset of {NoInits, PureInterface} that a
     *           trait owning the indexed statements can have as flags.
     */
    def indexStats(end: Addr)(implicit ctx: Context): (FlagSet, TastyFlagSet) = {
      var (initsFlags, initsTastyFlags) = NoInitsInterface
      def clearFlags() = {
        initsFlags      = NoFlags
        initsTastyFlags = EmptyFlags
      }
      while (currentAddr.index < end.index) {
        nextByte match {
          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
            val sym = symbolAtCurrent()
            skipTree()
            if (sym.isTerm && !sym.isOneOf(DeferredOrLazyOrMethod))
              clearFlags()
            else if (sym.isClass ||
              sym.is(Method, butNot = Deferred) && !sym.isConstructor)
              initsTastyFlags &= NoInits
          case IMPORT =>
            skipTree()
          case PACKAGE =>
            processPackage { (_, end) => implicit ctx => indexStats(end) }
          case _ =>
            skipTree()
            clearFlags()
        }
      }
      assert(currentAddr.index == end.index)
      (initsFlags, initsTastyFlags)
    }

    /** Process package with given operation `op`. The operation takes as arguments
     *   - a `RefTree` representing the `pid` of the package,
     *   - an end address,
     *   - a context which has the processed package as owner
     */
    def processPackage[T](op: (RefTree, Addr) => Context => T)(implicit ctx: Context): T = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return processPackage(op)(sctx)
      readByte()
      val end = readEnd()
      val tpe = readTypeAsTypeRef()
      val pid = ref(tpe).asInstanceOf[RefTree]
      op(pid, end)(localContext(tpe.typeSymbolDirect.moduleClass))
    }

    def ref[T <: TypeRef](tp: T): Tree = {
      RefTree(TypeTree(tp), tp.sym.name.toTypeName)
    }

    /** Create symbols the longest consecutive sequence of parameters with given
     *  `tag` starting at current address.
     */
    def indexParams(tag: Int)(implicit ctx: Context): Unit =
      while (nextByte == tag) {
        symbolAtCurrent()
        skipTree()
      }

    /** Create symbols for all type and value parameters of template starting
     *  at current address.
     */
    def indexTemplateParams()(implicit ctx: Context): Unit = {
      assert(readByte() == TEMPLATE)
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
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readNewMember()(sctx)
      val symAddr = currentAddr
      val sym     = symAtAddr(symAddr)
      val tag     = readByte()
      val end     = readEnd()
      val name    = readName()

      ctx.log(s"completing member $name at $symAddr. (sym=${showSym(sym)})")

      val completer = sym.completer

      def readParamss(implicit ctx: Context): List[List[NoCycle/*ValDef*/]] = {
        collectWhile(nextByte == PARAMS) {
          readByte()
          readEnd()
          readParams(PARAM)
        }
      }

      val localCtx = localContext(sym)
      val noCycle  = tag match {
        case DEFDEF =>
          val (isExtension, exceptExtension) = completer.tastyFlagSet.except(Extension)
          assertTasty(!exceptExtension, s"unsupported Scala 3 flags on def: ${show(exceptExtension)}")
          if (isExtension) ctx.log(s"$name is a Scala 3 extension method.")
          val tparams = readParams[NoCycle](TYPEPARAM)(localCtx)
          val vparamss = readParamss(localCtx)
          val tpt = readTpt()(localCtx)
          val typeParams = tparams.map(symFromNoCycle)
          val valueParamss = ctx.normalizeIfConstructor(
            vparamss.map(_.map(symFromNoCycle)), name == nme.CONSTRUCTOR)
          val resType = ctx.effectiveResultType(sym, typeParams, tpt.tpe)
          sym.info = ctx.methodType(typeParams, valueParamss, resType)
          NoCycle(at = symAddr)
        case VALDEF => // valdef in TASTy is either a module value or a method forwarder to a local value.
          val (isInline, exceptInline) = completer.tastyFlagSet.except(Inline)
          assertTasty(!exceptInline, s"unsupported flags on val: ${show(exceptInline)}")
          val tpe = readTpt()(localCtx).tpe
          if (isInline) assertTasty(tpe.isInstanceOf[ConstantType], s"inline val ${sym.nameString} with non-constant type $tpe")
          sym.info = if (sym.flags.not(Module)) internal.nullaryMethodType(tpe) else tpe // TODO: really?
          NoCycle(at = symAddr)
        case TYPEDEF | TYPEPARAM =>
          assertTasty(!completer.tastyFlagSet, s"unsupported Scala 3 flags on type: ${show(completer.tastyFlagSet)}")
          if (sym.isClass) {
            sym.owner.ensureCompleted()
            readTemplate(symAddr)(localCtx)
          }
          else {
            sym.info = TypeBounds.empty // needed to avoid cyclic references when unpickling rhs, see i3816.scala
            // sym.setFlag(Provisional)
            val rhs = readTpt()(localCtx)
            sym.info = new NoCompleter {
              override def completerTypeParams(sym: Symbol)(implicit ctx: Context) =
                rhs.tpe.typeParams
            }
            // TODO check for cycles
            sym.info = rhs.tpe match {
              case TypeBounds(_, hi: PolyType) => hi
              case tpe => tpe
            }
            // sym.normalizeOpaque()
            // sym.resetFlag(Provisional)
            NoCycle(at = symAddr)
          }
        case PARAM =>
          assertTasty(!completer.tastyFlagSet, s"unsupported flags on parameter: ${show(completer.tastyFlagSet)}")
          val tpt = readTpt()(localCtx)
          if (nothingButMods(end) && sym.not(ParamAccessor)) {
            sym.info = tpt.tpe
            NoCycle(at = symAddr)
          }
          else {
            sym.info = NullaryMethodType(tpt.tpe)
            NoCycle(at = symAddr)
          }
        case _ => sys.error(s"Reading new member with tag ${astTagToString(tag)}")
      }
      ctx.log(s"typed { $sym: ${sym.tpe} } in (owner=${showSym(ctx.owner)})")
      goto(end)
      noCycle
    }

    private def readTemplate(symAddr: Addr)(implicit ctx: Context): NoCycle = {
      val cls = completeClassTpe1
      val localDummy = symbolAtCurrent()
      val parentCtx = ctx.withOwner(localDummy)
      assert(readByte() == TEMPLATE)
      val end = readEnd()
      ctx.log(s"Template: reading parameters of $cls")
      val tparams = readIndexedParams[NoCycle](TYPEPARAM)
      val vparams = readIndexedParams[NoCycle](PARAM)
      ctx.log(s"Template: indexing members of $cls")
      val (bodyFlags, bodyTastyFlags) = {
        val bodyIndexer = fork
        // The first DEFDEF corresponds to the primary constructor
        while (bodyIndexer.reader.nextByte != DEFDEF) bodyIndexer.skipTree()
        bodyIndexer.indexStats(end)
      }
      ctx.log(s"Template: adding parents of $cls")
      val parents = collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
        nextUnsharedTag match {
          case APPLY | TYPEAPPLY | BLOCK => readTerm()(parentCtx)
          case _ => readTpt()(parentCtx)
        }
      }
      val parentTypes = parents.map { tpt =>
        val tpe = tpt.tpe.dealias
        ctx.log(s"parent: $tpe")
        if (tpe.typeSymbolDirect == definitions.ObjectClass) definitions.AnyRefTpe
        else tpe
      }
      if (nextByte == SELFDEF) {
        ctx.log(s"Template: adding self-type of $cls")
        readByte()
        readName()
        val selfTpe = readTpt().tpe
        ctx.log(s"Template: self-type is $selfTpe")
        cls.typeOfThis = selfTpe
      }
      ctx.log(s"Template: reading constructor of $cls")
      readIndexedMember() // ctor
      cls.info = {
        val classInfo = new ClassInfoType(parentTypes, cls.rawInfo.decls, cls.asType)
        if (tparams.isEmpty) classInfo
        else new PolyType(tparams.map(symFromNoCycle), classInfo)
      }
      NoCycle(at = symAddr)
    }

//    private def readNewDef()(implicit ctx: Context): Tree = {
//      val sctx = sourceChangeContext()
//      if (sctx `ne` ctx) return readNewDef()(sctx)
//      val start = currentAddr
//      val sym = symAtAddr(start)
//      val tag = readByte()
//      val end = readEnd()
//
//      def readParamss(implicit ctx: Context): List[List[ValDef]] = {
//        collectWhile(nextByte == PARAMS) {
//          readByte()
//          readEnd()
//          readParams[ValDef](PARAM)
//        }
//      }
//
//      val localCtx = localContext(sym)
//
//      def readRhs(implicit ctx: Context): LazyTree =
//        if (nothingButMods(end))
//          EmptyTree
//        else if (sym.isInlineMethod)
//          // The body of an inline method is stored in an annotation, so no need to unpickle it again
//          new Trees.Lazy[Tree] {
//            def complete(implicit ctx: Context) = typer.Inliner.bodyToInline(sym)
//          }
//        else
//          readLater(end, rdr => ctx => rdr.readTerm()(ctx.retractMode(Mode.InSuperCall)))
//
//      def ValDef(tpt: Tree) =
//        ta.assignType(untpd.ValDef(sym.name.asTermName, tpt, readRhs(localCtx)), sym)
//
//      def DefDef(tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree) =
//         ta.assignType(
//            untpd.DefDef(sym.name.asTermName, tparams, vparamss, tpt, readRhs(localCtx)),
//            sym)
//
//      def TypeDef(rhs: Tree) =
//        ta.assignType(untpd.TypeDef(sym.name.asTypeName, rhs), sym)
//
//      def ta =  ctx.typeAssigner
//
//      val name = readName()
//      pickling.println(s"reading def of $name at $start")
//      val tree: MemberDef = tag match {
//        case DEFDEF =>
//          val tparams = readParams[TypeDef](TYPEPARAM)(localCtx)
//          val vparamss = readParamss(localCtx)
//          val tpt = readTpt()(localCtx)
//          val typeParams = tparams.map(_.symbol)
//          val valueParamss = ctx.normalizeIfConstructor(
//              vparamss.nestedMap(_.symbol), name == nme.CONSTRUCTOR)
//          val resType = ctx.effectiveResultType(sym, typeParams, tpt.tpe)
//          sym.info = ctx.methodType(typeParams, valueParamss, resType)
//          DefDef(tparams, vparamss, tpt)
//        case VALDEF =>
//          val tpt = readTpt()(localCtx)
//          sym.info = tpt.tpe
//          ValDef(tpt)
//        case TYPEDEF | TYPEPARAM =>
//          if (sym.isClass) {
//            sym.owner.ensureCompleted() // scalacLinkedClass uses unforcedDecls. Make sure it does not miss anything.
//            val companion = sym.scalacLinkedClass
//
//            // Is the companion defined in the same Tasty file as `sym`?
//            // The only case to check here is if `sym` is a root. In this case
//            // `companion` might have been entered by the environment but it might
//            // be missing from the Tasty file. So we check explicitly for that.
//            def isCodefined = roots.contains(companion.denot) == seenRoots.contains(companion)
//
//            if (companion.exists && isCodefined) sym.registerCompanion(companion)
//            TypeDef(readTemplate(localCtx))
//          } else {
//            sym.info = TypeBounds.empty // needed to avoid cyclic references when unpickling rhs, see i3816.scala
//            sym.setFlag(Provisional)
//            val rhs = readTpt()(localCtx)
//            sym.info = new NoCompleter {
//              override def completerTypeParams(sym: Symbol)(implicit ctx: Context) =
//                rhs.tpe.typeParams
//            }
//            sym.info = rhs.tpe match {
//              case _: TypeBounds | _: ClassInfo => checkNonCyclic(sym, rhs.tpe, reportErrors = false)
//              case _ => rhs.tpe.toBounds
//            }
//            sym.normalizeOpaque()
//            sym.resetFlag(Provisional)
//            TypeDef(rhs)
//          }
//        case PARAM =>
//          val tpt = readTpt()(localCtx)
//          if (nothingButMods(end)) {
//            sym.info = tpt.tpe
//            ValDef(tpt)
//          }
//          else {
//            sym.info = ExprType(tpt.tpe)
//            pickling.println(i"reading param alias $name -> $currentAddr")
//            DefDef(Nil, Nil, tpt)
//          }
//      }
//      goto(end)
//      setSpan(start, tree)
//      if (!sym.isType) { // Only terms might have leaky aliases, see the documentation of `checkNoPrivateLeaks`
//        sym.info = ta.avoidPrivateLeaks(sym)
//      }
//
//      if (ctx.mode.is(Mode.ReadComments)) {
//        assert(ctx.docCtx.isDefined, "Mode is `ReadComments`, but no `docCtx` is set.")
//        commentUnpicklerOpt.foreach { commentUnpickler =>
//          val comment = commentUnpickler.commentAt(start)
//          ctx.docCtx.get.addDocstring(tree.symbol, comment)
//          tree.setComment(comment)
//        }
//      }
//
//      tree.setDefTree
//    }

//    private def readTemplate(implicit ctx: Context): Template = {
//      val start = currentAddr
//      assert(sourcePathAt(start).isEmpty)
//      val cls = ctx.owner.asClass
//      val assumedSelfType =
//        if (cls.is(Module) && cls.owner.isClass) TermRef(cls.owner.thisType, cls.name.sourceModuleName)
//        else NoType
//      cls.info = new TempClassInfo(cls.owner.thisType, cls, cls.unforcedDecls, assumedSelfType)
//      val localDummy = symbolAtCurrent()
//      val parentCtx = ctx.withOwner(localDummy)
//      assert(readByte() == TEMPLATE)
//      val end = readEnd()
//      val tparams = readIndexedParams[TypeDef](TYPEPARAM)
//      val vparams = readIndexedParams[ValDef](PARAM)
//      // It's important to index the class definitions before unpickling the parents
//      // (see the parents-cycle test for examples where this matter)
//      val bodyFlags = {
//        val bodyIndexer = fork
//        // The first DEFDEF corresponds to the primary constructor
//        while (bodyIndexer.reader.nextByte != DEFDEF) bodyIndexer.skipTree()
//        bodyIndexer.indexStats(end)
//      }
//      val parents = collectWhile(nextByte != SELFDEF && nextByte != DEFDEF) {
//        nextUnsharedTag match {
//          case APPLY | TYPEAPPLY | BLOCK => readTerm()(parentCtx)
//          case _ => readTpt()(parentCtx)
//        }
//      }
//      val parentTypes = parents.map(_.tpe.dealias)
//      val self =
//        if (nextByte == SELFDEF) {
//          readByte()
//          untpd.ValDef(readName(), readTpt(), EmptyTree).withType(NoType)
//        }
//        else EmptyValDef
//      cls.info = ClassInfo(cls.owner.thisType, cls, parentTypes, cls.unforcedDecls,
//        if (self.isEmpty) NoType else self.tpt.tpe)
//      cls.setNoInitsFlags(parentsKind(parents), bodyFlags)
//      val constr = readIndexedDef().asInstanceOf[DefDef]
//      val mappedParents = parents.map(_.changeOwner(localDummy, constr.symbol))
//
//      val lazyStats = readLater(end, rdr => implicit ctx => {
//        val stats = rdr.readIndexedStats(localDummy, end)
//        tparams ++ vparams ++ stats
//      })
//      setSpan(start,
//        untpd.Template(constr, mappedParents, Nil, self, lazyStats)
//          .withType(localDummy.termRef))
//    }

//    def skipToplevel()(implicit ctx: Context): Unit= {
//      if (!isAtEnd && isTopLevel) {
//        skipTree()
//        skipToplevel()
//      }
//    }

    def isTopLevel: Boolean =
      nextByte == IMPORT || nextByte == PACKAGE

//    def readTopLevel()(implicit ctx: Context): List[Tree] = {
//      @tailrec def read(acc: ListBuffer[Tree]): List[Tree] = {
//        if (isTopLevel) {
//          acc += readIndexedStat(NoSymbol)
//          if (!isAtEnd) read(acc) else acc.toList
//        }
//        else // top-level trees which are not imports or packages are not part of tree
//          acc.toList
//      }
//      read(new ListBuffer[tpd.Tree])
//    }

//    def readIndexedStat(exprOwner: Symbol)(implicit ctx: Context): Tree = nextByte match {
//      case TYPEDEF | VALDEF | DEFDEF =>
//        readIndexedDef()
//      case IMPORT =>
//        readImport()
//      case PACKAGE =>
//        val start = currentAddr
//        processPackage { (pid, end) => implicit ctx =>
//          setSpan(start, PackageDef(pid, readIndexedStats(exprOwner, end)(ctx)))
//        }
//      case _ =>
//        readTerm()(ctx.withOwner(exprOwner))
//    }

//    def readImport()(implicit ctx: Context): Tree = {
//      val start = currentAddr
//      assert(sourcePathAt(start).isEmpty)
//      readByte()
//      readEnd()
//      val importGiven = nextByte == GIVEN
//      if (importGiven) readByte()
//      val expr = readTerm()
//      setSpan(start, Import(importGiven, expr, readSelectors()))
//    }

//    def readSelectors()(implicit ctx: Context): List[untpd.Tree] = nextByte match {
//      case IMPORTED =>
//        val start = currentAddr
//        assert(sourcePathAt(start).isEmpty)
//        readByte()
//        val from = setSpan(start, untpd.Ident(readName()))
//        nextByte match {
//          case RENAMED =>
//            val start2 = currentAddr
//            readByte()
//            val to = setSpan(start2, untpd.Ident(readName()))
//            untpd.Thicket(from, to) :: readSelectors()
//          case _ =>
//            from :: readSelectors()
//        }
//      case BOUNDED =>
//        val start = currentAddr
//        readByte()
//        val bounded = setSpan(start, untpd.TypeBoundsTree(untpd.EmptyTree, untpd.TypedSplice(readTpt())))
//        bounded :: readSelectors()
//      case _ =>
//        Nil
//    }

//    def readIndexedStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] =
//      until(end)(readIndexedStat(exprOwner))

//    def readStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] = {
//      fork.indexStats(end)
//      readIndexedStats(exprOwner, end)
//    }

   def readIndexedParams[T <: MaybeCycle /*MemberDef*/](tag: Int)(implicit ctx: Context): List[T] =
     collectWhile(nextByte == tag) { readIndexedMember().asInstanceOf[T] }

   def readParams[T <: MaybeCycle /*MemberDef*/](tag: Int)(implicit ctx: Context): List[T] = {
     fork.indexParams(tag)
     readIndexedParams(tag)
   }

// ------ Reading trees -----------------------------------------------------

    def readTerm()(implicit ctx: Context): Tree = {  // TODO: rename to readTree
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readTerm()(sctx)
      val start = currentAddr
      val tag = readByte()
      ctx.log(s"reading term ${astTagToString(tag)} at $start, ${ctx.source}")

      def readPathTerm(): Tree = {
        goto(start)
        readType() match {
          case path: TypeRef => TypeTree(path)
//          case path: TermRef => ref(path)
          case path: ThisType => new This(nme.EMPTY.toTypeName).setType(path)
          case path: ConstantType => Literal(path.value).setType(path)
        }
      }

      def completeSelect(name: Name, sig: Sig): Select = {
        val localCtx = ctx // if (name == nme.CONSTRUCTOR) ctx.addMode(Mode.) else ctx
        val qual = readTerm()(localCtx)
        val qualType = qual.tpe.widen
//        val denot = accessibleDenot(qualType, name, sig)
//        val owner = denot.symbol.maybeOwner
//        if (owner.isPackageObject && qualType.termSymbol.is(Package))
//          qualType = qualType.select(owner.sourceModule)
        val tpe = selectFromSig(qualType, name, sig) {
          case name: TypeName => TypeRef(qualType, name)
          case name: TermName => TypeRef(qualType, name)
        }
        Select(qual, name).setType(tpe) // ConstFold(Select(qual, name).setType(tpe))
      }

//      def readQualId(): (untpd.Ident, TypeRef) = {
//        val qual = readTerm().asInstanceOf[untpd.Ident]
//         (untpd.Ident(qual.name).withSpan(qual.span), qual.tpe.asInstanceOf[TypeRef])
//      }

        def selectFromSig(qualType: Type, name: Name, sig: Sig)(ifNotOverload: Name => Type) = {
          if (sig ne NotAMethod) {
            ctx.log(s"looking for overloaded method on $qualType.$name of signature ${sig.show}")
            val MethodSignature(args, ret) = sig
            val retSym = ctx.loadingMirror.findMemberFromRoot(ret)
            var seenTypeParams = false
            val (tyParamCount, argsSyms) = {
              val (tyParamCounts, params) = args.partitionMap(identity)
              assertTasty(tyParamCounts.length <= 1, s"Multiple type parameter lists on signature ${sig.show} for member $name.")
              (tyParamCounts.headOption.getOrElse(0), params.map(ctx.loadingMirror.findMemberFromRoot))
            }
            val member = qualType.member(name)
            val alts = member.asTerm.alternatives
            val tpe = alts.find { sym =>
              val method = sym.asMethod
              val params = method.paramss.flatten
              method.returnType.erasure.typeSymbolDirect == retSym &&
                params.length == argsSyms.length &&
                tyParamCount == method.typeParams.length &&
                params.zip(argsSyms).forall { case (param, sym) => param.tpe.erasure.typeSymbolDirect == sym }
            }.map(_.tpe).getOrElse(ifNotOverload(name))
            ctx.log(s"selected $tpe")
            tpe
          }
          else ifNotOverload(name)
        }

//      def accessibleDenot(qualType: Type, name: Name, sig: Signature) = {
//        val pre = ctx.typeAssigner.maybeSkolemizePrefix(qualType, name)
//        val d = qualType.findMember(name, pre).atSignature(sig)
//        if (!d.symbol.exists || d.symbol.isAccessibleFrom(pre)) d
//        else qualType.findMember(name, pre, excluded = Private).atSignature(sig)
//      }

      def readSimpleTerm(): Tree = tag match {
//        case SHAREDterm =>
//          forkAt(readAddr()).readTerm()
//        case IDENT =>
//          untpd.Ident(readName()).withType(readType())
        case IDENTtpt =>
          Ident(readName().toTypeName).setType(readType())
        case SELECT =>
         readSigName() match {
           case Left(SigName(name, sig)) => completeSelect(name, sig)
           case Right(name)              => completeSelect(name, NotAMethod)
         }
        case SELECTtpt =>
          val name = readName().toTypeName
          completeSelect(name, NotAMethod)
//        case QUALTHIS =>
//          val (qual, tref) = readQualId()
//          untpd.This(qual).withType(ThisType.raw(tref))
        case NEW =>
          val tpt = readTpt()
          New(tpt).setType(tpt.tpe)
//        case THROW =>
//          Throw(readTerm())
       case SINGLETONtpt =>
         val tpt = readTerm()
         SingletonTypeTree(tpt).setType(tpt.tpe)
//        case BYNAMEtpt =>
//          ByNameTypeTree(readTpt())
//        case NAMEDARG =>
//          NamedArg(readName(), readTerm())
        case _ =>
          readPathTerm()
      }

      def readLengthTerm(): Tree = {
        val end = readEnd()
        val result =
          (tag: @switch) match {
//            case SUPER =>
//              val qual = readTerm()
//              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (untpd.EmptyTypeIdent, NoType))
//              tpd.Super(qual, mixId, ctx.mode.is(Mode.InSuperCall), mixTpe.typeSymbol)
            case APPLY =>
              val fn = readTerm()
              val args = until(end)(readTerm())
              Apply(fn, args).setType(fn.tpe.dealiasWiden.finalResultType)
            case TYPEAPPLY =>
              val term = readTerm()
              val args = until(end)(readTpt())
              TypeApply(term, args).setType(term.tpe.resultType.substituteTypes(term.tpe.typeParams, args.map(_.tpe)))
//            case TYPED =>
//              val expr = readTerm()
//              val tpt = readTpt()
//              Typed(expr, tpt)
//            case ASSIGN =>
//              Assign(readTerm(), readTerm())
//            case BLOCK =>
//              val exprReader = fork
//              skipTree()
//              val stats = readStats(ctx.owner, end)
//              val expr = exprReader.readTerm()
//              Block(stats, expr)
//            case INLINED =>
//              val exprReader = fork
//              skipTree()
//              def maybeCall = nextUnsharedTag match {
//                case VALDEF | DEFDEF => EmptyTree
//                case _ => readTerm()
//              }
//              val call = ifBefore(end)(maybeCall, EmptyTree)
//              val bindings = readStats(ctx.owner, end).asInstanceOf[List[ValOrDefDef]]
//              val expansion = exprReader.readTerm() // need bindings in scope, so needs to be read before
//              Inlined(call, bindings, expansion)
//            case IF =>
//              if (nextByte == INLINE) {
//                readByte()
//                InlineIf(readTerm(), readTerm(), readTerm())
//              }
//              else
//                If(readTerm(), readTerm(), readTerm())
//            case LAMBDA =>
//              val meth = readTerm()
//              val tpt = ifBefore(end)(readTpt(), EmptyTree)
//              Closure(Nil, meth, tpt)
//            case MATCH =>
//              if (nextByte == IMPLICIT) {
//                readByte()
//                InlineMatch(EmptyTree, readCases(end))
//              }
//              else if (nextByte == INLINE) {
//                readByte()
//                InlineMatch(readTerm(), readCases(end))
//              }
//              else Match(readTerm(), readCases(end))
//            case RETURN =>
//              val from = readSymRef()
//              val expr = ifBefore(end)(readTerm(), EmptyTree)
//              Return(expr, Ident(from.termRef))
//            case WHILE =>
//              WhileDo(readTerm(), readTerm())
//            case TRY =>
//              Try(readTerm(), readCases(end), ifBefore(end)(readTerm(), EmptyTree))
//            case SELECTouter =>
//              val levels = readNat()
//              readTerm().outerSelect(levels, SkolemType(readType()))
//            case REPEATED =>
//              val elemtpt = readTpt()
//              SeqLiteral(until(end)(readTerm()), elemtpt)
//            case BIND =>
//              val sym = symAtAddr.getOrElse(start, forkAt(start).createSymbol())
//              readName()
//              readType()
//              Bind(sym, readTerm())
//            case ALTERNATIVE =>
//              Alternative(until(end)(readTerm()))
//            case UNAPPLY =>
//              val fn = readTerm()
//              val implicitArgs =
//                collectWhile(nextByte == IMPLICITarg) {
//                  readByte()
//                  readTerm()
//                }
//              val patType = readType()
//              val argPats = until(end)(readTerm())
//              UnApply(fn, implicitArgs, argPats, patType)
//            case REFINEDtpt =>
//              val refineCls = ctx.newRefinedClassSymbol(coordAt(start))
//              typeAtAddr(start) = refineCls.typeRef
//              val parent = readTpt()
//              val refinements = readStats(refineCls, end)(localContext(refineCls))
//              RefinedTypeTree(parent, refinements, refineCls)
            case APPLIEDtpt =>
              // If we do directly a tpd.AppliedType tree we might get a
              // wrong number of arguments in some scenarios reading F-bounded
              // types. This came up in #137 of collection strawman.
              val tycon   = readTpt()
              val args    = until(end)(readTpt())
              val ownType = typeRef(tycon.tpe.prefix, tycon.tpe.typeSymbolDirect, args.map(_.tpe))
              AppliedTypeTree(tycon, args).setType(ownType)
//            case ANNOTATEDtpt =>
//              Annotated(readTpt(), readTerm())
            case LAMBDAtpt =>
              val tparams    = readParams[NoCycle](TYPEPARAM)
              val body       = readTpt()
              val typeParams = tparams.map(symFromNoCycle)
              val tpe        = polyType(typeParams, body.tpe)
              TypeTree(tpe)
            //  LambdaTypeTree(tparams, body)
//            case MATCHtpt =>
//              val fst = readTpt()
//              val (bound, scrut) =
//                if (nextUnsharedTag == CASEDEF) (EmptyTree, fst) else (fst, readTpt())
//              MatchTypeTree(bound, scrut, readCases(end))
            case TYPEBOUNDStpt =>
              val lo = readTpt()
              val hi = if (currentAddr == end) lo else readTpt()
              TypeBoundsTree(lo, hi).setType(internal.typeBounds(lo.tpe, hi.tpe))
//            case HOLE =>
//              readHole(end, isType = false)
//            case _ =>
//              readPathTerm()
          }
        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
        result
      }

      val tree = if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm()
//      if (!tree.isInstanceOf[TypTree]) // FIXME: Necessary to avoid self-type cyclic reference in tasty_tools
//        tree.overwriteType(tree.tpe.simplified)
//      setSpan(start, tree)
      tree
    }

    def readTpt()(implicit ctx: Context): Tree = {
      val sctx = sourceChangeContext()
      if (sctx `ne` ctx) return readTpt()(sctx)
      val start = currentAddr
      val tpt: Tree = nextByte match {
//        case SHAREDterm =>
//          readByte()
//          forkAt(readAddr()).readTpt()
//        case BLOCK =>
//          readByte()
//          val end = readEnd()
//          val typeReader = fork
//          skipTree()
//          val aliases = readStats(ctx.owner, end)
//          val tpt = typeReader.readTpt()
//          Block(aliases, tpt)
//        case HOLE =>
//          readByte()
//          val end = readEnd()
//          readHole(end, isType = true)
        case tag =>
          if (isTypeTreeTag(tag)) readTerm()
          else {
            val tp = readType()
            if ((tp ne NoType) && (tp ne ErrorType)) TypeTree(tp) else EmptyTree
          }
      }
      tpt
    }

//    def readCases(end: Addr)(implicit ctx: Context): List[CaseDef] =
//      collectWhile((nextUnsharedTag == CASEDEF) && currentAddr != end) {
//        if (nextByte == SHAREDterm) {
//          readByte()
//          forkAt(readAddr()).readCase()(ctx.fresh.setNewScope)
//        }
//        else readCase()(ctx.fresh.setNewScope)
//      }

//    def readCase()(implicit ctx: Context): CaseDef = {
//      val sctx = sourceChangeContext()
//      if (sctx `ne` ctx) return readCase()(sctx)
//      val start = currentAddr
//      assert(readByte() == CASEDEF)
//      val end = readEnd()
//      val pat = readTerm()
//      val rhs = readTerm()
//      val guard = ifBefore(end)(readTerm(), EmptyTree)
//      setSpan(start, CaseDef(pat, guard, rhs))
//    }

//    def readLater[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Trees.Lazy[T] =
//      readLaterWithOwner(end, op)(ctx)(ctx.owner)

    def readLaterWithOwner[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Symbol => Trees.Lazy[T] = {
      val localReader = fork
      goto(end)
      owner => new LazyReader(localReader, owner/*, ctx.mode*/, ctx.source, op)
    }

//    def readHole(end: Addr, isType: Boolean)(implicit ctx: Context): Tree = {
//      val idx = readNat()
//      val args = until(end)(readTerm())
//      val splice = splices(idx)
//      def wrap(arg: Tree) =
//        if (arg.isTerm) { implicit qctx: scala.quoted.QuoteContext => new TastyTreeExpr(arg, ToolboxImpl.scopeId)}
//        else new TreeType(arg, ToolboxImpl.scopeId)
//      val reifiedArgs = args.map(wrap)
//      val filled = if (isType) {
//        val quotedType = splice.asInstanceOf[Seq[Any] => quoted.Type[_]](reifiedArgs)
//        PickledQuotes.quotedTypeToTree(quotedType)
//      } else {
//        val splice1 = splice.asInstanceOf[Seq[Any] => scala.quoted.QuoteContext => quoted.Expr[_]]
//        val quotedExpr = splice1(reifiedArgs)(dotty.tools.dotc.quoted.QuoteContext())
//        PickledQuotes.quotedExprToTree(quotedExpr)
//      }
//      // We need to make sure a hole is created with the source file of the surrounding context, even if
//      // it filled with contents a different source file. Otherwise nodes containing holes might end
//      // up without a position. PositionPickler makes sure that holes always get spans assigned,
//      // so we can just return the filler tree with the new source and no span here.
//      if (filled.source == ctx.source) filled
//      else {
//        val filled1 = filled.cloneIn(ctx.source)
//        filled1.span = NoSpan
//        filled1
//      }
//    }

// ------ Setting positions ------------------------------------------------

//    /** Pickled span for `addr`. */
//    def spanAt(addr: Addr)(implicit ctx: Context): Span =
//      if (ctx.mode.is(Mode.ReadPositions)) {
//        posUnpicklerOpt match {
//          case Some(posUnpickler) =>
//            posUnpickler.spanAt(addr)
//          case _  =>
//            NoSpan
//        }
//      } else NoSpan

//    /** Coordinate for the symbol at `addr`. */
//    def coordAt(addr: Addr)(implicit ctx: Context): Coord = {
//      val span = spanAt(addr)
//      if (span.exists)
//        spanCoord(span)
//      else
//        indexCoord(addr.index)
//    }

    /** Pickled source path at `addr`. */
    def sourcePathAt(addr: Addr)(implicit ctx: Context): String = ""
//      if (ctx.mode.is(Mode.ReadPositions)) {
//        posUnpicklerOpt match {
//          case Some(posUnpickler) =>
//            posUnpickler.sourcePathAt(addr)
//          case _  =>
//            ""
//        }
//      } else ""

    /** If currentAddr carries a source path, the current context with
     *  the source of that path, otherwise the current context itself.
     */
    def sourceChangeContext(addr: Addr = currentAddr)(implicit ctx: Context): Context = {
      val path = sourcePathAt(addr)
      if (!path.isEmpty) {
        ctx.log(s"source change at $addr: $path")
        sys.error("Context requires to change source.") // ctx.withSource(ctx.getSource(path))
      }
      else ctx
    }

//    /** Set position of `tree` at given `addr`. */
//    def setSpan[T <: untpd.Tree](addr: Addr, tree: T)(implicit ctx: Context): tree.type = {
//      val span = spanAt(addr)
//      if (span.exists) tree.span = span
//      tree
//    }
  }

  class LazyReader[T <: AnyRef](
      reader: TreeReader, owner: Symbol/*, mode: Mode*/, source: AbstractFile,
      op: TreeReader => Context => T) extends Trees.Lazy[T] {
    def complete(implicit ctx: Context): T = {
      ctx.log(s"starting to read at ${reader.reader.currentAddr} with owner $owner")
      Contexts.withPhaseNoLater(ctx.picklerPhase) { implicit ctx =>
        op(reader)(ctx
          .withOwner(owner))
//          .withModeBits(mode)
//          .withSource(source))
      }
    }
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
      if (myChildren == null) myChildren = {
        val buf = new mutable.ListBuffer[OwnerTree]
        reader.scanTrees(buf, end, if (tag == TEMPLATE) NoMemberDefs else AllDefs)
        buf.toList
      }
      myChildren
    }

    /** Find the owner of definition at `addr` */
    def findOwner(addr: Addr)(implicit ctx: Context): Symbol = {
      def search(cs: List[OwnerTree], current: Symbol): Symbol =
        try cs match {
        case ot :: cs1 =>
          if (ot.addr.index == addr.index) {
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
      try search(children, NoSymbol)
      catch {
        case ex: TreeWithoutOwner =>
          ctx.log(s"ownerTree = $ownerTree") // pickling.println
          throw ex
      }
    }

    override def toString: String =
      s"OwnerTree(${addr.index}, ${end.index}, ${if (myChildren == null) "?" else myChildren.mkString(" ")})"
  }

  def symFromNoCycle(noCycle: NoCycle): Symbol = symAtAddr(noCycle.at)
}

object TreeUnpickler {

//  /** Define the expected format of the tasty bytes
//   *   - TopLevel: Tasty that contains a full class nested in its package
//   *   - Term: Tasty that contains only a term tree
//   *   - TypeTree: Tasty that contains only a type tree or a reference to a type
//   */
//  sealed trait UnpickleMode
//  object UnpickleMode {
//    /** Unpickle a full class in some package */
//    object TopLevel extends UnpickleMode
//    /** Unpickle as a TermTree */
//    object Term extends UnpickleMode
//    /** Unpickle as a TypeTree */
//    object TypeTree extends UnpickleMode
//  }

  sealed trait MaybeCycle
  object MaybeCycle {
    case class  NoCycle(at: Addr) extends MaybeCycle
    case object Tombstone         extends MaybeCycle
  }

  //  /** A marker value used to detect cyclic reference while unpickling definitions. */
  //  case object PoisonTree extends TermTree with CannotHaveAttrs { override def isEmpty: Boolean = true }

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}
