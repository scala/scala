package scala.tools.nsc
package tasty

import TastyBuffer._

//import Comments.CommentsContext
//import Contexts._
//import Symbols._
//import Types._
//import Scopes._
//import SymDenotations._
//import Names._
//import NameOps._
//import StdNames._
//import Flags._
//import Constants._
//import Annotations._
//import NameKinds._
//import typer.ConstFold
//import typer.Checking.checkNonCyclic
//import util.Spans._
//import util.SourceFile
//import ast.{TreeTypeMap, Trees, tpd, untpd}
//import Trees._
//import Decorators._
//import transform.SymUtils._

//import scala.annotation.{switch, tailrec}
//import scala.collection.mutable.ListBuffer
//import scala.collection.mutable
//import config.Printers.pickling
//import core.quoted.PickledQuotes
//import dotty.tools.dotc.quoted.ToolboxImpl

//import scala.quoted
//import scala.internal.quoted.{TastyTreeExpr, TreeType}
//import scala.annotation.constructorOnly
//import scala.annotation.internal.sharable

/** Unpickler for typed trees
 *  @param reader              the reader from which to unpickle
 *  @param posUnpicklerOpt     the unpickler for positions, if it exists
 *  @param commentUnpicklerOpt the unpickler for comments, if it exists
 *  @param splices
 */
abstract class TreeUnpickler(reader: TastyReader,
                             nameAtRef: NameRef => TastyUnpickler.TermName,
                             posUnpicklerOpt: Option[PositionUnpickler],
                             commentUnpicklerOpt: Option[CommentUnpickler],
                             splices: Seq[Any]) extends TASTYUniverse {
  import symbolTable._
  import TastyFormat._
//  import TreeUnpickler._
//  import tpd._
//
//  /** A map from addresses of definition entries to the symbols they define */
//  private val symAtAddr  = new mutable.HashMap[Addr, Symbol]
//
//  /** A temporary map from addresses of definition entries to the trees they define.
//   *  Used to remember trees of symbols that are created by a completion. Emptied
//   *  once the tree is inlined into a larger tree.
//   */
//  private val treeAtAddr = new mutable.HashMap[Addr, Tree]
//
//  /** A map from addresses of type entries to the types they define.
//   *  Currently only populated for types that might be recursively referenced
//   *  from within themselves (i.e. RecTypes, LambdaTypes).
//   */
//  private val typeAtAddr = new mutable.HashMap[Addr, Type]

  /** The root symbol denotation which are defined by the Tasty file associated with this
   *  TreeUnpickler. Set by `enterTopLevel`.
   */
  private[this] var roots: Set[Symbol] = null

//  /** The root symbols that are defined in this Tasty file. This
//   *  is a subset of `roots.map(_.symbol)`.
//   */
//  private[this] var seenRoots: Set[Symbol] = Set()

  /** The root owner tree. See `OwnerTree` class definition. Set by `enterTopLevel`. */
  private[this] var ownerTree: OwnerTree = _

//  private def registerSym(addr: Addr, sym: Symbol) =
//    symAtAddr(addr) = sym

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[Symbol]): Unit = {
    this.roots = roots
    val rdr = new TreeReader(reader).fork
    ownerTree = new OwnerTree(NoAddr, 0, rdr.fork, reader.endAddr)
    if (rdr.isTopLevel)
      rdr.indexStats(reader.endAddr)
  }
//
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
//
//  class Completer(reader: TastyReader)(implicit @constructorOnly ctx: Context) extends LazyType {
//    import reader._
//    val owner = ctx.owner
//    val source = ctx.source
//    def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
//      treeAtAddr(currentAddr) =
//        new TreeReader(reader).readIndexedDef()(
//          ctx.withPhaseNoLater(ctx.picklerPhase).withOwner(owner).withSource(source))
//    }
//  }
//
  class TreeReader(val reader: TastyReader) {
    import reader._

    def forkAt(start: Addr): TreeReader = new TreeReader(subReader(start, endAddr))
    def fork: TreeReader = forkAt(currentAddr)
//
//    def skipTree(tag: Int): Unit =
//      if (tag >= firstLengthTreeTag) goto(readEnd())
//      else if (tag >= firstNatASTTreeTag) { readNat(); skipTree() }
//      else if (tag >= firstASTTreeTag) skipTree()
//      else if (tag >= firstNatTreeTag) readNat()
//    def skipTree(): Unit = skipTree(readByte())
//
//    def skipParams(): Unit =
//      while (nextByte == PARAMS || nextByte == TYPEPARAM) skipTree()
//
//    /** Record all directly nested definitions and templates in current tree
//     *  as `OwnerTree`s in `buf`.
//     *  A complication concerns member definitions. These are lexically nested in a
//     *  Template node, but need to be listed separately in the OwnerTree of the enclosing class
//     *  in order not to confuse owner chains.
//     */
//    def scanTree(buf: ListBuffer[OwnerTree], mode: MemberDefMode = AllDefs): Unit = {
//      val start = currentAddr
//      val tag = readByte()
//      tag match {
//        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | TEMPLATE =>
//          val end = readEnd()
//          for (i <- 0 until numRefs(tag)) readNat()
//          if (tag == TEMPLATE) {
//            // Read all member definitions now, whereas non-members are children of
//            // template's owner tree.
//            val nonMemberReader = fork
//            scanTrees(buf, end, MemberDefsOnly)
//            buf += new OwnerTree(start, tag, nonMemberReader, end)
//          }
//          else if (mode != NoMemberDefs)
//            buf += new OwnerTree(start, tag, fork, end)
//          goto(end)
//        case tag =>
//          if (mode == MemberDefsOnly) skipTree(tag)
//          else if (tag >= firstLengthTreeTag) {
//            val end = readEnd()
//            var nrefs = numRefs(tag)
//            if (nrefs < 0) {
//              for (i <- nrefs until 0) scanTree(buf)
//              goto(end)
//            }
//            else {
//              for (i <- 0 until nrefs) readNat()
//              if (tag == BIND) {
//                // a Bind is never the owner of anything, so we set `end = start`
//                buf += new OwnerTree(start, tag, fork, end = start)
//              }
//
//              scanTrees(buf, end)
//            }
//          }
//          else if (tag >= firstNatASTTreeTag) { readNat(); scanTree(buf) }
//          else if (tag >= firstASTTreeTag) scanTree(buf)
//          else if (tag >= firstNatTreeTag) readNat()
//      }
//    }
//
//    /** Record all directly nested definitions and templates between current address and `end`
//     *  as `OwnerTree`s in `buf`
//     */
//    def scanTrees(buf: ListBuffer[OwnerTree], end: Addr, mode: MemberDefMode = AllDefs): Unit = {
//      while (currentAddr.index < end.index) scanTree(buf, mode)
//      assert(currentAddr.index == end.index)
//    }
//
//    /** The next tag, following through SHARED tags */
//    def nextUnsharedTag: Int = {
//      val tag = nextByte
//      if (tag == SHAREDtype || tag == SHAREDterm) {
//        val lookAhead = fork
//        lookAhead.reader.readByte()
//        forkAt(lookAhead.reader.readAddr()).nextUnsharedTag
//      }
//      else tag
//    }
//
//    def readName(): TermName = nameAtRef(readNameRef())
//
//// ------ Reading types -----------------------------------------------------
//
//    /** Read names in an interleaved sequence of (parameter) names and types/bounds */
//    def readParamNames(end: Addr): List[Name] =
//      until(end) {
//        val name = readName()
//        skipTree()
//        name
//      }
//
//    /** Read types or bounds in an interleaved sequence of (parameter) names and types/bounds */
//    def readParamTypes[T <: Type](end: Addr)(implicit ctx: Context): List[T] =
//      until(end) { readNat(); readType().asInstanceOf[T] }
//
//    /** Read reference to definition and return symbol created at that definition */
//    def readSymRef()(implicit ctx: Context): Symbol = symbolAt(readAddr())
//
//    /** The symbol at given address; createa new one if none exists yet */
//    def symbolAt(addr: Addr)(implicit ctx: Context): Symbol = symAtAddr.get(addr) match {
//      case Some(sym) =>
//        sym
//      case None =>
//        val sym = forkAt(addr).createSymbol()(ctx.withOwner(ownerTree.findOwner(addr)))
//        ctx.log(i"forward reference to $sym")
//        sym
//    }
//
//    /** The symbol defined by current definition */
//    def symbolAtCurrent()(implicit ctx: Context): Symbol = symAtAddr.get(currentAddr) match {
//      case Some(sym) =>
//        assert(ctx.owner == sym.owner, i"owner discrepancy for $sym, expected: ${ctx.owner}, found: ${sym.owner}")
//        sym
//      case None =>
//        createSymbol()
//    }
//
//    def readConstant(tag: Int)(implicit ctx: Context): Constant = (tag: @switch) match {
//      case UNITconst =>
//        Constant(())
//      case TRUEconst =>
//        Constant(true)
//      case FALSEconst =>
//        Constant(false)
//      case BYTEconst =>
//        Constant(readInt().toByte)
//      case SHORTconst =>
//        Constant(readInt().toShort)
//      case CHARconst =>
//        Constant(readNat().toChar)
//      case INTconst =>
//        Constant(readInt())
//      case LONGconst =>
//        Constant(readLongInt())
//      case FLOATconst =>
//        Constant(java.lang.Float.intBitsToFloat(readInt()))
//      case DOUBLEconst =>
//        Constant(java.lang.Double.longBitsToDouble(readLongInt()))
//      case STRINGconst =>
//        Constant(readName().toString)
//      case NULLconst =>
//        Constant(null)
//      case CLASSconst =>
//        Constant(readType())
//      case ENUMconst =>
//        Constant(readTermRef().termSymbol)
//    }
//
//    /** Read a type */
//    def readType()(implicit ctx: Context): Type = {
//      val start = currentAddr
//      val tag = readByte()
//      pickling.println(s"reading type ${astTagToString(tag)} at $start, ${ctx.source}")
//
//      def registeringType[T](tp: Type, op: => T): T = {
//        typeAtAddr(start) = tp
//        op
//      }
//
//      def readLengthType(): Type = {
//        val end = readEnd()
//
//        def readMethodic[N <: Name, PInfo <: Type, LT <: LambdaType]
//            (companion: LambdaTypeCompanion[N, PInfo, LT], nameMap: Name => N): LT = {
//          val result = typeAtAddr.getOrElse(start, {
//              val nameReader = fork
//              nameReader.skipTree() // skip result
//              val paramReader = nameReader.fork
//              val paramNames = nameReader.readParamNames(end).map(nameMap)
//              companion(paramNames)(
//                pt => registeringType(pt, paramReader.readParamTypes[PInfo](end)),
//                pt => readType())
//            })
//          goto(end)
//          result.asInstanceOf[LT]
//        }
//
//        val result =
//          (tag: @switch) match {
//            case TERMREFin =>
//              var sname = readName()
//              val prefix = readType()
//              val space = readType()
//              sname match {
//                case SignedName(name, sig) =>
//                  TermRef(prefix, name, space.decl(name).asSeenFrom(prefix).atSignature(sig))
//                case name =>
//                  TermRef(prefix, name, space.decl(name).asSeenFrom(prefix))
//              }
//            case TYPEREFin =>
//              val name = readName().toTypeName
//              val prefix = readType()
//              val space = readType()
//              space.decl(name) match {
//                case symd: SymDenotation if prefix.isArgPrefixOf(symd.symbol) => TypeRef(prefix, symd.symbol)
//                case _ => TypeRef(prefix, name, space.decl(name).asSeenFrom(prefix))
//              }
//            case REFINEDtype =>
//              var name: Name = readName()
//              val parent = readType()
//              val ttag = nextUnsharedTag
//              if (ttag == TYPEBOUNDS || ttag == TYPEALIAS) name = name.toTypeName
//              RefinedType(parent, name, readType())
//                // Note that the lambda "rt => ..." is not equivalent to a wildcard closure!
//                // Eta expansion of the latter puts readType() out of the expression.
//            case APPLIEDtype =>
//              readType().appliedTo(until(end)(readType()))
//            case TYPEBOUNDS =>
//              val lo = readType()
//              val hi = readType()
//              if (lo.isMatch && (lo `eq` hi)) MatchAlias(lo)
//              else TypeBounds(lo, hi)
//            case ANNOTATEDtype =>
//              AnnotatedType(readType(), Annotation(readTerm()))
//            case ANDtype =>
//              AndType(readType(), readType())
//            case ORtype =>
//              OrType(readType(), readType())
//            case SUPERtype =>
//              SuperType(readType(), readType())
//            case MATCHtype =>
//              MatchType(readType(), readType(), until(end)(readType()))
//            case POLYtype =>
//              readMethodic(PolyType, _.toTypeName)
//            case METHODtype =>
//              readMethodic(MethodType, _.toTermName)
//            case ERASEDMETHODtype =>
//              readMethodic(ErasedMethodType, _.toTermName)
//            case GIVENMETHODtype =>
//              readMethodic(ContextualMethodType, _.toTermName)
//            case ERASEDGIVENMETHODtype =>
//              readMethodic(ErasedContextualMethodType, _.toTermName)
//            case IMPLICITMETHODtype =>
//              readMethodic(ImplicitMethodType, _.toTermName)
//            case TYPELAMBDAtype =>
//              readMethodic(HKTypeLambda, _.toTypeName)
//            case PARAMtype =>
//              readTypeRef() match {
//                case binder: LambdaType => binder.paramRefs(readNat())
//              }
//          }
//        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
//        result
//      }
//
//      def readSimpleType(): Type = (tag: @switch) match {
//        case TYPEREFdirect | TERMREFdirect =>
//          NamedType(NoPrefix, readSymRef())
//        case TYPEREFsymbol | TERMREFsymbol =>
//          readSymNameRef()
//        case TYPEREFpkg =>
//          readPackageRef().moduleClass.typeRef
//        case TERMREFpkg =>
//          readPackageRef().termRef
//        case TYPEREF =>
//          val name = readName().toTypeName
//          TypeRef(readType(), name)
//        case TERMREF =>
//          val sname = readName()
//          val prefix = readType()
//          sname match {
//            case SignedName(name, sig) =>
//              TermRef(prefix, name, prefix.member(name).atSignature(sig))
//            case name =>
//              TermRef(prefix, name)
//          }
//        case THIS =>
//          ThisType.raw(readType().asInstanceOf[TypeRef])
//        case RECtype =>
//          typeAtAddr.get(start) match {
//            case Some(tp) =>
//              skipTree(tag)
//              tp
//            case None =>
//              RecType(rt => registeringType(rt, readType()))
//          }
//        case RECthis =>
//          readTypeRef().asInstanceOf[RecType].recThis
//        case TYPEALIAS =>
//          TypeAlias(readType())
//        case SHAREDtype =>
//          val ref = readAddr()
//          typeAtAddr.getOrElseUpdate(ref, forkAt(ref).readType())
//        case BYNAMEtype =>
//          ExprType(readType())
//        case _ =>
//          ConstantType(readConstant(tag))
//      }
//
//      if (tag < firstLengthTreeTag) readSimpleType() else readLengthType()
//    }
//
//    private def readSymNameRef()(implicit ctx: Context): Type = {
//      val sym = readSymRef()
//      val prefix = readType()
//      val res = NamedType(prefix, sym)
//      prefix match {
//        case prefix: ThisType if (prefix.cls eq sym.owner) && !sym.is(Opaque) =>
//          res.withDenot(sym.denot)
//          // without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
//          // the problem arises when a self type of a trait is a type parameter of the same trait.
//        case _ => res
//      }
//    }
//
//    private def readPackageRef()(implicit ctx: Context): TermSymbol = {
//      val name = readName()
//      if (name == nme.ROOT || name == nme.ROOTPKG) defn.RootPackage
//      else if (name == nme.EMPTY_PACKAGE) defn.EmptyPackageVal
//      else ctx.requiredPackage(name)
//    }
//
//    def readTypeRef(): Type =
//      typeAtAddr(readAddr())
//
//    def readTermRef()(implicit ctx: Context): TermRef =
//      readType().asInstanceOf[TermRef]
//
//// ------ Reading definitions -----------------------------------------------------
//
//    private def nothingButMods(end: Addr): Boolean =
//      currentAddr == end || isModifierTag(nextByte)
//
//    private def localContext(owner: Symbol)(implicit ctx: Context) =
//      ctx.fresh.setOwner(owner)
//
//    private def normalizeFlags(tag: Int, givenFlags: FlagSet, name: Name, isAbsType: Boolean, rhsIsEmpty: Boolean)(implicit ctx: Context): FlagSet = {
//      val lacksDefinition =
//        rhsIsEmpty &&
//          name.isTermName && !name.isConstructorName && !givenFlags.isOneOf(TermParamOrAccessor) ||
//        isAbsType
//      var flags = givenFlags
//      if (lacksDefinition && tag != PARAM) flags |= Deferred
//      if (tag == DEFDEF) flags |= Method
//      if (givenFlags.is(Module))
//        flags = flags | (if (tag == VALDEF) ModuleValCreationFlags else ModuleClassCreationFlags)
//      if (ctx.owner.isClass) {
//        if (tag == TYPEPARAM) flags |= Param
//        else if (tag == PARAM) {
//          flags |= ParamAccessor
//          if (!rhsIsEmpty) // param alias
//            flags |= Method
//        }
//      }
//      else if (isParamTag(tag)) flags |= Param
//      flags
//    }
//
//    def isAbstractType(ttag: Int)(implicit ctx: Context): Boolean = nextUnsharedTag match {
//      case LAMBDAtpt =>
//        val rdr = fork
//        rdr.reader.readByte()  // tag
//        rdr.reader.readNat()   // length
//        rdr.skipParams()       // tparams
//        rdr.isAbstractType(rdr.nextUnsharedTag)
//      case TYPEBOUNDS | TYPEBOUNDStpt => true
//      case _ => false
//    }
//
//    /** Create symbol of definition node and enter in symAtAddr map
//     *  @return  the created symbol
//     */
//    def createSymbol()(implicit ctx: Context): Symbol = nextByte match {
//      case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
//        createMemberSymbol()
//      case BIND =>
//        createBindSymbol()
//      case TEMPLATE =>
//        val localDummy = ctx.newLocalDummy(ctx.owner)
//        registerSym(currentAddr, localDummy)
//        localDummy
//      case tag =>
//        throw new Error(s"illegal createSymbol at $currentAddr, tag = $tag")
//    }
//
//    private def createBindSymbol()(implicit ctx: Context): Symbol = {
//      val start = currentAddr
//      val tag = readByte()
//      val end = readEnd()
//      var name: Name = readName()
//      nextUnsharedTag match {
//        case TYPEBOUNDS | TYPEALIAS => name = name.toTypeName
//        case _ =>
//      }
//      val typeReader = fork
//      val completer = new LazyType {
//        def complete(denot: SymDenotation)(implicit ctx: Context) =
//          denot.info = typeReader.readType()
//      }
//      val sym = ctx.newSymbol(ctx.owner, name, Flags.Case, completer, coord = coordAt(start))
//      registerSym(start, sym)
//      sym
//    }
//
//    /** Create symbol of member definition or parameter node and enter in symAtAddr map
//     *  @return  the created symbol
//     */
//    def createMemberSymbol()(implicit ctx: Context): Symbol = {
//      val start = currentAddr
//      val tag = readByte()
//      val end = readEnd()
//      var name: Name = readName()
//      if (tag == TYPEDEF || tag == TYPEPARAM) name = name.toTypeName
//      skipParams()
//      val ttag = nextUnsharedTag
//      val isAbsType = isAbstractType(ttag)
//      val isClass = ttag == TEMPLATE
//      val templateStart = currentAddr
//      skipTree() // tpt
//      val rhsStart = currentAddr
//      val rhsIsEmpty = nothingButMods(end)
//      if (!rhsIsEmpty) skipTree()
//      val (givenFlags, annotFns, privateWithin) = readModifiers(end, readTypedAnnot, readTypedWithin, NoSymbol)
//      pickling.println(i"creating symbol $name at $start with flags $givenFlags")
//      val flags = normalizeFlags(tag, givenFlags, name, isAbsType, rhsIsEmpty)
//      def adjustIfModule(completer: LazyType) =
//        if (flags.is(Module)) ctx.adjustModuleCompleter(completer, name) else completer
//      val coord = coordAt(start)
//      val sym =
//        roots.find(root => (root.owner eq ctx.owner) && root.name == name) match {
//          case Some(rootd) =>
//            pickling.println(i"overwriting ${rootd.symbol} # ${rootd.hashCode}")
//            rootd.symbol.coord = coord
//            rootd.info = adjustIfModule(
//                new Completer(subReader(start, end)) with SymbolLoaders.SecondCompleter)
//            rootd.flags = flags &~ Touched // allow one more completion
//            rootd.setPrivateWithin(privateWithin)
//            seenRoots += rootd.symbol
//            rootd.symbol
//          case _ =>
//            val completer = adjustIfModule(new Completer(subReader(start, end)))
//            if (isClass)
//              ctx.newClassSymbol(ctx.owner, name.asTypeName, flags, completer, privateWithin, coord)
//            else
//              ctx.newSymbol(ctx.owner, name, flags, completer, privateWithin, coord)
//        }
//      sym.annotations = annotFns.map(_(sym))
//      ctx.owner match {
//        case cls: ClassSymbol => cls.enter(sym)
//        case _ =>
//      }
//      registerSym(start, sym)
//      if (isClass) {
//        sym.completer.withDecls(newScope)
//        forkAt(templateStart).indexTemplateParams()(localContext(sym))
//      }
//      else if (sym.isInlineMethod)
//        sym.addAnnotation(LazyBodyAnnotation { ctx0 =>
//          val ctx1 = localContext(sym)(ctx0).addMode(Mode.ReadPositions)
//          implicit val ctx: Context = sourceChangeContext(Addr(0))(ctx1)
//            // avoids space leaks by not capturing the current context
//          forkAt(rhsStart).readTerm()
//        })
//      goto(start)
//      sym
//    }
//
//    /** Read modifier list into triplet of flags, annotations and a privateWithin
//     *  boundary symbol.
//     */
//    def readModifiers[WithinType, AnnotType]
//        (end: Addr, readAnnot: Context => Symbol => AnnotType, readWithin: Context => WithinType, defaultWithin: WithinType)
//        (implicit ctx: Context): (FlagSet, List[Symbol => AnnotType], WithinType) = {
//      var flags: FlagSet = EmptyFlags
//      var annotFns: List[Symbol => AnnotType] = Nil
//      var privateWithin = defaultWithin
//      while (currentAddr.index != end.index) {
//        def addFlag(flag: FlagSet) = {
//          flags |= flag
//          readByte()
//        }
//        nextByte match {
//          case PRIVATE => addFlag(Private)
//          case INTERNAL => ??? // addFlag(Internal)
//          case PROTECTED => addFlag(Protected)
//          case ABSTRACT =>
//            readByte()
//            nextByte match {
//              case OVERRIDE => addFlag(AbsOverride)
//              case _ => flags |= Abstract
//            }
//          case FINAL => addFlag(Final)
//          case SEALED => addFlag(Sealed)
//          case CASE => addFlag(Case)
//          case IMPLICIT => addFlag(Implicit)
//          case ERASED => addFlag(Erased)
//          case LAZY => addFlag(Lazy)
//          case OVERRIDE => addFlag(Override)
//          case INLINE => addFlag(Inline)
//          case INLINEPROXY => addFlag(InlineProxy)
//          case MACRO => addFlag(Macro)
//          case OPAQUE => addFlag(Opaque)
//          case STATIC => addFlag(JavaStatic)
//          case OBJECT => addFlag(Module)
//          case TRAIT => addFlag(Trait)
//          case ENUM => addFlag(Enum)
//          case LOCAL => addFlag(Local)
//          case SYNTHETIC => addFlag(Synthetic)
//          case ARTIFACT => addFlag(Artifact)
//          case MUTABLE => addFlag(Mutable)
//          case FIELDaccessor => addFlag(Accessor)
//          case CASEaccessor => addFlag(CaseAccessor)
//          case COVARIANT => addFlag(Covariant)
//          case CONTRAVARIANT => addFlag(Contravariant)
//          case SCALA2X => addFlag(Scala2x)
//          case DEFAULTparameterized => addFlag(DefaultParameterized)
//          case STABLE => addFlag(StableRealizable)
//          case EXTENSION => addFlag(Extension)
//          case GIVEN => addFlag(Given)
//          case PARAMsetter => addFlag(ParamAccessor)
//          case EXPORTED => addFlag(Exported)
//          case PRIVATEqualified =>
//            readByte()
//            privateWithin = readWithin(ctx)
//          case PROTECTEDqualified =>
//            addFlag(Protected)
//            privateWithin = readWithin(ctx)
//          case ANNOTATION =>
//            annotFns = readAnnot(ctx) :: annotFns
//          case tag =>
//            assert(false, s"illegal modifier tag $tag at $currentAddr, end = $end")
//        }
//      }
//      (flags, annotFns.reverse, privateWithin)
//    }
//
//    private val readTypedWithin: Context => Symbol =
//      implicit ctx => readType().typeSymbol
//
//    private val readTypedAnnot: Context => Symbol => Annotation = {
//      implicit ctx =>
//        readByte()
//        val end = readEnd()
//        val tp = readType()
//        val lazyAnnotTree = readLaterWithOwner(end, rdr => ctx => rdr.readTerm()(ctx))
//
//        owner =>
//          Annotation.deferredSymAndTree(tp.typeSymbol)(lazyAnnotTree(owner).complete)
//    }
//
    /** Create symbols for the definitions in the statement sequence between
     *  current address and `end`.
     *  @return  the largest subset of {NoInits, PureInterface} that a
     *           trait owning the indexed statements can have as flags.
     */
    def indexStats(end: Addr): FlagSet = {
//      var initsFlags = NoInitsInterface
//      while (currentAddr.index < end.index) {
//        nextByte match {
//          case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM =>
//            val sym = symbolAtCurrent()
//            skipTree()
//            if (sym.isTerm && !sym.isOneOf(DeferredOrLazyOrMethod))
//              initsFlags = EmptyFlags
//            else if (sym.isClass ||
//              sym.is(Method, butNot = Deferred) && !sym.isConstructor)
//              initsFlags &= NoInits
//          case IMPORT =>
//            skipTree()
//          case PACKAGE =>
//            processPackage { (pid, end) => implicit ctx => indexStats(end) }
//          case _ =>
//            skipTree()
//            initsFlags = EmptyFlags
//        }
//      }
//      assert(currentAddr.index == end.index)
//      initsFlags
      NoFlags
    }
//
//    /** Process package with given operation `op`. The operation takes as arguments
//     *   - a `RefTree` representing the `pid` of the package,
//     *   - an end address,
//     *   - a context which has the processed package as owner
//     */
//    def processPackage[T](op: (RefTree, Addr) => Context => T)(implicit ctx: Context): T = {
//      val sctx = sourceChangeContext()
//      if (sctx `ne` ctx) return processPackage(op)(sctx)
//      readByte()
//      val end = readEnd()
//      val pid = ref(readTermRef()).asInstanceOf[RefTree]
//      op(pid, end)(localContext(pid.symbol.moduleClass))
//    }
//
//    /** Create symbols the longest consecutive sequence of parameters with given
//     *  `tag` starting at current address.
//     */
//    def indexParams(tag: Int)(implicit ctx: Context): Unit =
//      while (nextByte == tag) {
//        symbolAtCurrent()
//        skipTree()
//      }
//
//    /** Create symbols for all type and value parameters of template starting
//     *  at current address.
//     */
//    def indexTemplateParams()(implicit ctx: Context): Unit = {
//      assert(readByte() == TEMPLATE)
//      readEnd()
//      indexParams(TYPEPARAM)
//      indexParams(PARAM)
//    }
//
//    /** If definition was already read by a completer, return the previously read tree
//     *  or else read definition.
//     */
//    def readIndexedDef()(implicit ctx: Context): Tree = treeAtAddr.remove(currentAddr) match {
//      case Some(tree) =>
//        assert(tree != PoisonTree, s"Cyclic reference while unpickling definition at address ${currentAddr.index} in unit ${ctx.compilationUnit}")
//        skipTree()
//        tree
//      case none =>
//        val start = currentAddr
//        treeAtAddr(start) = PoisonTree
//        val tree = readNewDef()
//        treeAtAddr.remove(start)
//        tree
//    }
//
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
//
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
//
//    def skipToplevel()(implicit ctx: Context): Unit= {
//      if (!isAtEnd && isTopLevel) {
//        skipTree()
//        skipToplevel()
//      }
//    }
//
    def isTopLevel: Boolean =
      nextByte == IMPORT || nextByte == PACKAGE
//
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
//
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
//
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
//
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
//
//    def readIndexedStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] =
//      until(end)(readIndexedStat(exprOwner))
//
//    def readStats(exprOwner: Symbol, end: Addr)(implicit ctx: Context): List[Tree] = {
//      fork.indexStats(end)
//      readIndexedStats(exprOwner, end)
//    }
//
//    def readIndexedParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] =
//      collectWhile(nextByte == tag) { readIndexedDef().asInstanceOf[T] }
//
//    def readParams[T <: MemberDef](tag: Int)(implicit ctx: Context): List[T] = {
//      fork.indexParams(tag)
//      readIndexedParams(tag)
//    }
//
//// ------ Reading trees -----------------------------------------------------
//
//    def readTerm()(implicit ctx: Context): Tree = {  // TODO: rename to readTree
//      val sctx = sourceChangeContext()
//      if (sctx `ne` ctx) return readTerm()(sctx)
//      val start = currentAddr
//      val tag = readByte()
//      pickling.println(s"reading term ${astTagToString(tag)} at $start, ${ctx.source}")
//
//      def readPathTerm(): Tree = {
//        goto(start)
//        readType() match {
//          case path: TypeRef => TypeTree(path)
//          case path: TermRef => ref(path)
//          case path: ThisType => untpd.This(untpd.EmptyTypeIdent).withType(path)
//          case path: ConstantType => Literal(path.value)
//        }
//      }
//
//      def completeSelect(name: Name, sig: Signature): Select = {
//        val localCtx =
//          if (name == nme.CONSTRUCTOR) ctx.addMode(Mode.InSuperCall) else ctx
//        val qual = readTerm()(localCtx)
//        var qualType = qual.tpe.widenIfUnstable
//        val denot = accessibleDenot(qualType, name, sig)
//        val owner = denot.symbol.maybeOwner
//        if (owner.isPackageObject && qualType.termSymbol.is(Package))
//          qualType = qualType.select(owner.sourceModule)
//        val tpe = name match {
//          case name: TypeName => TypeRef(qualType, name, denot)
//          case name: TermName => TermRef(qualType, name, denot)
//        }
//        ConstFold(untpd.Select(qual, name).withType(tpe))
//      }
//
//      def readQualId(): (untpd.Ident, TypeRef) = {
//        val qual = readTerm().asInstanceOf[untpd.Ident]
//         (untpd.Ident(qual.name).withSpan(qual.span), qual.tpe.asInstanceOf[TypeRef])
//      }
//
//      def accessibleDenot(qualType: Type, name: Name, sig: Signature) = {
//        val pre = ctx.typeAssigner.maybeSkolemizePrefix(qualType, name)
//        val d = qualType.findMember(name, pre).atSignature(sig)
//        if (!d.symbol.exists || d.symbol.isAccessibleFrom(pre)) d
//        else qualType.findMember(name, pre, excluded = Private).atSignature(sig)
//      }
//
//      def readSimpleTerm(): Tree = tag match {
//        case SHAREDterm =>
//          forkAt(readAddr()).readTerm()
//        case IDENT =>
//          untpd.Ident(readName()).withType(readType())
//        case IDENTtpt =>
//          untpd.Ident(readName().toTypeName).withType(readType())
//        case SELECT =>
//          readName() match {
//            case SignedName(name, sig) => completeSelect(name, sig)
//            case name => completeSelect(name, Signature.NotAMethod)
//          }
//        case SELECTtpt =>
//          val name = readName().toTypeName
//          completeSelect(name, Signature.NotAMethod)
//        case QUALTHIS =>
//          val (qual, tref) = readQualId()
//          untpd.This(qual).withType(ThisType.raw(tref))
//        case NEW =>
//          New(readTpt())
//        case THROW =>
//          Throw(readTerm())
//        case SINGLETONtpt =>
//          SingletonTypeTree(readTerm())
//        case BYNAMEtpt =>
//          ByNameTypeTree(readTpt())
//        case NAMEDARG =>
//          NamedArg(readName(), readTerm())
//        case _ =>
//          readPathTerm()
//      }
//
//      def readLengthTerm(): Tree = {
//        val end = readEnd()
//        val result =
//          (tag: @switch) match {
//            case SUPER =>
//              val qual = readTerm()
//              val (mixId, mixTpe) = ifBefore(end)(readQualId(), (untpd.EmptyTypeIdent, NoType))
//              tpd.Super(qual, mixId, ctx.mode.is(Mode.InSuperCall), mixTpe.typeSymbol)
//            case APPLY =>
//              val fn = readTerm()
//              tpd.Apply(fn, until(end)(readTerm()))
//            case TYPEAPPLY =>
//              tpd.TypeApply(readTerm(), until(end)(readTpt()))
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
//            case APPLIEDtpt =>
//              // If we do directly a tpd.AppliedType tree we might get a
//              // wrong number of arguments in some scenarios reading F-bounded
//              // types. This came up in #137 of collection strawman.
//              val tycon = readTpt()
//              val args = until(end)(readTpt())
//              val ownType =
//                if (tycon.symbol == defn.andType) AndType(args(0).tpe, args(1).tpe)
//                else if (tycon.symbol == defn.orType) OrType(args(0).tpe, args(1).tpe)
//                else tycon.tpe.safeAppliedTo(args.tpes)
//              untpd.AppliedTypeTree(tycon, args).withType(ownType)
//            case ANNOTATEDtpt =>
//              Annotated(readTpt(), readTerm())
//            case LAMBDAtpt =>
//              val tparams = readParams[TypeDef](TYPEPARAM)
//              val body = readTpt()
//              LambdaTypeTree(tparams, body)
//            case MATCHtpt =>
//              val fst = readTpt()
//              val (bound, scrut) =
//                if (nextUnsharedTag == CASEDEF) (EmptyTree, fst) else (fst, readTpt())
//              MatchTypeTree(bound, scrut, readCases(end))
//            case TYPEBOUNDStpt =>
//              val lo = readTpt()
//              val hi = if (currentAddr == end) lo else readTpt()
//              TypeBoundsTree(lo, hi)
//            case HOLE =>
//              readHole(end, isType = false)
//            case _ =>
//              readPathTerm()
//          }
//        assert(currentAddr == end, s"$start $currentAddr $end ${astTagToString(tag)}")
//        result
//      }
//
//      val tree = if (tag < firstLengthTreeTag) readSimpleTerm() else readLengthTerm()
//      if (!tree.isInstanceOf[TypTree]) // FIXME: Necessary to avoid self-type cyclic reference in tasty_tools
//        tree.overwriteType(tree.tpe.simplified)
//      setSpan(start, tree)
//    }
//
//    def readTpt()(implicit ctx: Context): Tree = {
//      val sctx = sourceChangeContext()
//      if (sctx `ne` ctx) return readTpt()(sctx)
//      val start = currentAddr
//      val tree = nextByte match {
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
//        case _ =>
//          if (isTypeTreeTag(nextByte)) readTerm()
//          else {
//            val start = currentAddr
//            val tp = readType()
//            if (tp.exists) setSpan(start, TypeTree(tp)) else EmptyTree
//          }
//      }
//      setSpan(start, tree)
//    }
//
//    def readCases(end: Addr)(implicit ctx: Context): List[CaseDef] =
//      collectWhile((nextUnsharedTag == CASEDEF) && currentAddr != end) {
//        if (nextByte == SHAREDterm) {
//          readByte()
//          forkAt(readAddr()).readCase()(ctx.fresh.setNewScope)
//        }
//        else readCase()(ctx.fresh.setNewScope)
//      }
//
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
//
//    def readLater[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Trees.Lazy[T] =
//      readLaterWithOwner(end, op)(ctx)(ctx.owner)
//
//    def readLaterWithOwner[T <: AnyRef](end: Addr, op: TreeReader => Context => T)(implicit ctx: Context): Symbol => Trees.Lazy[T] = {
//      val localReader = fork
//      goto(end)
//      owner => new LazyReader(localReader, owner, ctx.mode, ctx.source, op)
//    }
//
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
//
//// ------ Setting positions ------------------------------------------------
//
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
//
//    /** Coordinate for the symbol at `addr`. */
//    def coordAt(addr: Addr)(implicit ctx: Context): Coord = {
//      val span = spanAt(addr)
//      if (span.exists)
//        spanCoord(span)
//      else
//        indexCoord(addr.index)
//    }
//
//    /** Pickled source path at `addr`. */
//    def sourcePathAt(addr: Addr)(implicit ctx: Context): String =
//      if (ctx.mode.is(Mode.ReadPositions)) {
//        posUnpicklerOpt match {
//          case Some(posUnpickler) =>
//            posUnpickler.sourcePathAt(addr)
//          case _  =>
//            ""
//        }
//      } else ""
//
//    /** If currentAddr carries a source path, the current context with
//     *  the source of that path, otherwise the current context itself.
//     */
//    def sourceChangeContext(addr: Addr = currentAddr)(implicit ctx: Context): Context = {
//      val path = sourcePathAt(addr)
//      if (path.nonEmpty) {
//        pickling.println(i"source change at $addr: $path")
//        ctx.withSource(ctx.getSource(path))
//      }
//      else ctx
//    }
//
//    /** Set position of `tree` at given `addr`. */
//    def setSpan[T <: untpd.Tree](addr: Addr, tree: T)(implicit ctx: Context): tree.type = {
//      val span = spanAt(addr)
//      if (span.exists) tree.span = span
//      tree
//    }
  }
//
//  class LazyReader[T <: AnyRef](
//      reader: TreeReader, owner: Symbol, mode: Mode, source: SourceFile,
//      op: TreeReader => Context => T) extends Trees.Lazy[T] {
//    def complete(implicit ctx: Context): T = {
//      pickling.println(i"starting to read at ${reader.reader.currentAddr} with owner $owner")
//      op(reader)(ctx
//        .withPhaseNoLater(ctx.picklerPhase)
//        .withOwner(owner)
//        .withModeBits(mode)
//        .withSource(source))
//    }
//  }
//
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
//
//    private var myChildren: List[OwnerTree] = null
//
//    /** All definitions that have the definition at `addr` as closest enclosing definition */
//    def children: List[OwnerTree] = {
//      if (myChildren == null) myChildren = {
//        val buf = new ListBuffer[OwnerTree]
//        reader.scanTrees(buf, end, if (tag == TEMPLATE) NoMemberDefs else AllDefs)
//        buf.toList
//      }
//      myChildren
//    }
//
//    /** Find the owner of definition at `addr` */
//    def findOwner(addr: Addr)(implicit ctx: Context): Symbol = {
//      def search(cs: List[OwnerTree], current: Symbol): Symbol =
//        try cs match {
//        case ot :: cs1 =>
//          if (ot.addr.index == addr.index) {
//            assert(current.exists, i"no symbol at $addr")
//            current
//          }
//          else if (ot.addr.index < addr.index && addr.index < ot.end.index)
//            search(ot.children, reader.symbolAt(ot.addr))
//          else
//            search(cs1, current)
//        case Nil =>
//          throw new TreeWithoutOwner
//      }
//      catch {
//        case ex: TreeWithoutOwner =>
//          pickling.println(i"no owner for $addr among $cs%, %")
//          throw ex
//      }
//      try search(children, NoSymbol)
//      catch {
//        case ex: TreeWithoutOwner =>
//          pickling.println(s"ownerTree = $ownerTree")
//          throw ex
//      }
//    }
//
//    override def toString: String =
//      s"OwnerTree(${addr.index}, ${end.index}, ${if (myChildren == null) "?" else myChildren.mkString(" ")})"
  }
}

object TreeUnpickler {

  /** Define the expected format of the tasty bytes
   *   - TopLevel: Tasty that contains a full class nested in its package
   *   - Term: Tasty that contains only a term tree
   *   - TypeTree: Tasty that contains only a type tree or a reference to a type
   */
  sealed trait UnpickleMode
  object UnpickleMode {
    /** Unpickle a full class in some package */
    object TopLevel extends UnpickleMode
    /** Unpickle as a TermTree */
    object Term extends UnpickleMode
    /** Unpickle as a TypeTree */
    object TypeTree extends UnpickleMode
  }

//  /** A marker value used to detect cyclic reference while unpickling definitions. */
//  @sharable val PoisonTree: tpd.Tree = new EmptyTree

  /** An enumeration indicating which subtrees should be added to an OwnerTree. */
  type MemberDefMode = Int
  final val MemberDefsOnly = 0   // add only member defs; skip other statements
  final val NoMemberDefs = 1     // add only statements that are not member defs
  final val AllDefs = 2          // add everything

  class TreeWithoutOwner extends Exception
}


