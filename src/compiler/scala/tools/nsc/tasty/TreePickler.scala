//package scala.tools.nsc
//package tasty
//
//import ast.Trees._
//import ast.{untpd, tpd}
//import TastyFormat._
//import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._, Flags._
//import typer.Inliner
//import NameKinds._
//import StdNames.nme
//import TastyBuffer._
//import transform.SymUtils._
//import printing.Printer
//import printing.Texts._
//import util.SourceFile
//import annotation.constructorOnly
//
//object TreePickler {
//
//  val sectionName = "ASTs"
//
//  case class Hole(idx: Int, args: List[tpd.Tree])(implicit @constructorOnly src: SourceFile) extends tpd.Tree {
//    override def fallbackToText(printer: Printer): Text =
//      s"[[$idx|" ~~ printer.toTextGlobal(args, ", ") ~~ "]]"
//  }
//}
//
//class TreePickler(pickler: TastyPickler) {
//  val buf: TreeBuffer = new TreeBuffer
//  pickler.newSection(TreePickler.sectionName, buf)
//  import TreePickler._
//  import buf._
//  import pickler.nameBuffer.nameIndex
//  import tpd._
//
//  private val symRefs = Symbols.newMutableSymbolMap[Addr]
//  private val forwardSymRefs = Symbols.newMutableSymbolMap[List[Addr]]
//  private val pickledTypes = new java.util.IdentityHashMap[Type, Any] // Value type is really Addr, but that's not compatible with null
//
//  private def withLength(op: => Unit) = {
//    val lengthAddr = reserveRef(relative = true)
//    op
//    fillRef(lengthAddr, currentAddr, relative = true)
//  }
//
//  def addrOfSym(sym: Symbol): Option[Addr] = {
//    symRefs.get(sym)
//  }
//
//  def preRegister(tree: Tree)(implicit ctx: Context): Unit = tree match {
//    case tree: MemberDef =>
//      if (!symRefs.contains(tree.symbol)) symRefs(tree.symbol) = NoAddr
//    case _ =>
//  }
//
//  def registerDef(sym: Symbol): Unit = {
//    symRefs(sym) = currentAddr
//    forwardSymRefs.get(sym) match {
//      case Some(refs) =>
//        refs.foreach(fillRef(_, currentAddr, relative = false))
//        forwardSymRefs -= sym
//      case None =>
//    }
//  }
//
//  def pickleName(name: Name): Unit = writeNat(nameIndex(name).index)
//
//  private def pickleNameAndSig(name: Name, sig: Signature): Unit =
//    pickleName(
//      if (sig eq Signature.NotAMethod) name
//      else SignedName(name.toTermName, sig))
//
//  private def pickleSymRef(sym: Symbol)(implicit ctx: Context) = symRefs.get(sym) match {
//    case Some(label) =>
//      if (label != NoAddr) writeRef(label) else pickleForwardSymRef(sym)
//    case None =>
//      // See pos/t1957.scala for an example where this can happen.
//      // I believe it's a bug in typer: the type of an implicit argument refers
//      // to a closure parameter outside the closure itself. TODO: track this down, so that we
//      // can eliminate this case.
//      ctx.log(i"pickling reference to as yet undefined $sym in ${sym.owner}", sym.sourcePos)
//      pickleForwardSymRef(sym)
//  }
//
//  private def pickleForwardSymRef(sym: Symbol)(implicit ctx: Context) = {
//    val ref = reserveRef(relative = false)
//    assert(!sym.is(Flags.Package), sym)
//    forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
//  }
//
//  private def isLocallyDefined(sym: Symbol)(implicit ctx: Context) =
//    sym.topLevelClass.isLinkedWith(pickler.rootCls)
//
//  def pickleConstant(c: Constant)(implicit ctx: Context): Unit = c.tag match {
//    case UnitTag =>
//      writeByte(UNITconst)
//    case BooleanTag =>
//      writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
//    case ByteTag =>
//      writeByte(BYTEconst)
//      writeInt(c.byteValue)
//    case ShortTag =>
//      writeByte(SHORTconst)
//      writeInt(c.shortValue)
//    case CharTag =>
//      writeByte(CHARconst)
//      writeNat(c.charValue)
//    case IntTag =>
//      writeByte(INTconst)
//      writeInt(c.intValue)
//    case LongTag =>
//      writeByte(LONGconst)
//      writeLongInt(c.longValue)
//    case FloatTag =>
//      writeByte(FLOATconst)
//      writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
//    case DoubleTag =>
//      writeByte(DOUBLEconst)
//      writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
//    case StringTag =>
//      writeByte(STRINGconst)
//      pickleName(c.stringValue.toTermName)
//    case NullTag =>
//      writeByte(NULLconst)
//    case ClazzTag =>
//      writeByte(CLASSconst)
//      pickleType(c.typeValue)
//    case EnumTag =>
//      writeByte(ENUMconst)
//      pickleType(c.symbolValue.termRef)
//  }
//
//  def pickleType(tpe0: Type, richTypes: Boolean = false)(implicit ctx: Context): Unit = {
//    val tpe = tpe0.stripTypeVar
//    try {
//      val prev = pickledTypes.get(tpe)
//      if (prev == null) {
//        pickledTypes.put(tpe, currentAddr)
//        pickleNewType(tpe, richTypes)
//      }
//      else {
//        writeByte(SHAREDtype)
//        writeRef(prev.asInstanceOf[Addr])
//      }
//    } catch {
//      case ex: AssertionError =>
//        println(i"error when pickling type $tpe")
//        throw ex
//    }
//  }
//
//  private def pickleNewType(tpe: Type, richTypes: Boolean)(implicit ctx: Context): Unit = tpe match {
//    case AppliedType(tycon, args) =>
//      writeByte(APPLIEDtype)
//      withLength { pickleType(tycon); args.foreach(pickleType(_)) }
//    case ConstantType(value) =>
//      pickleConstant(value)
//    case tpe: NamedType =>
//      val sym = tpe.symbol
//      def pickleExternalRef(sym: Symbol) = {
//        def pickleCore() = {
//          pickleNameAndSig(sym.name, tpe.signature)
//          pickleType(tpe.prefix)
//        }
//        val isShadowedRef =
//          sym.isClass && tpe.prefix.member(sym.name).symbol != sym
//        if (sym.is(Flags.Private) || isShadowedRef) {
//          writeByte(if (tpe.isType) TYPEREFin else TERMREFin)
//          withLength {
//            pickleCore()
//            pickleType(sym.owner.typeRef)
//          }
//        }
//        else {
//          writeByte(if (tpe.isType) TYPEREF else TERMREF)
//          pickleCore()
//        }
//      }
//      if (sym.is(Flags.Package)) {
//        writeByte(if (tpe.isType) TYPEREFpkg else TERMREFpkg)
//        pickleName(sym.fullName)
//      }
//      else if (tpe.prefix == NoPrefix) {
//        writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
//        pickleSymRef(sym)
//      }
//      else tpe.designator match {
//        case name: Name =>
//          writeByte(if (tpe.isType) TYPEREF else TERMREF)
//          pickleName(name); pickleType(tpe.prefix)
//        case sym: Symbol =>
//          if (isLocallyDefined(sym)) {
//            writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
//            pickleSymRef(sym); pickleType(tpe.prefix)
//          }
//          else pickleExternalRef(sym)
//      }
//    case tpe: ThisType =>
//      if (tpe.cls.is(Flags.Package) && !tpe.cls.isEffectiveRoot) {
//        writeByte(TERMREFpkg)
//        pickleName(tpe.cls.fullName)
//      }
//      else {
//        writeByte(THIS)
//        pickleType(tpe.tref)
//      }
//    case tpe: SuperType =>
//      writeByte(SUPERtype)
//      withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe) }
//    case tpe: RecThis =>
//      writeByte(RECthis)
//      val binderAddr = pickledTypes.get(tpe.binder)
//      assert(binderAddr != null, tpe.binder)
//      writeRef(binderAddr.asInstanceOf[Addr])
//    case tpe: SkolemType =>
//      pickleType(tpe.info)
//    case tpe: RefinedType =>
//      writeByte(REFINEDtype)
//      withLength {
//        pickleName(tpe.refinedName)
//        pickleType(tpe.parent)
//        pickleType(tpe.refinedInfo, richTypes = true)
//      }
//    case tpe: RecType =>
//      writeByte(RECtype)
//      pickleType(tpe.parent)
//    case tpe: TypeAlias =>
//      writeByte(TYPEALIAS)
//      pickleType(tpe.alias, richTypes)
//    case tpe: TypeBounds =>
//      writeByte(TYPEBOUNDS)
//      withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
//    case tpe: AnnotatedType =>
//      writeByte(ANNOTATEDtype)
//      withLength { pickleType(tpe.parent, richTypes); pickleTree(tpe.annot.tree) }
//    case tpe: AndType =>
//      writeByte(ANDtype)
//      withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
//    case tpe: OrType =>
//      writeByte(ORtype)
//      withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
//    case tpe: ExprType =>
//      writeByte(BYNAMEtype)
//      pickleType(tpe.underlying)
//    case tpe: HKTypeLambda =>
//      pickleMethodic(TYPELAMBDAtype, tpe)
//    case tpe: MatchType =>
//      writeByte(MATCHtype)
//      withLength {
//        pickleType(tpe.bound)
//        pickleType(tpe.scrutinee)
//        tpe.cases.foreach(pickleType(_))
//      }
//    case tpe: PolyType if richTypes =>
//      pickleMethodic(POLYtype, tpe)
//    case tpe: MethodType if richTypes =>
//      val tag = methodTypeTag(
//        isContextual = tpe.isContextualMethod,
//        isImplicit = tpe.isImplicitMethod && !tpe.isContextualMethod,
//        isErased = tpe.isErasedMethod)
//      pickleMethodic(tag, tpe)
//    case tpe: ParamRef =>
//      assert(pickleParamRef(tpe), s"orphan parameter reference: $tpe")
//    case tpe: LazyRef =>
//      pickleType(tpe.ref)
//  }
//
//  def pickleMethodic(tag: Int, tpe: LambdaType)(implicit ctx: Context): Unit = {
//    writeByte(tag)
//    withLength {
//      pickleType(tpe.resultType, richTypes = true)
//      tpe.paramNames.lazyZip(tpe.paramInfos).foreach { (name, tpe) =>
//        pickleName(name); pickleType(tpe)
//      }
//    }
//  }
//
//  def pickleParamRef(tpe: ParamRef)(implicit ctx: Context): Boolean = {
//    val binder = pickledTypes.get(tpe.binder)
//    val pickled = binder != null
//    if (pickled) {
//      writeByte(PARAMtype)
//      withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
//    }
//    pickled
//  }
//
//  def pickleTpt(tpt: Tree)(implicit ctx: Context): Unit =
//    pickleTree(tpt)
//
//  def pickleTreeUnlessEmpty(tree: Tree)(implicit ctx: Context): Unit =
//    if (!tree.isEmpty) pickleTree(tree)
//
//  def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ())(implicit ctx: Context): Unit = {
//    assert(symRefs(sym) == NoAddr, sym)
//    registerDef(sym)
//    writeByte(tag)
//    withLength {
//      pickleName(sym.name)
//      pickleParams
//      tpt match {
//        case _: Template | _: Hole => pickleTree(tpt)
//        case _ if tpt.isType => pickleTpt(tpt)
//      }
//      pickleTreeUnlessEmpty(rhs)
//      pickleModifiers(sym)
//    }
//  }
//
//  def pickleParam(tree: Tree)(implicit ctx: Context): Unit = {
//    registerTreeAddr(tree)
//    tree match {
//      case tree: ValDef => pickleDef(PARAM, tree.symbol, tree.tpt)
//      case tree: DefDef => pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
//      case tree: TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)
//    }
//  }
//
//  def pickleParams(trees: List[Tree])(implicit ctx: Context): Unit = {
//    trees.foreach(preRegister)
//    trees.foreach(pickleParam)
//  }
//
//  def pickleStats(stats: List[Tree])(implicit ctx: Context): Unit = {
//    stats.foreach(preRegister)
//    stats.foreach(stat => if (!stat.isEmpty) pickleTree(stat))
//  }
//
//  def pickleTree(tree: Tree)(implicit ctx: Context): Unit = {
//    val addr = registerTreeAddr(tree)
//    if (addr != currentAddr) {
//      writeByte(SHAREDterm)
//      writeRef(addr)
//    }
//    else
//      try tree match {
//        case Ident(name) =>
//          tree.tpe match {
//            case tp: TermRef if name != nme.WILDCARD =>
//              // wildcards are pattern bound, need to be preserved as ids.
//              pickleType(tp)
//            case tp =>
//              writeByte(if (tree.isType) IDENTtpt else IDENT)
//              pickleName(name)
//              pickleType(tp)
//          }
//        case This(qual) =>
//          if (qual.isEmpty) pickleType(tree.tpe)
//          else {
//            writeByte(QUALTHIS)
//            val ThisType(tref) = tree.tpe
//            pickleTree(qual.withType(tref))
//          }
//        case Select(qual, name) =>
//          name match {
//            case OuterSelectName(_, levels) =>
//              writeByte(SELECTouter)
//              withLength {
//                writeNat(levels)
//                pickleTree(qual)
//                val SkolemType(tp) = tree.tpe
//                pickleType(tp)
//              }
//            case _ =>
//              writeByte(if (name.isTypeName) SELECTtpt else SELECT)
//              val sig = tree.tpe.signature
//              pickleNameAndSig(name, sig)
//              pickleTree(qual)
//          }
//        case Apply(fun, args) =>
//          if (fun.symbol eq defn.throwMethod) {
//            writeByte(THROW)
//            pickleTree(args.head)
//          } else {
//            writeByte(APPLY)
//            withLength {
//              pickleTree(fun)
//              args.foreach(pickleTree)
//            }
//          }
//        case TypeApply(fun, args) =>
//          writeByte(TYPEAPPLY)
//          withLength {
//            pickleTree(fun)
//            args.foreach(pickleTpt)
//          }
//        case Literal(const1) =>
//          pickleConstant {
//            tree.tpe match {
//              case ConstantType(const2) => const2
//              case _ => const1
//            }
//          }
//        case Super(qual, mix) =>
//          writeByte(SUPER)
//          withLength {
//            pickleTree(qual);
//            if (!mix.isEmpty) {
//              val SuperType(_, mixinType: TypeRef) = tree.tpe
//              pickleTree(mix.withType(mixinType))
//            }
//          }
//        case New(tpt) =>
//          writeByte(NEW)
//          pickleTpt(tpt)
//        case Typed(expr, tpt) =>
//          writeByte(TYPED)
//          withLength { pickleTree(expr); pickleTpt(tpt) }
//        case NamedArg(name, arg) =>
//          writeByte(NAMEDARG)
//          pickleName(name)
//          pickleTree(arg)
//        case Assign(lhs, rhs) =>
//          writeByte(ASSIGN)
//          withLength { pickleTree(lhs); pickleTree(rhs) }
//        case Block(stats, expr) =>
//          writeByte(BLOCK)
//          stats.foreach(preRegister)
//          withLength { pickleTree(expr); stats.foreach(pickleTree) }
//        case tree @ If(cond, thenp, elsep) =>
//          writeByte(IF)
//          withLength {
//            if (tree.isInline) writeByte(INLINE)
//            pickleTree(cond)
//            pickleTree(thenp)
//            pickleTree(elsep)
//          }
//        case Closure(env, meth, tpt) =>
//          writeByte(LAMBDA)
//          assert(env.isEmpty)
//          withLength {
//            pickleTree(meth)
//            if (tpt.tpe.exists) pickleTpt(tpt)
//          }
//        case tree @ Match(selector, cases) =>
//          writeByte(MATCH)
//          withLength {
//            if (tree.isInline) {
//              if (selector.isEmpty) writeByte(IMPLICIT)
//              else { writeByte(INLINE); pickleTree(selector) }
//            }
//            else pickleTree(selector)
//            tree.cases.foreach(pickleTree)
//          }
//        case CaseDef(pat, guard, rhs) =>
//          writeByte(CASEDEF)
//          withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
//        case Return(expr, from) =>
//          writeByte(RETURN)
//          withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
//        case WhileDo(cond, body) =>
//          writeByte(WHILE)
//          withLength { pickleTree(cond); pickleTree(body) }
//        case Try(block, cases, finalizer) =>
//          writeByte(TRY)
//          withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
//        case SeqLiteral(elems, elemtpt) =>
//          writeByte(REPEATED)
//          withLength { pickleTree(elemtpt); elems.foreach(pickleTree) }
//        case Inlined(call, bindings, expansion) =>
//          writeByte(INLINED)
//          bindings.foreach(preRegister)
//          withLength {
//            pickleTree(expansion)
//            if (!call.isEmpty) pickleTree(call)
//            bindings.foreach { b =>
//              assert(b.isInstanceOf[DefDef] || b.isInstanceOf[ValDef])
//              pickleTree(b)
//            }
//          }
//        case Bind(name, body) =>
//          registerDef(tree.symbol)
//          writeByte(BIND)
//          withLength {
//            pickleName(name); pickleType(tree.symbol.info); pickleTree(body)
//          }
//        case Alternative(alts) =>
//          writeByte(ALTERNATIVE)
//          withLength { alts.foreach(pickleTree) }
//        case UnApply(fun, implicits, patterns) =>
//          writeByte(UNAPPLY)
//          withLength {
//            pickleTree(fun)
//            for (implicitArg <- implicits) {
//              writeByte(IMPLICITarg)
//              pickleTree(implicitArg)
//            }
//            pickleType(tree.tpe)
//            patterns.foreach(pickleTree)
//          }
//        case tree: ValDef =>
//          pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
//        case tree: DefDef =>
//          def pickleAllParams = {
//            pickleParams(tree.tparams)
//            for (vparams <- tree.vparamss) {
//              writeByte(PARAMS)
//              withLength { pickleParams(vparams) }
//            }
//          }
//          pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleAllParams)
//        case tree: TypeDef =>
//          pickleDef(TYPEDEF, tree.symbol, tree.rhs)
//        case tree: Template =>
//          registerDef(tree.symbol)
//          writeByte(TEMPLATE)
//          val (params, rest) = decomposeTemplateBody(tree.body)
//          withLength {
//            pickleParams(params)
//            tree.parents.foreach(pickleTree)
//            val cinfo @ ClassInfo(_, _, _, _, selfInfo) = tree.symbol.owner.info
//            if (!tree.self.isEmpty) {
//              writeByte(SELFDEF)
//              pickleName(tree.self.name)
//
//              if (!tree.self.tpt.isEmpty) pickleTree(tree.self.tpt)
//              else {
//                if (!tree.self.isEmpty) registerTreeAddr(tree.self)
//                pickleType {
//                  selfInfo match {
//                    case sym: Symbol => sym.info
//                    case tp: Type => tp
//                  }
//                }
//              }
//            }
//            pickleStats(tree.constr :: rest)
//          }
//        case Import(importGiven, expr, selectors) =>
//          writeByte(IMPORT)
//          withLength {
//            if (importGiven) writeByte(GIVEN)
//            pickleTree(expr)
//            pickleSelectors(selectors)
//          }
//        case PackageDef(pid, stats) =>
//          writeByte(PACKAGE)
//          withLength { pickleType(pid.tpe); pickleStats(stats) }
//        case tree: TypeTree =>
//          pickleType(tree.tpe)
//        case SingletonTypeTree(ref) =>
//          writeByte(SINGLETONtpt)
//          pickleTree(ref)
//        case RefinedTypeTree(parent, refinements) =>
//          if (refinements.isEmpty) pickleTree(parent)
//          else {
//            val refineCls = refinements.head.symbol.owner.asClass
//            pickledTypes.put(refineCls.typeRef, currentAddr)
//            writeByte(REFINEDtpt)
//            refinements.foreach(preRegister)
//            withLength { pickleTree(parent); refinements.foreach(pickleTree) }
//          }
//        case AppliedTypeTree(tycon, args) =>
//          writeByte(APPLIEDtpt)
//          withLength { pickleTree(tycon); args.foreach(pickleTree) }
//        case MatchTypeTree(bound, selector, cases) =>
//          writeByte(MATCHtpt)
//          withLength {
//            if (!bound.isEmpty) pickleTree(bound)
//            pickleTree(selector)
//            cases.foreach(pickleTree)
//          }
//        case ByNameTypeTree(tp) =>
//          writeByte(BYNAMEtpt)
//          pickleTree(tp)
//        case Annotated(tree, annot) =>
//          writeByte(ANNOTATEDtpt)
//          withLength { pickleTree(tree); pickleTree(annot) }
//        case LambdaTypeTree(tparams, body) =>
//          writeByte(LAMBDAtpt)
//          withLength { pickleParams(tparams); pickleTree(body) }
//        case TypeBoundsTree(lo, hi) =>
//          writeByte(TYPEBOUNDStpt)
//          withLength {
//            pickleTree(lo);
//            if (hi ne lo) pickleTree(hi)
//          }
//        case Hole(idx, args) =>
//          writeByte(HOLE)
//          withLength {
//            writeNat(idx)
//            args.foreach(pickleTree)
//          }
//      }
//      catch {
//        case ex: AssertionError =>
//          println(i"error when pickling tree $tree")
//          throw ex
//      }
//  }
//
//  def pickleSelectors(selectors: List[untpd.Tree])(implicit ctx: Context): Unit =
//    selectors foreach {
//      case Thicket((from @ Ident(_)) :: (to @ Ident(_)) :: Nil) =>
//        pickleSelector(IMPORTED, from)
//        pickleSelector(RENAMED, to)
//      case id @ Ident(_) =>
//        pickleSelector(IMPORTED, id)
//      case bounded @ TypeBoundsTree(untpd.EmptyTree, untpd.TypedSplice(tpt)) =>
//        registerTreeAddr(bounded)
//        writeByte(BOUNDED)
//        pickleTree(tpt)
//    }
//
//  def pickleSelector(tag: Int, id: untpd.Ident)(implicit ctx: Context): Unit = {
//    registerTreeAddr(id)
//    writeByte(tag)
//    pickleName(id.name)
//  }
//
//  def pickleModifiers(sym: Symbol)(implicit ctx: Context): Unit = {
//    import Flags._
//    var flags = sym.flags
//    val privateWithin = sym.privateWithin
//    if (privateWithin.exists) {
//      writeByte(if (flags.is(Protected)) PROTECTEDqualified else PRIVATEqualified)
//      pickleType(privateWithin.typeRef)
//      flags = flags &~ Protected
//    }
//    if (flags.is(ParamAccessor) && sym.isTerm && !sym.isSetter)
//      flags = flags &~ ParamAccessor // we only generate a tag for parameter setters
//    pickleFlags(flags, sym.isTerm)
//    sym.annotations.foreach(pickleAnnotation(sym, _))
//  }
//
//  def pickleFlags(flags: FlagSet, isTerm: Boolean)(implicit ctx: Context): Unit = {
//    import Flags._
//    def writeModTag(tag: Int) = {
//      assert(isModifierTag(tag))
//      writeByte(tag)
//    }
//    if (flags.is(Private)) writeModTag(PRIVATE)
//    if (flags.is(Protected)) writeModTag(PROTECTED)
//    if (flags.is(Final, butNot = Module)) writeModTag(FINAL)
//    if (flags.is(Case)) writeModTag(CASE)
//    if (flags.is(Override)) writeModTag(OVERRIDE)
//    if (flags.is(Inline)) writeModTag(INLINE)
//    if (flags.is(InlineProxy)) writeModTag(INLINEPROXY)
//    if (flags.is(Macro)) writeModTag(MACRO)
//    if (flags.is(JavaStatic)) writeModTag(STATIC)
//    if (flags.is(Module)) writeModTag(OBJECT)
//    if (flags.is(Enum)) writeModTag(ENUM)
//    if (flags.is(Local)) writeModTag(LOCAL)
//    if (flags.is(Synthetic)) writeModTag(SYNTHETIC)
//    if (flags.is(Artifact)) writeModTag(ARTIFACT)
//    if (flags.is(Scala2x)) writeModTag(SCALA2X)
//    if (isTerm) {
//      if (flags.is(Implicit)) writeModTag(IMPLICIT)
//      if (flags.is(Given)) writeModTag(GIVEN)
//      if (flags.is(Erased)) writeModTag(ERASED)
//      if (flags.is(Lazy, butNot = Module)) writeModTag(LAZY)
//      if (flags.is(AbsOverride)) { writeModTag(ABSTRACT); writeModTag(OVERRIDE) }
//      if (flags.is(Mutable)) writeModTag(MUTABLE)
//      if (flags.is(Accessor)) writeModTag(FIELDaccessor)
//      if (flags.is(CaseAccessor)) writeModTag(CASEaccessor)
//      if (flags.is(DefaultParameterized)) writeModTag(DEFAULTparameterized)
//      if (flags.is(StableRealizable)) writeModTag(STABLE)
//      if (flags.is(Extension)) writeModTag(EXTENSION)
//      if (flags.is(ParamAccessor)) writeModTag(PARAMsetter)
//      if (flags.is(Exported)) writeModTag(EXPORTED)
//      assert(!(flags.is(Label)))
//    } else {
//      if (flags.is(Sealed)) writeModTag(SEALED)
//      if (flags.is(Abstract)) writeModTag(ABSTRACT)
//      if (flags.is(Trait)) writeModTag(TRAIT)
//      if (flags.is(Covariant)) writeModTag(COVARIANT)
//      if (flags.is(Contravariant)) writeModTag(CONTRAVARIANT)
//      if (flags.is(Opaque)) writeModTag(OPAQUE)
//    }
//  }
//
//  private def isUnpicklable(owner: Symbol, ann: Annotation)(implicit ctx: Context) = ann match {
//    case Annotation.Child(sym) => sym.isInaccessibleChildOf(owner)
//      // If child annotation refers to a local class or enum value under
//      // a different toplevel class, it is impossible to pickle a reference to it.
//      // Such annotations will be reconstituted when unpickling the child class.
//      // See tests/pickling/i3149.scala
//    case _ =>
//      ann.symbol == defn.BodyAnnot // inline bodies are reconstituted automatically when unpickling
//  }
//
//  def pickleAnnotation(owner: Symbol, ann: Annotation)(implicit ctx: Context): Unit =
//    if (!isUnpicklable(owner, ann)) {
//      writeByte(ANNOTATION)
//      withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
//    }
//
//// ---- main entry points ---------------------------------------
//
//  def pickle(trees: List[Tree])(implicit ctx: Context): Unit = {
//    trees.foreach(tree => if (!tree.isEmpty) pickleTree(tree))
//    def missing = forwardSymRefs.keysIterator.map(sym => sym.showLocated + "(line " + sym.sourcePos.line + ")").toList
//    assert(forwardSymRefs.isEmpty, i"unresolved symbols: $missing%, % when pickling ${ctx.source}")
//  }
//
//  def compactify(): Unit = {
//    buf.compactify()
//
//    def updateMapWithDeltas(mp: MutableSymbolMap[Addr]) =
//      for (key <- mp.keysIterator.toBuffer[Symbol]) mp(key) = adjusted(mp(key))
//
//    updateMapWithDeltas(symRefs)
//  }
//}
