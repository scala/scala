/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
package scala.tools.nsc.typechecker

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.compat.Platform.currentTime
import scala.tools.nsc.util.{HashSet, Position, Set}
import symtab.Flags._
import util.HashSet

/** This trait provides methods to create symbols and to enter them into
 *  scopes.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Typers requires Analyzer {
  import global._
  import definitions._
  import posAssigner.atPos

  var appcnt = 0
  var idcnt = 0
  var selcnt = 0
  var implcnt = 0
  var impltime = 0l

  private val transformed = new HashMap[Tree, Tree]

  private val superDefs = new HashMap[Symbol, ListBuffer[Tree]]

  def resetTyper: unit = {
    resetContexts
    transformed.clear
    superDefs.clear
  }

  def newTyper(context: Context): Typer = new Typer(context)

  object UnTyper extends Traverser {
    override def traverse(tree: Tree) = {
      if (tree != EmptyTree) tree.tpe = null
      if (tree.hasSymbol) tree.symbol = NoSymbol
      super.traverse(tree)
    }
  }
  def makeNewScope(txt : Context, tree : Tree, sym : Symbol) =
    txt.makeNewScope(tree, sym)
  def newDecls(tree : CompoundTypeTree) = newScope
  def newDecls(tree : Template, clazz : Symbol) = newScope
  def newTemplateScope(impl : Template, clazz : Symbol) = newScope

  // Mode constants

  /** The three mode <code>NOmode</code>, <code>EXPRmode</code>
   *  and <code>PATTERNmode</code> are mutually exclusive.
   */
  val NOmode        = 0x000
  val EXPRmode      = 0x001
  val PATTERNmode   = 0x002
  val TYPEmode      = 0x004

  /** The mode <code>SCCmode</code> is orthogonal to above. When set we are
   *  in the this or super constructor call of a constructor.
   */
  val SCCmode       = 0x008

  /** The mode <code>FUNmode</code> is orthogonal to above.
   *  When set we are looking for a method or constructor.
   */
  val FUNmode       = 0x010

  /** The mode <code>POLYmode</code> is orthogonal to above.
   *  When set expression types can be polymorphic.
   */
  val POLYmode      = 0x020

  /** The mode <code>QUALmode</code> is orthogonal to above. When set
   *  expressions may be packages and Java statics modules.
   */
  val QUALmode      = 0x040

  /** The mode <code>TAPPmode</code> is set for the function/type constructor
   *  part of a type application. When set we do not decompose PolyTypes.
   */
  val TAPPmode      = 0x080

  /** The mode <code>SUPERCONSTRmode</code> is set for the <code>super</code>
   *  in a superclass constructor call <code>super.&lt;init&gt;</code>.
   */
  val SUPERCONSTRmode = 0x100

  /** The mode <code>SNDTRYmode</code> indicates that an application is typed
   *  for the 2nd time. In that case functions may no longer be coerced with
   *  implicit views.
   */
  val SNDTRYmode    = 0x200

  /** The mode <code>LHSmode</code> is set for the left-hand side of an
   *  assignment.
   */
  val LHSmode       = 0x400

  /** The mode <code>CONSTmode</code> is set when expressions should evaluate
   *  to constant sused for attribute arguments.
   */
  val CONSTmode     = 0x800

  /** The mode <code>REGPATmode</code> is set when regular expression patterns
   *  are allowed.
   */
  val REGPATmode    = 0x1000

  private val stickyModes: int  = EXPRmode | PATTERNmode | TYPEmode | CONSTmode

  private def funMode(mode: int) = mode & (stickyModes | SCCmode) | FUNmode | POLYmode

  private var xxx = 10;
  class Typer(context0: Context) {
    import context0.unit

    val infer = new Inferencer(context0) {
      override def isCoercible(tp: Type, pt: Type): boolean = (
        tp.isError || pt.isError ||
        context0.implicitsEnabled && // this condition prevents chains of views
        inferView(NoPos, tp, pt, false) != EmptyTree
      )
    }

    /**
     *  @param pos             ...
     *  @param from            ...
     *  @param to              ...
     *  @param reportAmbiguous ...
     *  @return                ...
     */
    private def inferView(pos: PositionType, from: Type, to: Type, reportAmbiguous: boolean): Tree = {
      if (settings.debug.value) log("infer view from "+from+" to "+to)//debug
      if (phase.id > currentRun.typerPhase.id) EmptyTree
      else from match {
        case MethodType(_, _) => EmptyTree
        case OverloadedType(_, _) => EmptyTree
        case PolyType(_, _) => EmptyTree
        case _ =>
          val result = inferImplicit(pos, functionType(List(from), to), true, reportAmbiguous)
          if (result != EmptyTree) result
          else inferImplicit(
            pos,
            functionType(List(appliedType(ByNameParamClass.typeConstructor, List(from))), to),
            true, reportAmbiguous)
      }
    }

    /**
     *  @param pos             ...
     *  @param from            ...
     *  @param name            ...
     *  @param tp              ...
     *  @param reportAmbiguous ...
     *  @return                ...
     */
    private def inferView(pos: PositionType, from: Type, name: Name, tp: Type, reportAmbiguous: boolean): Tree = {
      val to = refinedType(List(WildcardType), NoSymbol)
      val psym = (if (name.isTypeName) to.symbol.newAbstractType(pos, name)
                  else to.symbol.newValue(pos, name)) setInfo tp
      to.decls.enter(psym)
      inferView(pos, from, to, reportAmbiguous)
    }

    import infer._

    private var namerCache: Namer = null
    def namer = {
      if ((namerCache eq null) || namerCache.context != context)
        namerCache = new Namer(context)
      namerCache
    }

    private var context = context0
    def context1 = context

    /** Report a type error.
     *
     *  @param pos0   The position where to report the error
     *  @param ex     The exception that caused the error
     */
    def reportTypeError(pos0: PositionType, ex: TypeError): unit = {
      if (settings.debug.value) ex.printStackTrace()
      val pos = if (ex.pos == NoPos) pos0 else ex.pos
      ex match {
        case CyclicReference(sym, info: TypeCompleter) =>
          val msg =
            info.tree match {
              case ValDef(_, _, tpt, _) if (tpt.tpe eq null) =>
                "recursive "+sym+" needs type"
              case DefDef(_, _, _, _, tpt, _) if (tpt.tpe eq null) =>
                (if (sym.owner.isClass && sym.owner.info.member(sym.name).hasFlag(OVERLOADED)) "overloaded "
                 else "recursive ")+sym+" needs result type"
              case _ =>
                ex.getMessage()
            }
          if (context.retyping) context.error(pos, msg)
          else context.unit.error(pos, msg)
          if (sym == ObjectClass)
            throw new FatalError("cannot redefine root "+sym)
        case _ =>
          context.error(pos, ex)
      }
    }

    /** Check that <code>tree</code> is a stable expression.
     *
     *  @param tree ...
     *  @return     ...
     */
    def checkStable(tree: Tree): Tree =
      if (treeInfo.isPureExpr(tree)) tree
      else errorTree(tree, "stable identifier required, but " + tree + " found.")

    /** Check that type <code>tp</code> is not a subtype of itself.
     *
     *  @param pos ...
     *  @param tp  ...
     *  @return    <code>true</code> if <code>tp</code> is not a subtype of itself.
     */
    def checkNonCyclic(pos: PositionType, tp: Type): boolean = {
      def checkNotLocked(sym: Symbol): boolean = {
        sym.initialize
        if (sym hasFlag LOCKED) {
          error(pos, "cyclic aliasing or subtyping involving "+sym); false
        } else true
      }
      tp match {
        case TypeRef(pre, sym, args) =>
          (checkNotLocked(sym)) && (
            !sym.isAliasType && !sym.isAbstractType ||
            checkNonCyclic(pos, pre.memberInfo(sym).subst(sym.typeParams, args), sym)
          )
        case SingleType(pre, sym) =>
          checkNotLocked(sym)
        case st: SubType =>
          checkNonCyclic(pos, st.supertype)
        case ct: CompoundType =>
          var p = ct.parents
          while (!p.isEmpty && checkNonCyclic(pos, p.head)) p = p.tail
          p.isEmpty
        case _ =>
          true
      }
    }

    def checkNonCyclic(pos: PositionType, tp: Type, lockedSym: Symbol): boolean = {
      lockedSym.setFlag(LOCKED)
      val result = checkNonCyclic(pos, tp)
      lockedSym.resetFlag(LOCKED)
      result
    }

    def checkNonCyclic(sym: Symbol): unit =
      if (!checkNonCyclic(sym.pos, sym.tpe)) sym.setInfo(ErrorType);

    def checkNonCyclic(defn: Tree, tpt: Tree): unit = {
      if (!checkNonCyclic(defn.pos, tpt.tpe, defn.symbol)) {
        tpt.tpe = ErrorType
        defn.symbol.setInfo(ErrorType)
      }
    }

    def checkParamsConvertible(pos: PositionType, tpe: Type): unit = tpe match {
      case MethodType(formals, restpe) =>
        if (formals.exists(.symbol.==(ByNameParamClass)) && formals.length != 1)
          error(pos, "methods with `=>'-parameter can be converted to function values only if they take no other parameters")
        if (formals exists (.symbol.==(RepeatedParamClass)))
          error(pos, "methods with `*'-parameters cannot be converted to function values");
        checkParamsConvertible(pos, restpe)
      case _ =>
    }

    def checkRegPatOK(pos: PositionType, mode: int): unit =
      if ((mode & REGPATmode) == 0) {
        error(pos, "no regular expression pattern allowed here\n"+
              "(regular expression patterns are only allowed in arguments to *-parameters)")
      }

    /** Check that type of given tree does not contain local or private
     *  components.
     */
    object checkNoEscaping extends TypeMap {
      private var owner: Symbol = _
      private var scope: Scope = _
      private var badSymbol: Symbol = _

      /** Check that type <code>tree</code> does not refer to private
       *  components unless itself is wrapped in something private
       *  (<code>owner</code> tells where the type occurs).
       *
       *  @param owner ...
       *  @param tree  ...
       *  @return      ...
       */
      def privates[T <: Tree](owner: Symbol, tree: T): T =
        check(owner, EmptyScope, tree)

      /** Check that type <code>tree</code> does not refer to entities
       *  defined in scope <code>scope</code>.
       *
       *  @param scope ...
       *  @param pt    ...
       *  @param tree  ...
       *  @return      ...
       */
      def locals[T <: Tree](scope: Scope, pt: Type, tree: T): T =
        if (isFullyDefined(pt)) tree setType pt else check(NoSymbol, scope, tree)

      def check[T <: Tree](owner: Symbol, scope: Scope, tree: T): T = {
        this.owner = owner
        this.scope = scope
        badSymbol = NoSymbol
        assert(tree.tpe ne null, tree)//debug
        apply(tree.tpe)
        if (badSymbol == NoSymbol) tree
        else if (badSymbol.isErroneous) setError(tree)
        else {
          val tp1 = try {
            heal(tree.tpe)
          } catch {
            case ex: MalformedType =>
              tree.tpe
              // revert to `tree.tpe', because the healing widening operation would introduce
              // a malformed type
          }
          if (tp1 eq tree.tpe) {
            error(tree.pos,
              (if (badSymbol hasFlag PRIVATE) "private " else "") + badSymbol +
              " escapes its defining scope as part of type "+tree.tpe)
            setError(tree)
          } else
            check(owner, scope, tree setType tp1)
        }
      }

      object heal extends TypeMap {
        def apply(tp: Type): Type = tp match {
          case SingleType(pre, sym) =>
            if ((variance == 1) && (pre contains badSymbol)) {
              val tp1 = tp.widen
              if (tp1 contains badSymbol) tp else tp1
            } else tp
          case _ =>
            mapOver(tp)
        }
      }

      override def apply(t: Type): Type = {
        def checkNoEscape(sym: Symbol): unit = {
          if (sym.hasFlag(PRIVATE)) {
            var o = owner
            var leaking = true
            while (o != NoSymbol && o != sym.owner &&
                   !o.isLocal && !o.hasFlag(PRIVATE) &&
                   !o.privateWithin.ownerChain.contains(sym.owner))
              o = o.owner
            if (o == sym.owner) badSymbol = sym
          } else if (sym.owner.isTerm && !sym.isTypeParameterOrSkolem) {
            var e = scope.lookupEntry(sym.name)
            while ((e ne null) && e.owner == scope && badSymbol == NoSymbol) {
              if (e.sym == sym) badSymbol = e.sym
              e = scope.lookupNextEntry(e)
            }
          }
        }
        if (badSymbol == NoSymbol)
          t match {
            case TypeRef(_, sym, _) => checkNoEscape(sym)
            case SingleType(_, sym) => checkNoEscape(sym)
            case _ =>
          }
        mapOver(t)
      }
    }

    def reenterValueParams(vparamss: List[List[ValDef]]): unit =
      for (val vparams <- vparamss; val vparam <- vparams)
        context.scope enter vparam.symbol

    def reenterTypeParams(tparams: List[AbsTypeDef]): List[Symbol] =
      for (val tparam <- tparams) yield {
        context.scope enter tparam.symbol
        tparam.symbol.deSkolemize
      }

    /** The qualifying class of a this or super with prefix <code>qual</code>.
     *
     *  @param tree ...
     *  @param qual ...
     *  @return     ...
     */
    def qualifyingClassContext(tree: Tree, qual: Name): Context = {
      if (qual.isEmpty) {
        if (context.enclClass.owner.isPackageClass)
          error(tree.pos, tree+" can be used only in a class, object, or template")
        context.enclClass
      } else {
        var c = context.enclClass
        while (c != NoContext && c.owner.name != qual) c = c.outer.enclClass
        if (c == NoContext) error(tree.pos, qual+" is not an enclosing class")
        c
      }
    }

    /** <p>
     *    Post-process an identifier or selection node, performing the following:
     *  </p>
     *  <ol>
     *  <!--(1)--><li>Check that non-function pattern expressions are stable</li>
     *  <!--(2)--><li>Check that packages and static modules are not used as values</li>
     *  <!--(3)--><li>Turn tree type into stable type if possible and required by context.</li>
     *  </ol>
     */
    private def stabilize(tree: Tree, pre: Type, mode: int, pt: Type): Tree = {
      def isDeprecated(sym: Symbol) = sym.isDeprecated
      if (tree.symbol.hasFlag(OVERLOADED) && (mode & FUNmode) == 0)
        inferExprAlternative(tree, pt)
      val sym = tree.symbol
      if (!phase.erasedTypes &&
          isDeprecated(sym) && !context.owner.ownerChain.exists(isDeprecated)) {
        unit.deprecationWarning(tree.pos,
          sym+sym.locationString+" is deprecated")
      }
      if (tree.tpe.isError) tree
      else if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) { // (1)
        checkStable(tree)
      } else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !sym.isValue) { // (2)
        errorTree(tree, sym+" is not a value")
      } else {
        if (sym.isStable && pre.isStable && tree.tpe.symbol != ByNameParamClass &&
            (pt.isStable || (mode & QUALmode) != 0 && !sym.isConstant ||
             sym.isModule && !sym.isMethod)) tree.setType(singleType(pre, sym))
        else tree
      }
    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def stabilizeFun(tree: Tree, mode: int, pt: Type): Tree = {
      val sym = tree.symbol
      val pre = tree match {
        case Select(qual, _) => qual.tpe
        case _ => NoPrefix
      }
      if (tree.tpe.isInstanceOf[MethodType] && pre.isStable && sym.tpe.paramTypes.isEmpty &&
          (pt.isStable || (mode & QUALmode) != 0 && !sym.isConstant || sym.isModule))
          tree.setType(MethodType(List(), singleType(pre, sym)))
      else tree
    }

    /** The member with given name of given qualifier tree */
    def member(qual: Tree, name: Name) = qual.tpe match {
      case ThisType(clazz) if (context.enclClass.owner.ownerChain contains clazz) =>
        qual.tpe.member(name)
      case _  =>
        if (phase.next.erasedTypes) qual.tpe.member(name)
        else qual.tpe.nonLocalMember(name)
    }

    def silent(op: Typer => Tree): AnyRef /* in fact, TypeError or Tree */ = try {
      if (context.reportGeneralErrors) {
        val context1 = context.makeSilent(context.reportAmbiguousErrors)
        context1.undetparams = context.undetparams
        context1.savedTypeBounds = context.savedTypeBounds
        val typer1 = newTyper(context1)
        val result = op(typer1)
        context.undetparams = context1.undetparams
        context.savedTypeBounds = context1.savedTypeBounds
        result
      } else {
        op(this)
      }
    } catch {
      case ex: CyclicReference => throw ex
      case ex: TypeError => ex
    }

    /** Perform the following adaptations of expression, pattern or type `tree' wrt to
     *  given mode `mode' and given prototype `pt':
     *  (0) Convert expressions with constant types to literals
     *  (1) Resolve overloading, unless mode contains FUNmode
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, if `pt' is a function type and method is not a constructor,
     *        convert to function by eta-expansion,
     *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt'
     *        and it is not a constructor, apply it to ()
     *  otherwise issue an error
     *  (5) Convert constructors in a pattern as follows:
     *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) If constructor refers to an exractor, convert to application of
     *        unapply or unapplySeq method.
     *
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode nut not FUNmode, check that types are fully parameterized
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is byte, short or char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is Scala.unit.
     *  (13) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
    protected def adapt(tree: Tree, mode: int, pt: Type): Tree = tree.tpe match {
      case ct @ ConstantType(value) if ((mode & TYPEmode) == 0 && (ct <:< pt)) => // (0)
        copy.Literal(tree, value)
      case OverloadedType(pre, alts) if ((mode & FUNmode) == 0) => // (1)
        inferExprAlternative(tree, pt)
        adapt(tree, mode, pt)
      case PolyType(List(), restpe) => // (2)
        adapt(tree setType restpe, mode, pt)
      case TypeRef(_, sym, List(arg))
      if ((mode & EXPRmode) != 0 && sym == ByNameParamClass) => // (2)
        adapt(tree setType arg, mode, pt)
      case PolyType(tparams, restpe) if ((mode & (TAPPmode | PATTERNmode)) == 0) => // (3)
        val tparams1 = cloneSymbols(tparams)
        val tree1 = if (tree.isType) tree
                    else TypeApply(tree, tparams1 map (tparam =>
                      TypeTree(tparam.tpe) setOriginal tree)) setPos tree.pos
        context.undetparams = context.undetparams ::: tparams1
        adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt)
      case mt: ImplicitMethodType if ((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) => // (4.1)
        if (!context.undetparams.isEmpty & (mode & POLYmode) == 0) { // (9)
          val tparams = context.undetparams
          context.undetparams = List()
          inferExprInstance(tree, tparams, pt)
          adapt(tree, mode, pt)
        } else typed(applyImplicitArgs(tree), mode, pt)
      case mt: MethodType
      if (((mode & (EXPRmode | FUNmode | LHSmode)) == EXPRmode) &&
          (context.undetparams.isEmpty || (mode & POLYmode) != 0)) =>
        val meth = tree.symbol
        if (!meth.isConstructor &&
            //isCompatible(tparamsToWildcards(mt, context.undetparams), pt) &&
            pt != WildcardType &&
            (pt <:< functionType(mt.paramTypes map (t => WildcardType), WildcardType))) { // (4.2)
          if (settings.debug.value) log("eta-expanding "+tree+":"+tree.tpe+" to "+pt)
          checkParamsConvertible(tree.pos, tree.tpe)
          typed(etaExpand(tree), mode, pt)
        } else if (!meth.isConstructor && mt.paramTypes.isEmpty) { // (4.3)
          adapt(typed(Apply(tree, List()) setPos tree.pos), mode, pt)
        } else if (context.implicitsEnabled) {
          errorTree(tree, "missing arguments for "+meth+meth.locationString+
                    (if (meth.isConstructor) ""
                     else ";\nprefix this method with `&' if you want to treat it as a partially applied function"))
        } else {
          setError(tree)
        }
      case _ =>
        if (tree.isType) {
          if ((mode & FUNmode) != 0) {
            tree
          } else if (tree.hasSymbol && !tree.symbol.typeParams.isEmpty) { // (7)
            errorTree(tree, tree.symbol+" takes type parameters")
          } else tree match { // (6)
            case TypeTree() => tree
            case _ => TypeTree(tree.tpe) setOriginal(tree)
          }
        } else if ((mode & (PATTERNmode | FUNmode)) == (PATTERNmode | FUNmode)) { // (5)
          val constr = tree.symbol.filter(.isCaseFactory)
          if (constr != NoSymbol) {
            val clazz = constr.tpe.finalResultType.symbol
            assert(clazz hasFlag CASE, tree)
            val prefix = tree.tpe.finalResultType.prefix
            val tree1 = TypeTree(clazz.primaryConstructor.tpe.asSeenFrom(prefix, clazz.owner)) setOriginal tree
            try {
              inferConstructorInstance(tree1, clazz.typeParams, widen(pt))
            } catch {
              case tpe : TypeError => throw tpe
              case t : Throwable =>
                logError("CONTEXT: " + context.unit.source.dbg(tree.pos), t)
              throw t
            }
            tree1
          } else {
            val extractor = tree.symbol.filter(sym => definitions.unapplyMember(sym.tpe).exists)
            if (extractor != NoSymbol) {
              tree setSymbol extractor
            } else {
              errorTree(tree, tree.symbol + " is not a case class constructor, nor does it have an unapply/unapplySeq method")
            }
          }
        } else if ((mode & (EXPRmode | FUNmode)) == (EXPRmode | FUNmode) &&
                   !tree.tpe.isInstanceOf[MethodType] &&
                   !tree.tpe.isInstanceOf[OverloadedType] &&
                   ((mode & TAPPmode) == 0 || tree.tpe.typeParams.isEmpty) &&
                   member(adaptToName(tree, nme.apply), nme.apply)
                     .filter(m => m.tpe.paramSectionCount > 0) != NoSymbol) { // (8)
          val qual = adaptToName(tree, nme.apply) match {
            case id @ Ident(_) =>
              val pre = if (id.symbol.owner.isPackageClass) id.symbol.owner.thisType
                        else if (id.symbol.owner.isClass)
                          context.enclosingSubClassContext(id.symbol.owner).prefix
                        else NoPrefix
              stabilize(id, pre, EXPRmode | QUALmode, WildcardType)
            case sel @ Select(qualqual, _) =>
              stabilize(sel, qualqual.tpe, EXPRmode | QUALmode, WildcardType)
            case other =>
              other
          }
          typed(atPos(tree.pos)(Select(qual, nme.apply)), mode, pt)
        } else if (!context.undetparams.isEmpty && (mode & POLYmode) == 0) { // (9)
          instantiate(tree, mode, pt)
        } else if (tree.tpe <:< pt) {
          tree
        } else {
          if ((mode & PATTERNmode) != 0) {
            if ((tree.symbol ne null) && tree.symbol.isModule)
              inferModulePattern(tree, pt)
            if (isPopulated(tree.tpe, approximateAbstracts(pt)))
              return tree
          }
          val tree1 = constfold(tree, pt) // (10) (11)
          if (tree1.tpe <:< pt) adapt(tree1, mode, pt)
          else {
            if ((mode & (EXPRmode | FUNmode)) == EXPRmode) {
              pt match {
                case TypeRef(_, sym, _) =>
                  // note: was if (pt.symbol == UnitClass) but this leads to a potentially
                  // infinite expansion if pt is constant type ()
                  if (sym == UnitClass && tree.tpe <:< AnyClass.tpe) // (12)
                    return typed(atPos(tree.pos)(Block(List(tree), Literal(()))), mode, pt)
                case _ =>
              }
              if (!context.undetparams.isEmpty) {
                return instantiate(tree, mode, pt)
              }
              if (context.implicitsEnabled && !tree.tpe.isError && !pt.isError) {
                // (13); the condition prevents chains of views
                if (settings.debug.value) log("inferring view from "+tree.tpe+" to "+pt)
                val coercion = inferView(tree.pos, tree.tpe, pt, true)
                // convert forward views of delegate types into closures wrapped around
                // the delegate's apply method (the "Invoke" method, which was translated into apply)
                if (forMSIL && coercion != null && isCorrespondingDelegate(tree.tpe, pt)) {
                  val meth: Symbol = tree.tpe.member(nme.apply)
                  if(settings.debug.value)
                    log("replacing forward delegate view with: " + meth + ":" + meth.tpe)
                  return typed(Select(tree, meth), mode, pt)
                }
                if (coercion != EmptyTree) {
                  if (settings.debug.value) log("inferred view from "+tree.tpe+" to "+pt+" = "+coercion+":"+coercion.tpe)
                  return typed(Apply(coercion, List(tree)) setPos tree.pos, mode, pt)
                }
              }
            }
            if (settings.debug.value) log("error tree = "+tree)
            typeErrorTree(tree, tree.tpe, pt)
          }
        }
    }
//      Console.println("adapt "+tree+":"+tree.tpe+", mode = "+mode+", pt = "+pt)
//      adapt(tree, mode, pt)
//    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def instantiate(tree: Tree, mode: int, pt: Type): Tree = {
      val tparams = context.undetparams
      context.undetparams = List()
      inferExprInstance(tree, tparams, pt)
      adapt(tree, mode, pt)
    }

    /**
     *  @param qual ...
     *  @param name ...
     *  @param tp   ...
     *  @return     ...
     */
    def adaptToMember(qual: Tree, name: Name, tp: Type): Tree = {
      val qtpe = qual.tpe.widen
      if (qual.isTerm && ((qual.symbol eq null) || !qual.symbol.isTerm || qual.symbol.isValue) &&
          phase.id <= currentRun.typerPhase.id && !qtpe.isError && !tp.isError &&
          qtpe.symbol != AllRefClass && qtpe.symbol != AllClass && qtpe != WildcardType) {
        val coercion = inferView(qual.pos, qtpe, name, tp, true)
        if (coercion != EmptyTree)
          typedQualifier(atPos(qual.pos)(Apply(coercion, List(qual))))
        else qual
      } else qual
    }

    def adaptToName(qual: Tree, name: Name) =
      if (member(qual, name) != NoSymbol) qual
      else adaptToMember(qual, name, WildcardType)

    private def completeParentType(tpt: Tree, tparams: List[Symbol], enclTparams: List[Symbol], vparamss: List[List[ValDef]], superargs: List[Tree]): Type = {
      enclTparams foreach context.scope.enter
      namer.enterValueParams(context.owner, vparamss)
      val newTree = New(tpt)
        .setType(PolyType(tparams, appliedType(tpt.tpe, tparams map (.tpe))))
      val tree = typed(atPos(tpt.pos)(Apply(Select(newTree, nme.CONSTRUCTOR), superargs)))
      if (settings.debug.value) log("superconstr "+tree+" co = "+context.owner);//debug
      tree.tpe
    }

    def parentTypes(templ: Template): List[Tree] = try {
      if (templ.parents.isEmpty) List()
      else {
        var supertpt = typedTypeConstructor(templ.parents.head)
        val firstParent = supertpt.tpe.symbol
        var mixins = templ.parents.tail map typedType
        // If first parent is a trait, make it first mixin and add its superclass as first parent
        while ((supertpt.tpe.symbol ne null) && supertpt.tpe.symbol.initialize.isTrait) {
          val supertpt1 = typedType(supertpt)
          if (!supertpt1.tpe.isError) {
            mixins = supertpt1 :: mixins
            supertpt = TypeTree(supertpt1.tpe.parents.head) setOriginal supertpt /* setPos supertpt.pos */
          }
        }
        val constr = treeInfo.firstConstructor(templ.body)
        if (supertpt.tpe.symbol != firstParent) {
          constr match {
            case DefDef(_, _, _, _, _, Apply(_, superargs)) =>
              if (!superargs.isEmpty)
                error(superargs.head.pos, firstParent+" is a trait; does not take constructor arguments")
            case _ =>
          }
        }
        if (supertpt.hasSymbol) {
          val tparams = supertpt.symbol.typeParams
          if (!tparams.isEmpty) {
            constr match {
              case EmptyTree =>
                error(supertpt.pos, "missing type arguments")
              case DefDef(_, _, _, vparamss, _, Apply(_, superargs)) =>
                val outercontext = context.outer
                supertpt = TypeTree(
                  newTyper(makeNewScope(outercontext, constr, outercontext.owner))
                    .completeParentType(
                      supertpt,
                      tparams,
                      context.owner.unsafeTypeParams,
                      vparamss map (.map(.duplicate)),
                      superargs map (.duplicate))) setOriginal supertpt /* setPos supertpt.pos */
            }
          }
        }
        //Console.println("parents("+context.owner+") = "+supertpt :: mixins);//DEBUG
        List.mapConserve(supertpt :: mixins)(tpt => checkNoEscaping.privates(context.owner, tpt))
      }
    } catch {
      case ex: TypeError =>
        templ.tpe = null
        reportTypeError(templ.pos, ex)
        List(TypeTree(AnyRefClass.tpe))
    }

    /** <p>Check that</p>
     *  <ul>
     *    <li>all parents are class types,</li>
     *    <li>first parent class is not a mixin; following classes are mixins,</li>
     *    <li>final classes are not inherited,</li>
     *    <li>
     *      sealed classes are only inherited by classes which are
     *      nested within definition of base class, or that occur within same
     *      statement sequence,
     *    </li>
     *    <li>self-type of current class is a subtype of self-type of each parent class.</li>
     *    <li>no two parents define same symbol.</li>
     *  </ul>
     */
    def validateParentClasses(parents: List[Tree], selfType: Type): unit = {

      def validateParentClass(parent: Tree, superclazz: Symbol): unit = {
        if (!parent.tpe.isError) {
          val psym = parent.tpe.symbol.initialize
          if (!psym.isClass) {
            error(parent.pos, "class type expected")
          } else if (psym != superclazz) {
            if (psym.isTrait) {
              val ps = psym.info.parents
              if (!ps.isEmpty && !superclazz.isSubClass(ps.head.symbol))
                error(parent.pos, "illegal inheritance; super"+superclazz+
                      "\n is not a subclass of the super"+ps.head.symbol+
                      "\n of the mixin " + psym);
            } else if (settings.migrate.value) {
              error(parent.pos, migrateMsg+psym+" needs to be a declared as a trait")
            }else {
              error(parent.pos, psym+" needs to be a trait be mixed in")
            }
          } else if (psym hasFlag FINAL) {
            error(parent.pos, "illegal inheritance from final class")
          } else if (psym.isSealed && !phase.erasedTypes) {
            if (context.unit.source.file != psym.sourceFile)
              error(parent.pos, "illegal inheritance from sealed "+psym)
            else
              psym addChild context.owner
          }
          if (!(selfType <:< parent.tpe.typeOfThis) && !phase.erasedTypes) {
            //Console.println(context.owner);//DEBUG
            //Console.println(context.owner.unsafeTypeParams);//DEBUG
            //Console.println(List.fromArray(context.owner.info.closure));//DEBUG
            error(parent.pos, "illegal inheritance;\n self-type "+
                  selfType+" does not conform to "+parent +
                  "'s selftype "+parent.tpe.typeOfThis)
            if (settings.explaintypes.value) explainTypes(selfType, parent.tpe.typeOfThis)
          }
          if (parents exists (p => p != parent && p.tpe.symbol == psym && !psym.isError))
            error(parent.pos, psym+" is inherited twice")
        }
      }

      if (!parents.isEmpty && !parents.head.tpe.isError)
        for (val p <- parents) validateParentClass(p, parents.head.tpe.symbol)
    }

    def checkFinitary(classinfo: ClassInfoType) {
      val clazz = classinfo.symbol
      for (val tparam <- clazz.typeParams) {
        if (classinfo.expansiveRefs(tparam) contains tparam) {
          error(tparam.pos, "class graph is not finitary because type parameter "+tparam.name+" is expansively recursive")
          val newinfo = ClassInfoType(
            classinfo.parents map (.subst(List(tparam), List(AnyRefClass.tpe))),
            classinfo.decls,
            clazz)
          clazz.setInfo {
            clazz.info match {
              case PolyType(tparams, _) => PolyType(tparams, newinfo)
              case _ => newinfo
            }
          }
        }
      }
    }

    /**
     *  @param cdef ...
     *  @return     ...
     */
    def typedClassDef(cdef: ClassDef): Tree = {
//      attributes(cdef)
      val clazz = cdef.symbol
      reenterTypeParams(cdef.tparams)
      val tparams1 = List.mapConserve(cdef.tparams)(typedAbsTypeDef)
      val self1 = cdef.self match {
        case ValDef(mods, name, tpt, EmptyTree) =>
          val tpt1 = checkNoEscaping.privates(clazz.thisSym, typedType(tpt))
          copy.ValDef(cdef.self, mods, name, tpt1, EmptyTree) setType NoType
      }
      val implScope = newScope
      if (self1.name != nme.WILDCARD) implScope enter self1.symbol
      val impl1 = newTyper(context.make(cdef.impl, clazz, implScope))
        .typedTemplate(cdef.impl, parentTypes(cdef.impl))
      val impl2 = addSyntheticMethods(impl1, clazz, context.unit)
      val ret = copy.ClassDef(cdef, cdef.mods, cdef.name, tparams1, self1, impl2)
        .setType(NoType)
      ret
    }

    /**
     *  @param mdef ...
     *  @return     ...
     */
    def typedModuleDef(mdef: ModuleDef): Tree = {
      //Console.println("sourcefile of " + mdef.symbol + "=" + mdef.symbol.sourceFile)
//      attributes(mdef)
      val clazz = mdef.symbol.moduleClass
      val impl1 = newTyper(context.make(mdef.impl, clazz, newTemplateScope(mdef.impl, clazz)))
        .typedTemplate(mdef.impl, parentTypes(mdef.impl))
      val impl2 = addSyntheticMethods(impl1, clazz, context.unit)

      copy.ModuleDef(mdef, mdef.mods, mdef.name, impl2) setType NoType
    }

    /**
     *  @param stat ...
     *  @return     ...
     */
    def addGetterSetter(stat: Tree): List[Tree] = stat match {
      case ValDef(mods, name, tpt, rhs)
      if (mods.flags & (PRIVATE | LOCAL)) != (PRIVATE | LOCAL) && !stat.symbol.isModuleVar =>
        val vdef = copy.ValDef(stat, mods | PRIVATE | LOCAL, nme.getterToLocal(name), tpt, rhs)
        val value = vdef.symbol
        val getter = if (mods hasFlag DEFERRED) value else value.getter(value.owner)
        assert(getter != NoSymbol, stat)
        if (getter hasFlag OVERLOADED)
          error(getter.pos, getter+" is defined twice")
        val getterDef: DefDef = {
          getter.attributes = value.initialize.attributes
          val result = DefDef(getter, vparamss =>
              if (mods hasFlag DEFERRED) EmptyTree
              else typed(atPos(vdef.pos)(Select(This(value.owner), value)), EXPRmode, value.tpe))
          result.tpt.asInstanceOf[TypeTree] setOriginal tpt /* setPos tpt.pos */
          checkNoEscaping.privates(getter, result.tpt)
          copy.DefDef(result, result.mods withAnnotations mods.annotations, result.name,
                      result.tparams, result.vparamss, result.tpt, result.rhs)
          //todo: withAnnotations is probably unnecessary
        }
        def setterDef: DefDef = {
          val setr = getter.setter(value.owner)
          setr.attributes = value.attributes
          val result = atPos(vdef.pos)(
            DefDef(setr, vparamss =>
              if ((mods hasFlag DEFERRED) || (setr hasFlag OVERLOADED))
                EmptyTree
              else
                typed(Assign(Select(This(value.owner), value),
                             Ident(vparamss.head.head)))))
          copy.DefDef(result, result.mods withAnnotations mods.annotations, result.name,
                      result.tparams, result.vparamss, result.tpt, result.rhs)
        }
        val gs = if (mods hasFlag MUTABLE) List(getterDef, setterDef)
                 else List(getterDef)
        if (mods hasFlag DEFERRED) gs else vdef :: gs

      case DocDef(comment, defn) =>
        addGetterSetter(defn) map (stat => DocDef(comment, stat))

      case Annotated(annot, defn) =>
        addGetterSetter(defn) map (stat => Annotated(annot, stat))

      case _ =>
        List(stat)
    }

    protected def enterSyms(txt : Context, trees : List[Tree]) = {
      var txt0 = txt;
      for (val tree <- trees) txt0 = enterSym(txt0, tree)
    }
    protected def enterSym(txt : Context, tree : Tree) : Context =
      if (txt eq context) namer.enterSym(tree)
      else new Namer(txt).enterSym(tree)

    /**
     *  @param templ    ...
     *  @param parents1 ...
     *  @return         ...
     */
    def typedTemplate(templ: Template, parents1: List[Tree]): Template = {
      val clazz = context.owner
      if (templ.symbol == NoSymbol)
        templ setSymbol clazz.newLocalDummy(templ.pos)
      val selfType =
        if (clazz.isAnonymousClass && !phase.erasedTypes)
          intersectionType(clazz.info.parents, clazz.owner)
        else clazz.typeOfThis
      // the following is necessary for templates generated later
      enterSyms(context.outer.make(templ, clazz, clazz.info.decls), templ.body)
      validateParentClasses(parents1, selfType)
      if (!phase.erasedTypes)
        checkFinitary(clazz.info.resultType.asInstanceOf[ClassInfoType])
      val body =
        if (phase.id <= currentRun.typerPhase.id && !reporter.hasErrors)
          templ.body flatMap addGetterSetter
        else templ.body
      val body1 = typedStats(body, templ.symbol)
      copy.Template(templ, parents1, body1) setType clazz.tpe
    }

    /**
     *  @param vdef ...
     *  @return     ...
     */
    def typedValDef(vdef: ValDef): ValDef = {
//      attributes(vdef)
      val sym = vdef.symbol
      val typer1 = if (sym.hasFlag(PARAM) && sym.owner.isConstructor)
                     newTyper(context.makeConstructorContext)
                   else this
      var tpt1 = checkNoEscaping.privates(sym, typer1.typedType(vdef.tpt))
      checkNonCyclic(vdef, tpt1)
      val rhs1 =
        if (vdef.rhs.isEmpty) {
          if (sym.isVariable && sym.owner.isTerm && phase.id <= currentRun.typerPhase.id)
            error(vdef.pos, "local variables must be initialized")
          vdef.rhs
        } else {
          newTyper(context.make(vdef, sym)).transformedOrTyped(vdef.rhs, tpt1.tpe)
        }
      copy.ValDef(vdef, vdef.mods, vdef.name, tpt1, checkDead(rhs1)) setType NoType
    }

    /** Enter all aliases of local parameter accessors.
     *
     *  @param clazz    ...
     *  @param vparamss ...
     *  @param rhs      ...
     */
    def computeParamAliases(clazz: Symbol, vparamss: List[List[ValDef]], rhs: Tree): unit = {
      if (settings.debug.value) log("computing param aliases for "+clazz+":"+clazz.primaryConstructor.tpe+":"+rhs);//debug
      def decompose(call: Tree): (Tree, List[Tree]) = call match {
        case Apply(fn, args) =>
          val (superConstr, args1) = decompose(fn)
          val formals = fn.tpe.paramTypes
          val args2 = if (formals.isEmpty || formals.last.symbol != RepeatedParamClass) args
                      else args.take(formals.length - 1) ::: List(EmptyTree)
          if (args2.length != formals.length)
            assert(false, "mismatch " + clazz + " " + formals + " " + args2);//debug
          (superConstr, args1 ::: args2)
        case Block(stats, expr) =>
          decompose(stats.head)
        case _ =>
          (call, List())
      }
      val (superConstr, superArgs) = decompose(rhs)
      assert(superConstr.symbol ne null)//debug
      if (superConstr.symbol.isPrimaryConstructor) {
        val superClazz = superConstr.symbol.owner
        if (!superClazz.hasFlag(JAVA)) {
          val superParamAccessors = superClazz.constrParamAccessors
          if (superParamAccessors.length != superArgs.length) {
            Console.println("" + superClazz + ":" +
              superClazz.info.decls.toList.filter(.hasFlag(PARAMACCESSOR)))
            assert(false, "mismatch: " + superParamAccessors +
                          ";" + rhs + ";" + superClazz.info.decls)//debug
          }
          List.map2(superParamAccessors, superArgs) { (superAcc, superArg) =>
            superArg match {
              case Ident(name) =>
                if (vparamss.exists(.exists(vp => vp.symbol == superArg.symbol))) {
                  var alias = superAcc.initialize.alias
                  if (alias == NoSymbol)
                    alias = superAcc.getter(superAcc.owner)
                  if (alias != NoSymbol &&
                      superClazz.info.nonPrivateMember(alias.name) != alias)
                    alias = NoSymbol
                  if (alias != NoSymbol) {
                    var ownAcc = clazz.info.decl(name).suchThat(.hasFlag(PARAMACCESSOR))
                    if ((ownAcc hasFlag ACCESSOR) && !(ownAcc hasFlag DEFERRED))
                      ownAcc = ownAcc.accessed
                    if (!ownAcc.isVariable && !alias.accessed.isVariable) {
                      if (settings.debug.value)
                        log("" + ownAcc + " has alias "+alias + alias.locationString);//debug
                      ownAcc.asInstanceOf[TermSymbol].setAlias(alias)
                    }
                  }
                }
              case _ =>
            }
          }
          ()
        }
      }
    }

    /**
     *  @param ddef ...
     *  @return     ...
     */
    def typedDefDef(ddef: DefDef): DefDef = {
//      attributes(ddef)

      val meth = ddef.symbol

      def checkPrecedes(tree: Tree): unit = tree match {
        case Block(stat :: _, _) => checkPrecedes(stat)
        case Apply(fun, _) =>
          if (fun.symbol.isConstructor &&
              fun.symbol.owner == meth.owner && fun.symbol.pos >= meth.pos)
            error(fun.pos, "called constructor's definition must precede calling constructor's definition")
        case _ =>
      }
      def typedSuperCall(tree: Tree, pt: Type): Tree = {
        val result = typed(tree, EXPRmode | SCCmode, pt)
        checkPrecedes(result)
        result
      }

      reenterTypeParams(ddef.tparams)
      reenterValueParams(ddef.vparamss)
      val tparams1 = List.mapConserve(ddef.tparams)(typedAbsTypeDef)
      val vparamss1 = List.mapConserve(ddef.vparamss)(vparams1 =>
        List.mapConserve(vparams1)(typedValDef))
      for (val vparams <- vparamss1; val vparam <- vparams) {
        checkNoEscaping.locals(context.scope, WildcardType, vparam.tpt); ()
      }
      var tpt1 =
        checkNoEscaping.locals(context.scope, WildcardType,
          checkNoEscaping.privates(meth,
              typedType(ddef.tpt)))
      checkNonCyclic(ddef, tpt1)
      ddef.tpt.setType(tpt1.tpe)
      var rhs1 =
        if (ddef.name == nme.CONSTRUCTOR) {
          if (!meth.hasFlag(SYNTHETIC) &&
              !(meth.owner.isClass ||
                meth.owner.isModuleClass ||
                meth.owner.isAnonymousClass ||
                meth.owner.isRefinementClass))
            error(ddef.pos, "constructor definition not allowed here "+meth.owner)//debug
          val result = ddef.rhs match {
            case block @ Block(stat :: stats, expr) =>
              // the following makes sure not to copy the tree if no subtrees have changed
              val stat1 = typedSuperCall(stat, WildcardType)
              val block1 = newTyper(context.makeConstructorSuffixContext)
                .typedBlock(Block(stats, expr) setPos block.pos, EXPRmode, UnitClass.tpe)
              val stats1 = if ((stat eq stat1) && (stats eq block1.stats)) block.stats
                           else stat1 :: block1.stats
              copy.Block(block, stats1, block1.expr) setType block1.tpe
            case _ =>
              typedSuperCall(ddef.rhs, UnitClass.tpe)
          }
          if (meth.isPrimaryConstructor && phase.id <= currentRun.typerPhase.id && !reporter.hasErrors)
            computeParamAliases(meth.owner, vparamss1, result)
          result
        } else transformedOrTyped(ddef.rhs, tpt1.tpe)
      if (tpt1.tpe.symbol != AllClass && !context.returnsSeen) rhs1 = checkDead(rhs1)
      copy.DefDef(ddef, ddef.mods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def typedAbsTypeDef(tdef: AbsTypeDef): AbsTypeDef = {
      val lo1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.lo))
      val hi1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.hi))
      checkNonCyclic(tdef.symbol)
      if (!(lo1.tpe <:< hi1.tpe))
        error(tdef.pos,
              "lower bound "+lo1.tpe+" does not conform to upper bound "+hi1.tpe)
      copy.AbsTypeDef(tdef, tdef.mods, tdef.name, lo1, hi1) setType NoType
    }

    def typedAliasTypeDef(tdef: AliasTypeDef): AliasTypeDef = {
      reenterTypeParams(tdef.tparams)
      val tparams1 = List.mapConserve(tdef.tparams)(typedAbsTypeDef)
      val rhs1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.rhs))
      checkNonCyclic(tdef.symbol)
      copy.AliasTypeDef(tdef, tdef.mods, tdef.name, tparams1, rhs1) setType NoType
    }

    private def enterLabelDef(stat: Tree): unit = stat match {
      case ldef @ LabelDef(_, _, _) =>
        if (ldef.symbol == NoSymbol)
          ldef.symbol = namer.enterInScope(
            context.owner.newLabel(ldef.pos, ldef.name) setInfo MethodType(List(), UnitClass.tpe))
      case _ =>
    }

    def typedLabelDef(ldef: LabelDef): LabelDef = {
      val restpe = ldef.symbol.tpe.resultType
      val rhs1 = typed(ldef.rhs, restpe)
      ldef.params foreach (param => param.tpe = param.symbol.tpe)
      copy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
    }

    /**
     *  @param clazz ...
     *  @return      ...
     */
    def anonymousClassRefinement(clazz: Symbol): Type = {
      val tp = refinedType(clazz.info.parents, clazz.owner)
      val thistp = tp.symbol.thisType
      def canBeInRefinement(sym: Symbol) =
        settings.Xexperimental.value ||
        tp.nonPrivateMember(sym.name).filter { other =>
          !other.isTerm || (thistp.memberType(other) matches thistp.memberType(sym))
        } != NoSymbol
      for (val sym <- clazz.info.decls.toList) {
        if (sym.isPublic && !sym.isClass && !sym.isConstructor && canBeInRefinement(sym))
          addMember(thistp, tp, sym)
      }
      tp
    }

    /**
     *  @param block ...
     *  @param mode  ...
     *  @param pt    ...
     *  @return      ...
     */
    def typedBlock(block: Block, mode: int, pt: Type): Block = {
      if (context.retyping) {
        for (val stat <- block.stats) {
          if (stat.isDef) context.scope.enter(stat.symbol)
        }
      }
      if (!inIDE)
        namer.enterSyms(block.stats)
      block.stats foreach enterLabelDef
      val stats1 = typedStats(block.stats, context.owner)
      val expr1 = typed(block.expr, mode & ~(FUNmode | QUALmode), pt)
      val block1 = copy.Block(block, stats1, expr1)
        .setType(if (treeInfo.isPureExpr(block)) expr1.tpe else expr1.tpe.deconst)
      if (isFullyDefined(pt)) block1
      else {
        if (block1.tpe.symbol.isAnonymousClass)
          block1 setType anonymousClassRefinement(block1.tpe.symbol)
        checkNoEscaping.locals(context.scope, pt, block1)
      }
    }

    /**
     *  @param cdef   ...
     *  @param pattpe ...
     *  @param pt     ...
     *  @return       ...
     */
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef = {
      val pat1: Tree = typedPattern(cdef.pat, pattpe)
      //Console.println("UNAPPLY3:"+pat1+":"+pat1.tpe)
      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree
                         else typed(cdef.guard, BooleanClass.tpe)
      var body1: Tree = typed(cdef.body, pt)
      if (!context.savedTypeBounds.isEmpty) {
        body1.tpe = context.restoreTypeBounds(body1.tpe)
        if (isFullyDefined(pt))
        // the following is a hack to make the pattern matcher work !!! (still needed?)
          body1 =
            typed {
              atPos(body1.pos) {
                TypeApply(Select(body1, Any_asInstanceOf), List(TypeTree(pt)))
              }
            }
      }
      body1 = checkNoEscaping.locals(context.scope, pt, body1)
      copy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
    }

    def typedCases(tree: Tree, cases: List[CaseDef], pattp0: Type, pt: Type): List[CaseDef] = {
      var pattp = pattp0
      List.mapConserve(cases) ( cdef =>
          newTyper(makeNewScope(context, cdef, context.owner)).typedCase(cdef, pattp, pt))
/* not yet!
        cdef.pat match {
          case Literal(Constant(null)) =>
            if (!(pattp <:< NonNullClass.tpe))
              pattp = intersectionType(List(pattp, NonNullClass.tpe), context.owner)
          case _ =>
        }
        result
*/
    }

    /**
     *  @param fun  ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typedFunction(fun: Function, mode: int, pt: Type): Tree = {
      val codeExpected = !forCLDC && !forMSIL && (pt.symbol isNonBottomSubClass CodeClass)

      def decompose(pt: Type): (Symbol, List[Type], Type) =
        if ((isFunctionType(pt)
             ||
             pt.symbol == PartialFunctionClass &&
             fun.vparams.length == 1 && fun.body.isInstanceOf[Match])
            && // see bug901 for a reason why next conditions are neeed
            (pt.typeArgs.length - 1 == fun.vparams.length
             ||
             fun.vparams.exists(.tpt.isEmpty)))
          (pt.symbol, pt.typeArgs.init, pt.typeArgs.last)
        else
          (FunctionClass(fun.vparams.length), fun.vparams map (x => NoType), WildcardType)

      val (clazz, argpts, respt) = decompose(if (codeExpected) pt.typeArgs.head else pt)

      if (fun.vparams.length != argpts.length)
        errorTree(fun, "wrong number of parameters; expected = " + argpts.length)
      else {
        val vparamSyms = List.map2(fun.vparams, argpts) { (vparam, argpt) =>
          if (vparam.tpt.isEmpty)
            vparam.tpt.tpe =
              if (argpt == NoType || argpt == WildcardType) {
                error(vparam.pos, "missing parameter type"); ErrorType
              }
              else argpt
          enterSym(context, vparam)
          if (context.retyping) context.scope enter vparam.symbol
          vparam.symbol
        }
        // XXX: here to for IDE hooks.
        val vparams = List.mapConserve(fun.vparams)(typedValDef)
        for (val vparam <- vparams) {
          checkNoEscaping.locals(context.scope, WildcardType, vparam.tpt); ()
        }
        val body = checkNoEscaping.locals(context.scope, respt, typed(fun.body, respt))
        val formals = vparamSyms map (.tpe)
        val restpe = body.tpe.deconst
        val funtpe = typeRef(clazz.tpe.prefix, clazz, formals ::: List(restpe))
        val fun1 = copy.Function(fun, vparams, checkNoEscaping.locals(context.scope, restpe, body))
          .setType(funtpe)
        if (codeExpected) {
          val liftPoint = Apply(Select(Ident(CodeModule), nme.lift_), List(fun1))
          typed(atPos(fun.pos)(liftPoint))
        } else fun1
      }
    }

    def typedRefinement(stats: List[Tree]): List[Tree] = {
      if (!inIDE)
        namer.enterSyms(stats)
      val stats1 = typedStats(stats, NoSymbol)
      for (val stat <- stats1; stat.isDef) {
        val member = stat.symbol
        if (context.owner.info.baseClasses.tail forall
            (bc => member.matchingSymbol(bc, context.owner.thisType) == NoSymbol)) {
          if (!settings.Xexperimental.value)
            error(member.pos, member.toString+" does not refine a member of its base type")
        } else {
          member setFlag OVERRIDE
        }
      }
      stats1
    }

    def typedImport(imp : Import) : Import = imp;

    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val inBlock = exprOwner == context.owner
      def typedStat(stat: Tree): Tree = {
        if (context.owner.isRefinementClass && !treeInfo.isDeclaration(stat))
          errorTree(stat, "only declarations allowed here")
        stat match {
          case imp @ Import(_, _) =>
            val imp0 = typedImport(imp)
            if (imp0 ne null) {
              context = context.makeNewImport(imp0)
              imp0.symbol.initialize
            }
            EmptyTree
          case _ =>
            val localTyper = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) this
                             else newTyper(context.make(stat, exprOwner))
            checkDead(localTyper.typed(stat))
        }
      }
      def accesses(accessor: Symbol, accessed: Symbol) =
        (accessed hasFlag LOCAL) && (accessed hasFlag PARAMACCESSOR) ||
        (accessor hasFlag ACCESSOR) &&
        !(accessed hasFlag ACCESSOR) && accessed.isPrivateLocal
      def checkNoDoubleDefs(stats: List[Tree]) = {
        val scope = if (inBlock) context.scope else context.owner.info.decls;
        var e = scope.elems;
        while ((e ne null) && e.owner == scope) {
          var e1 = scope.lookupNextEntry(e);
          while ((e1 ne null) && e1.owner == scope) {
            if (!accesses(e.sym, e1.sym) && !accesses(e1.sym, e.sym) &&
                (e.sym.isType || inBlock || (e.sym.tpe matches e1.sym.tpe)))
              if (!e.sym.isErroneous && !e1.sym.isErroneous)
                error(e.sym.pos, e1.sym+" is defined twice");
            e1 = scope.lookupNextEntry(e1);
          }
          e = e.next
        }
        stats
      }
      checkNoDoubleDefs(List.mapConserve(stats)(typedStat))
    }

    def typedArg(arg: Tree, mode: int, newmode: int, pt: Type): Tree = {
      val argTyper = if ((mode & SCCmode) != 0) newTyper(context.makeConstructorContext)
                     else this
      checkDead(argTyper.typed(arg, mode & stickyModes | newmode, pt))
    }

    def typedArgs(args: List[Tree], mode: int) =
      List.mapConserve(args)(arg => typedArg(arg, mode, 0, WildcardType))

    def typedArgs(args: List[Tree], mode: int, originalFormals: List[Type], adaptedFormals: List[Type]) = {
      val varargs = isVarArgs(originalFormals)
      if (!args.isEmpty)
        args.last match {
          case Typed(expr, Ident(name)) if (name == nme.WILDCARD_STAR.toTypeName) =>
            if (!varargs)
              error(args.last.pos, "_*-argument does not correspond to *-parameter")
            else if (originalFormals.length != adaptedFormals.length)
              error(args.last.pos, "_*-argument may not appear after other arguments matching a *-parameter")
          case _ =>
        }
      if (varargs && (mode & PATTERNmode) != 0) {
        val nonVarCount = originalFormals.length - 1
        val prefix =
          List.map2(args take nonVarCount, adaptedFormals take nonVarCount) ((arg, formal) =>
            typedArg(arg, mode, 0, formal))
        val suffix =
          List.map2(args drop nonVarCount, adaptedFormals drop nonVarCount) ((arg, formal) =>
            typedArg(arg, mode, REGPATmode, formal))
        prefix ::: suffix
      } else {
        List.map2(args, adaptedFormals)((arg, formal) => typedArg(arg, mode, 0, formal))
      }
    }

    /**
     *  @param tree ...
     *  @param fun0 ...
     *  @param args ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typedApply(tree: Tree, fun0: Tree, args: List[Tree], mode: int, pt: Type): Tree = {
      var fun = fun0
      if (fun.hasSymbol && (fun.symbol hasFlag OVERLOADED)) {
        // preadapt symbol to number of arguments given
        val argtypes = args map (arg => AllClass.tpe)
        val pre = fun.symbol.tpe.prefix
        var sym = fun.symbol filter { alt =>
          isApplicableSafe(context.undetparams, pre.memberType(alt), argtypes, pt)
        }
        if (sym hasFlag OVERLOADED) {
          // eliminate functions that would result from tupling transforms
          val sym1 = sym filter { alt =>
            formalTypes(alt.tpe.paramTypes, argtypes.length).length == argtypes.length
          }
          if (sym1 != NoSymbol) sym = sym1
        }
        if (sym != NoSymbol)
          fun = adapt(fun setSymbol sym setType pre.memberType(sym), funMode(mode), WildcardType)
      }
      fun.tpe match {
        case OverloadedType(pre, alts) =>
          val undetparams = context.undetparams
          context.undetparams = List()
          val args1 = typedArgs(args, mode)
          context.undetparams = undetparams
          inferMethodAlternative(fun, context.undetparams, args1 map (.tpe.deconst), pt)
          typedApply(tree, adapt(fun, funMode(mode), WildcardType), args1, mode, pt)
        case MethodType(formals0, restpe) =>
          val formals = formalTypes(formals0, args.length)
          var args1 = actualArgs(tree.pos, args, formals.length)
          if (args1.length != args.length) {
            silent(.typedApply(tree, fun, args1, mode, pt)) match {
              case t: Tree => t
              case ex => errorTree(tree, "wrong number of arguments for "+treeSymTypeMsg(fun))
            }
          } else if (formals.length != args1.length) {
            errorTree(tree, "wrong number of arguments for "+treeSymTypeMsg(fun))
          } else {
            val tparams = context.undetparams
            context.undetparams = List()
            if (tparams.isEmpty) {
              val args2 = typedArgs(args1, mode, formals0, formals)
              def ifPatternSkipFormals(tp: Type) = tp match {
                case MethodType(_, rtp) if ((mode & PATTERNmode) != 0) => rtp
                case _ => tp
              }

              // Replace the Delegate-Chainer methods += and -= with corresponding
              // + and - calls, which are translated in the code generator into
              // Combine and Remove
              if (forMSIL) {
                fun match {
                  case Select(qual, name) =>
                   if (isSubType(qual.tpe, definitions.DelegateClass.tpe)
                      && (name == encode("+=") || name == encode("-=")))
                     {
                       val n = if (name == encode("+=")) nme.PLUS else nme.MINUS
                       val f = Select(qual, n)
                       // the compiler thinks, the PLUS method takes only one argument,
                       // but he thinks it's an instance method -> still two ref's on the stack
                       //  -> translated by backend
                       val rhs = copy.Apply(tree, f, args1)
                       return typed(Assign(qual, rhs))
                     }
                  case _ => ()
                }
              }

              if (fun.symbol == List_apply && args.isEmpty) {
                atPos(tree.pos) { gen.mkNil setType restpe }
              } else if ((mode & CONSTmode) != 0 && fun.symbol.owner == PredefModule.tpe.symbol && fun.symbol.name == nme.Array) {
                val elems = new Array[Constant](args2.length)
                var i = 0;
                for (val arg <- args2) arg match {
                  case Literal(value) =>
                    elems(i) = value
                    i = i + 1
                  case _ => errorTree(arg, "constant required")
                }
                val arrayConst = new ArrayConstant(elems, restpe)
                Literal(arrayConst) setType mkConstantType(arrayConst)
              }
              else
                constfold(copy.Apply(tree, fun, args2).setType(ifPatternSkipFormals(restpe)))
            } else {
              assert((mode & PATTERNmode) == 0); // this case cannot arise for patterns
              val lenientTargs = protoTypeArgs(tparams, formals, restpe, pt)
              val strictTargs = List.map2(lenientTargs, tparams)((targ, tparam) =>
                if (targ == WildcardType) tparam.tpe else targ)
              def typedArgToPoly(arg: Tree, formal: Type): Tree = {
                val lenientPt = formal.subst(tparams, lenientTargs)
                val arg1 = typedArg(arg, mode, POLYmode, lenientPt)
                val argtparams = context.undetparams
                context.undetparams = List()
                if (!argtparams.isEmpty) {
                  val strictPt = formal.subst(tparams, strictTargs)
                  inferArgumentInstance(arg1, argtparams, strictPt, lenientPt)
                }
                arg1
              }
              val args2 = List.map2(args1, formals)(typedArgToPoly)
              if (args2 exists (.tpe.isError)) setError(tree)
              else {
                if (settings.debug.value) log("infer method inst "+fun+", tparams = "+tparams+", args = "+args2.map(.tpe)+", pt = "+pt+", lobounds = "+tparams.map(.tpe.bounds.lo)+", parambounds = "+tparams.map(.info));//debug
                val undetparams = inferMethodInstance(fun, tparams, args2, pt)
                val result = typedApply(tree, fun, args2, mode, pt)
                context.undetparams = undetparams
                result
              }
            }
          }
        case ErrorType =>
          setError(copy.Apply(tree, fun, args))
        /* --- begin unapply  --- */

        case otpe if (mode & PATTERNmode) != 0 && definitions.unapplyMember(otpe).exists =>
          val unapp = definitions.unapplyMember(otpe)
          assert(unapp.exists, tree)
          val unappType = otpe.memberType(unapp)
          val argDummyType = pt // was unappArg
          val argDummy =  context.owner.newValue(fun.pos, nme.SELECTOR_DUMMY)
            .setFlag(SYNTHETIC)
            .setInfo(argDummyType)
          if (args.length > MaxTupleArity)
            error(fun.pos, "too many arguments for unapply pattern, maximum = "+MaxTupleArity)
          val arg = Ident(argDummy) setType argDummyType
          val oldArgType = arg.tpe
          if (!isApplicableSafe(List(), unappType, List(arg.tpe), WildcardType)) {
            //Console.println("UNAPP: need to typetest, arg.tpe = "+arg.tpe+", unappType = "+unappType)
            def freshArgType(tp: Type): (Type, List[Symbol]) = tp match {
              case MethodType(formals, restpe) =>
                (formals(0), List())
              case PolyType(tparams, restype) =>
                val tparams1 = cloneSymbols(tparams)
                (freshArgType(restype)._1.substSym(tparams, tparams1), tparams1)
              case OverloadedType(_, _) =>
                error(fun.pos, "cannot resolve overloaded unapply")
                (ErrorType, List())
            }
            val (unappFormal, freeVars) = freshArgType(unappType)
            val context1 = context.makeNewScope(context.tree, context.owner)
            freeVars foreach context1.scope.enter
            val typer1 = new Typer(context1)
            arg.tpe = typer1.infer.inferTypedPattern(tree.pos, unappFormal, arg.tpe)
            //todo: replace arg with arg.asInstanceOf[inferTypedPattern(unappFormal, arg.tpe)] instead.
          }

          val fun1untyped = atPos(fun.pos) {
            Apply(
              Select(
                gen.mkAttributedRef(fun.tpe.prefix, fun.symbol) setType null,
                // setType null is necessary so that ref will be stabilized; see bug 881
                unapp),
              List(arg))
          }
          //Console.println("UNAPPLY2 "+fun+"/"+fun.tpe+" "+fun1untyped)
          val fun1 = typed(fun1untyped)
          if (fun1.tpe.isErroneous) setError(tree)
          else {
            val formals0 = unapplyTypeList(fun1.symbol, fun1.tpe)
            val formals1 = formalTypes(formals0, args.length)
            val args1 = typedArgs(args, mode, formals0, formals1)
            if (!isFullyDefined(pt)) assert(false, tree+" ==> "+UnApply(fun1, args1)+", pt = "+pt)
            // restore old type (this will never work, but just pass typechecking)
            arg.tpe = oldArgType
            UnApply(fun1, args1) setPos tree.pos setType pt
          }

/* --- end unapply  --- */
        case _ =>
          errorTree(tree, fun+" does not take parameters")
      }
    }

    def getConstant(tree: Tree): Constant = tree match {
      case Literal(value) => value
      case arg => error(arg.pos, "attribute argument needs to be a constant; found: "+arg); Constant(null)
    }

    def typedAnnotation[T](annot: Annotation, reify: Tree => T): AnnotationInfo[T] = {
      var attrError: Boolean = false;
      def error(pos: PositionType, msg: String): Null = {
        context.error(pos, msg)
        attrError = true
        null
      }
      typed(annot.constr, EXPRmode | CONSTmode, AnnotationClass.tpe) match {
        case t @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          if (t.isErroneous) {
            AnnotationInfo[T](ErrorType, List(), List())
          }
          else {
            val annType = tpt.tpe
            val constrArgs = args map reify
            val attrScope = annType.decls
              .filter(sym => sym.isMethod && !sym.isConstructor && sym.hasFlag(JAVA))
            val names = new collection.mutable.HashSet[Symbol]
            names ++= attrScope.elements.filter(.isMethod)
            if (args.length == 1) {
              names.retain(sym => sym.name != nme.value)
            }
            val nvPairs = annot.elements map {
              case Assign(ntree @ Ident(name), rhs) => {
                val sym = attrScope.lookup(name);
                if (sym == NoSymbol) {
                  error(ntree.pos, "unknown attribute element name: " + name)
                } else if (!names.contains(sym)) {
                  error(ntree.pos, "duplicate value for element " + name)
                } else {
                  names -= sym
                  (sym.name, reify(typed(rhs, EXPRmode | CONSTmode, sym.tpe.resultType)))
                }
              }
            }
            for (val name <- names) {
              if (!name.attributes.contains(AnnotationInfo(AnnotationDefaultAttr.tpe, List(), List()))) {
                error(annot.constr.pos, "attribute " + annType.symbol.fullNameString + " is missing element " + name.name)
              }
            }
            if (annType.symbol.hasFlag(JAVA) && settings.target.value == "jvm-1.4") {
              context.warning (annot.constr.pos, "Java annotation will not be emitted in classfile unless you use the '-target:jvm-1.5' option")
            }
            if (attrError) AnnotationInfo(ErrorType, List(), List())
            else AnnotationInfo(annType, constrArgs, nvPairs)
          }
      }
    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    protected def typed1(tree: Tree, mode: int, pt: Type): Tree = {
      //Console.println("typed1("+tree.getClass()+","+Integer.toHexString(mode)+","+pt+")")
      def ptOrLub(tps: List[Type]) = if (isFullyDefined(pt)) pt else lub(tps)

      def typedTypeApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
        case OverloadedType(pre, alts) =>
          inferPolyAlternatives(fun, args map (.tpe))
          typedTypeApply(fun, args)
        case PolyType(tparams, restpe) if (tparams.length != 0) =>
          if (tparams.length == args.length) {
            val targs = args map (.tpe)
            checkBounds(tree.pos, NoPrefix, NoSymbol, tparams, targs, "")
            if (fun.symbol == Predef_classOf) {
              if (!targs.head.symbol.isClass || targs.head.symbol.isRefinementClass)
                error(args.head.pos, "class type required");
              Literal(Constant(targs.head)) setPos tree.pos setType ClassClass.tpe
            } else
              copy.TypeApply(tree, fun, args) setType restpe.subst(tparams, targs)
          } else {
            errorTree(tree, "wrong number of type parameters for "+treeSymTypeMsg(fun))
          }
        case ErrorType =>
          setError(tree)
        case _ =>
          errorTree(tree, treeSymTypeMsg(fun)+" does not take type parameters.")
        }

      /**
       *  @param args ...
       *  @return     ...
       */
      def tryTypedArgs(args: List[Tree], other : TypeError) : List[Tree] = {
        val c = context.makeSilent(false)
        c.retyping = true
        try {
          newTyper(c).typedArgs(args, mode)
        } catch {
          case ex: TypeError =>
            null
        }
      }

      /** Try to apply function to arguments; if it does not work try to
       *  insert an implicit conversion.
       *
       *  @param fun  ...
       *  @param args ...
       *  @return     ...
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree =
        silent(.typedApply(tree, fun, args, mode, pt)) match {
          case t: Tree =>
            t
          case ex: TypeError =>
            val Select(qual, name) = fun
            val args1 = tryTypedArgs(args, ex)
            val qual1 =
              if ((args1 ne null) && !pt.isError) {
                def templateArgType(arg: Tree) =
                  new BoundedWildcardType(mkTypeBounds(arg.tpe, AnyClass.tpe))
                adaptToMember(qual, name, MethodType(args1 map templateArgType, pt))
              } else qual
            if (qual1 ne qual) {
              val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
              typed1(tree1, mode | SNDTRYmode, pt)
            } else {
              reportTypeError(tree.pos, ex)
              setError(tree)
            }
        }

      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree], ex: TypeError): Tree = {
        val prefix = name.subName(0, name.length - nme.EQL.length)
        def mkAssign(vble: Tree): Tree =
          Assign(
            vble,
              Apply(Select(vble.duplicate, prefix) setPos fun.pos, args) setPos tree.pos
          ) setPos tree.pos
        val tree1 = qual match {
          case Select(qualqual, vname) =>
            gen.evalOnce(qualqual, context.owner, context.unit) { qq =>
              mkAssign(Select(qq(), vname) setPos qual.pos)
            }
          case Apply(Select(table, nme.apply), indices) =>
            gen.evalOnceAll(table :: indices, context.owner, context.unit) { ts =>
              val tab = ts.head
              val is = ts.tail
              Apply(
                 Select(tab(), nme.update) setPos table.pos,
                 ((is map (i => i())) ::: List(
                   Apply(
                     Select(
                       Apply(
                         Select(tab(), nme.apply) setPos table.pos,
                         is map (i => i())) setPos qual.pos,
                       prefix) setPos fun.pos,
                     args) setPos tree.pos)
                 )
               ) setPos tree.pos
             }
           case Ident(_) =>
             mkAssign(qual)
        }
        if (settings.debug.value) log("retry assign: "+tree1)
        silent(.typed1(tree1, mode, pt)) match {
          case t: Tree =>
            t
          case _ =>
            reportTypeError(tree.pos, ex)
            setError(tree)
        }
      }

      /** Attribute a selection where <code>tree</code> is <code>qual.name</code>.
       *  <code>qual</code> is already attributed.
       *
       *  @param qual ...
       *  @param name ...
       *  @return     ...
       */
      def typedSelect(qual: Tree, name: Name): Tree = {
        val sym =
          if (tree.symbol != NoSymbol) {
            if (phase.erasedTypes && qual.isInstanceOf[Super])
              qual.tpe = tree.symbol.owner.tpe
            if (false && settings.debug.value) { // todo: replace by settings.check.value?
              val alts = qual.tpe.member(tree.symbol.name).alternatives
              if (!(alts exists (alt =>
                alt == tree.symbol || alt.isTerm && (alt.tpe matches tree.symbol.tpe))))
                assert(false, "symbol "+tree.symbol+tree.symbol.locationString+" not in "+alts+" of "+qual.tpe+
                       "\n members = "+qual.tpe.members+
                       "\n type history = "+qual.tpe.symbol.infosString+
                       "\n phase = "+phase)
            }
            tree.symbol
          } else {
            member(qual, name)
          }
        if (sym == NoSymbol && name != nme.CONSTRUCTOR && (mode & EXPRmode) != 0) {
          val qual1 = adaptToName(qual, name)
          if (qual1 ne qual) return typed(copy.Select(tree, qual1, name), mode, pt)
        }
        if (!sym.exists) {
          if (settings.debug.value) Console.err.println("qual = "+qual+":"+qual.tpe+"\nSymbol="+qual.tpe.symbol+"\nsymbol-info = "+qual.tpe.symbol.info+"\nscope-id = "+qual.tpe.symbol.info.decls.hashCode()+"\nmembers = "+qual.tpe.members+"\nname = "+name+"\nfound = "+sym+"\nowner = "+context.enclClass.owner)
          if (!qual.tpe.widen.isErroneous) {
            error(tree.pos,
              if (name == nme.CONSTRUCTOR)
                qual.tpe.widen+" does not have a constructor"
              else
                decode(name)+" is not a member of "+qual.tpe.widen +
                (if ((context.unit ne null) && Position.line(context.unit.source, qual.pos) <
                     Position.line(context.unit.source, tree.pos))
                  "\npossible cause: maybe a semicolon is missing before `"+decode(name)+"'?"
                 else ""))
          }
          setError(tree)
        } else {
          val tree1 = tree match {
            case Select(_, _) => copy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => copy.SelectFromTypeTree(tree, qual, name)
          }
          val result = stabilize(checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe, mode, pt)
          if (sym.isCaseFactory && !phase.erasedTypes) checkStable(qual)
          result
        }
      }

      /** does given name name an identifier visible at this point?
       *
       *  @param name the given name
       *  @return     <code>true</code> if an identifier with the given name is visible.
       */
      def namesSomeIdent(name: Name): boolean = {
        var cx = context
        while (cx != NoContext) {
          val pre = cx.enclClass.prefix
          val defEntry = cx.scope.lookupEntry(name)
          if ((defEntry ne null) && defEntry.sym.exists) return true
          cx = cx.enclClass
          if ((pre.member(name) filter (
            sym => sym.exists && context.isAccessible(sym, pre, false))) != NoSymbol) return true
          cx = cx.outer
        }
        var imports = context.imports      // impSym != NoSymbol => it is imported from imports.head
        while (!imports.isEmpty) {
          if (imports.head.importedSymbol(name) != NoSymbol) return true
          imports = imports.tail
        }
        false
      }

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def typedIdent(name: Name): Tree = {
        def ambiguousError(msg: String) =
          error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg)

        var defSym: Symbol = tree.symbol // the directly found symbol
        var pre: Type = NoPrefix         // the prefix type of defSym, if a class member
        var qual: Tree = EmptyTree       // the qualififier tree if transformed tree is a select
        // if we are in a constructor of a pattern, ignore all methods
        // which are not case factories (note if we don't do that
        // case x :: xs in class List would return the :: method.
        def qualifies(sym: Symbol): boolean =
          sym.exists &&
          ((mode & PATTERNmode | FUNmode) != (PATTERNmode | FUNmode) ||
           !sym.isSourceMethod || sym.isCaseFactory)

        if (defSym == NoSymbol) {
          var defEntry: ScopeEntry = null // the scope entry of defSym, if defined in a local scope

          var cx = context
          while (defSym == NoSymbol && cx != NoContext) {
            pre = cx.enclClass.prefix
            defEntry = cx.scope.lookupEntry(name)
            if ((defEntry ne null) && qualifies(defEntry.sym)) {
              defSym = defEntry.sym
            } else {
              cx = cx.enclClass
              defSym = pre.member(name) filter (
                sym => qualifies(sym) && context.isAccessible(sym, pre, false))
              if (defSym == NoSymbol) cx = cx.outer
            }
          }

          val symDepth = if (defEntry eq null) cx.depth
                         else cx.depth - (cx.scope.nestingLevel - defEntry.owner.nestingLevel)
          var impSym: Symbol = NoSymbol;      // the imported symbol
          var imports = context.imports;      // impSym != NoSymbol => it is imported from imports.head
          while (!impSym.exists && !imports.isEmpty && imports.head.depth > symDepth) {
            impSym = imports.head.importedSymbol(name)
            if (!impSym.exists) imports = imports.tail
          }

          // detect ambiguous definition/import,
          // update `defSym' to be the final resolved symbol,
          // update `pre' to be `sym's prefix type in case it is an imported member,
          // and compute value of:

          if (defSym.exists && impSym.exists) {
            // imported symbols take precedence over package-owned symbols in different
            // compilation units. Defined symbols take precedence over errenous imports.
            if (defSym.owner.isPackageClass &&
                (!currentRun.compiles(defSym) ||
                 (context.unit ne null) && defSym.sourceFile != context.unit.source.file))
              defSym = NoSymbol
            else if (impSym.isError)
              impSym = NoSymbol
          }
          if (defSym.exists) {
            if (impSym.exists)
              ambiguousError(
                "it is both defined in "+defSym.owner +
                " and imported subsequently by \n"+imports.head)
            else if (!defSym.owner.isClass || defSym.owner.isPackageClass || defSym.isTypeParameterOrSkolem)
              pre = NoPrefix
            else
              qual = atPos(tree.pos)(gen.mkAttributedQualifier(pre))
          } else {
            if (impSym.exists) {
              var impSym1 = NoSymbol
              var imports1 = imports.tail
              def ambiguousImport() = {
                if (!(imports.head.qual.tpe =:= imports1.head.qual.tpe))
                  ambiguousError(
                    "it is imported twice in the same scope by\n"+imports.head +  "\nand "+imports1.head)
              }
              while (!imports1.isEmpty &&
                     (!imports.head.isExplicitImport(name) ||
                      imports1.head.depth == imports.head.depth)) {
                var impSym1 = imports1.head.importedSymbol(name)
                if (impSym1.exists) {
                  if (imports1.head.isExplicitImport(name)) {
                    if (imports.head.isExplicitImport(name) ||
                        imports1.head.depth != imports.head.depth) ambiguousImport()
                    impSym = impSym1
                    imports = imports1
                  } else if (!imports.head.isExplicitImport(name) &&
                             imports1.head.depth == imports.head.depth) ambiguousImport()
                }
                imports1 = imports1.tail
              }
              defSym = impSym
              qual = atPos(tree.pos)(resetPos(imports.head.qual.duplicate))
              pre = qual.tpe
            } else {
              if (settings.debug.value) {
                log(context.imports)//debug
              }
              error(tree.pos, "not found: "+decode(name))
              defSym = context.owner.newErrorSymbol(name)
            }
          }
        }
        if (defSym.owner.isPackageClass) pre = defSym.owner.thisType
        if (defSym.isThisSym) typed1(This(defSym.owner), mode, pt)
        else {
          val tree1 = if (qual == EmptyTree) tree
                      else atPos(tree.pos)(Select(qual, name))
                    // atPos necessary because qualifier might come from startContext
          stabilize(checkAccessible(tree1, defSym, pre, qual), pre, mode, pt)
        }
      }

      // begin typed1
      val sym: Symbol = tree.symbol
      if (sym ne null) sym.initialize
      //if (settings.debug.value && tree.isDef) log("typing definition of "+sym);//DEBUG
      tree match {
        case PackageDef(name, stats) =>
          val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
            .typedStats(stats, NoSymbol)
          copy.PackageDef(tree, name, stats1) setType NoType

        case tree @ ClassDef(_, _, _, _, _) =>
          newTyper(makeNewScope(context, tree, sym)).typedClassDef(tree)

        case tree @ ModuleDef(_, _, _) =>
          newTyper(context.make(tree, sym.moduleClass)).typedModuleDef(tree)

        case vdef @ ValDef(_, _, _, _) =>
          typedValDef(vdef)

        case ddef @ DefDef(_, _, _, _, _, _) =>
          newTyper(makeNewScope(context, tree, sym)).typedDefDef(ddef)

        case tdef @ AbsTypeDef(_, _, _, _) =>
          newTyper(makeNewScope(context, tree, sym)).typedAbsTypeDef(tdef)

        case tdef @ AliasTypeDef(_, _, _, _) =>
          newTyper(makeNewScope(context, tree, sym)).typedAliasTypeDef(tdef)

        case ldef @ LabelDef(_, _, _) =>
          var lsym = ldef.symbol
          var typer1 = this
          if (lsym == NoSymbol) { // labeldef is part of template
            typer1 = newTyper(makeNewScope(context, tree, context.owner))
            typer1.enterLabelDef(ldef)
          }
          typer1.typedLabelDef(ldef)

        case DocDef(comment, defn) =>
          val ret = typed(defn, mode, pt)
          if (comments ne null) comments(defn.symbol) = comment
          ret

        case Annotated(annot, arg) =>
          val arg1 = typed(arg, mode, pt)
          def annotTypeTree(ainfo: AnnotationInfo[Any]): Tree =
            TypeTree(arg1.tpe.withAttribute(ainfo)) setOriginal tree
          if (arg1.isType) {
            val annotInfo = typedAnnotation(annot, identity[Tree])
            if (settings.Xplugtypes.value) annotTypeTree(annotInfo) else arg1
          } else {
            val annotInfo = typedAnnotation(annot, getConstant)
            arg1 match {
              case _: DefTree =>
                if (!annotInfo.atp.isError) {
                  val attributed =
                    if (arg1.symbol.isModule) arg1.symbol.moduleClass else arg1.symbol
                  attributed.attributes = annotInfo :: attributed.attributes
                }
                arg1
              case _ =>
                val atpt =  annotTypeTree(annotInfo)
                Typed(arg1, atpt) setPos tree.pos setType atpt.tpe
            }
          }

        case tree @ Block(_, _) =>
          newTyper(makeNewScope(context, tree, context.owner))
            .typedBlock(tree, mode, pt)

        case Sequence(elems) =>
          checkRegPatOK(tree.pos, mode)
          val elems1 = List.mapConserve(elems)(elem => typed(elem, mode, pt))
          copy.Sequence(tree, elems1) setType pt

        case Alternative(alts) =>
          val alts1 = List.mapConserve(alts)(alt => typed(alt, mode, pt))
          copy.Alternative(tree, alts1) setType pt

        case Star(elem) =>
          checkRegPatOK(tree.pos, mode)
          val elem1 = typed(elem, mode, pt)
          copy.Star(tree, elem1) setType pt

        case Bind(name, body) =>
          var vble = tree.symbol
          if (name.isTypeName) {
            assert(body == EmptyTree)
            if (vble == NoSymbol)
              vble =
                if (isFullyDefined(pt))
                  context.owner.newAliasType(tree.pos, name) setInfo pt
                else
                  context.owner.newAbstractType(tree.pos, name) setInfo
                    mkTypeBounds(AllClass.tpe, AnyClass.tpe)
            if (vble.name == nme.WILDCARD.toTypeName) context.scope.enter(vble)
            else namer.enterInScope(vble)
            tree setSymbol vble setType vble.tpe
          } else {
            if (vble == NoSymbol)
              vble = context.owner.newValue(tree.pos, name)
            if (vble.name.toTermName != nme.WILDCARD) {
/*
            if (namesSomeIdent(vble.name))
              context.warning(tree.pos,
                "pattern variable"+vble.name+" shadows a value visible in the environment;\n"+
                "use backquotes `"+vble.name+"` if you mean to match against that value;\n" +
                "or rename the variable or use an explicit bind "+vble.name+"@_ to avoid this warning.")
*/
              namer.enterInScope(vble)
            }
            val body1 = typed(body, mode, pt)
            vble.setInfo(
              if (treeInfo.isSequenceValued(body)) seqType(body1.tpe)
              else body1.tpe)
            copy.Bind(tree, name, body1) setSymbol vble setType body1.tpe   // buraq, was: pt
          }

        case ArrayValue(elemtpt, elems) =>
          val elemtpt1 = typedType(elemtpt)
          val elems1 = List.mapConserve(elems)(elem => typed(elem, mode, elemtpt1.tpe))
          copy.ArrayValue(tree, elemtpt1, elems1)
            .setType(if (isFullyDefined(pt) && !phase.erasedTypes) pt
                     else appliedType(ArrayClass.typeConstructor, List(elemtpt1.tpe)))

        case tree @ Function(_, _) =>
          if (tree.symbol == NoSymbol)
            tree.symbol = context.owner.newValue(tree.pos, nme.ANON_FUN_NAME)
              .setFlag(SYNTHETIC).setInfo(NoType)
          newTyper(makeNewScope(context, tree, tree.symbol)).typedFunction(tree, mode, pt)

        case Assign(lhs, rhs) =>
          def mayBeVarGetter(sym: Symbol) = sym.info match {
            case PolyType(List(), _) => sym.owner.isClass && !sym.isStable
            case _: ImplicitMethodType => sym.owner.isClass && !sym.isStable
            case _ => false
          }
          val lhs1 = typed(lhs, EXPRmode | LHSmode, WildcardType)
          val varsym = lhs1.symbol
          if ((varsym ne null) && mayBeVarGetter(varsym))
            lhs1 match {
              case Select(qual, name) =>
                return typed(
                  Apply(
                    Select(qual, nme.getterToSetter(name)) setPos lhs.pos,
                    List(rhs)) setPos tree.pos,
                  mode, pt)

              case _ =>

            }
          if ((varsym ne null) && (varsym.isVariable || varsym.isValue && phase.erasedTypes)) {
            val rhs1 = typed(rhs, lhs1.tpe)
            copy.Assign(tree, lhs1, checkDead(rhs1)) setType UnitClass.tpe
          } else {
            if (!lhs1.tpe.isError) error(tree.pos, "assignment to non-variable ")
            setError(tree)
          }

        case If(cond, thenp, elsep) =>
          val cond1 = checkDead(typed(cond, BooleanClass.tpe))
          if (elsep.isEmpty) {
            val thenp1 = typed(thenp, UnitClass.tpe)
            copy.If(tree, cond1, thenp1, elsep) setType UnitClass.tpe
          } else {
            val thenp1 = typed(thenp, pt)
            val elsep1 = typed(elsep, pt)
            copy.If(tree, cond1, thenp1, elsep1) setType ptOrLub(List(thenp1.tpe, elsep1.tpe))
          }

        case Match(selector, cases) =>
          val selector1 = checkDead(typed(selector))
          val cases1 = typedCases(tree, cases, selector1.tpe.widen, pt)
          copy.Match(tree, selector1, cases1) setType ptOrLub(cases1 map (.tpe))

        case Return(expr) =>
          val enclMethod = context.enclMethod
          if (enclMethod == NoContext || enclMethod.owner.isConstructor) {
            errorTree(tree, "return outside method definition")
          } else {
            val DefDef(_, _, _, _, restpt, _) = enclMethod.tree
            if (restpt.tpe eq null) {
              errorTree(tree, "method " + enclMethod.owner +
                        " has return statement; needs result type")
            } else {
              context.enclMethod.returnsSeen = true
              val expr1: Tree = typed(expr, restpt.tpe)
              copy.Return(tree, checkDead(expr1)) setSymbol enclMethod.owner setType AllClass.tpe
            }
          }

        case Try(block, catches, finalizer) =>
          val block1 = typed(block, pt)
          val catches1 = typedCases(tree, catches, ThrowableClass.tpe, pt)
          val finalizer1 = if (finalizer.isEmpty) finalizer
                           else typed(finalizer, UnitClass.tpe)
          copy.Try(tree, block1, catches1, finalizer1)
            .setType(ptOrLub(block1.tpe :: (catches1 map (.tpe))))

        case Throw(expr) =>
          val expr1 = typed(expr, ThrowableClass.tpe)
          copy.Throw(tree, expr1) setType AllClass.tpe

        case New(tpt: Tree) =>
          var tpt1 = typedTypeConstructor(tpt)
          if (tpt1.hasSymbol && !tpt1.symbol.typeParams.isEmpty) {
            context.undetparams = cloneSymbols(tpt1.symbol.typeParams)
            tpt1 = TypeTree()
              .setOriginal(tpt1) /* .setPos(tpt1.pos) */
              .setType(appliedType(tpt1.tpe, context.undetparams map (.tpe)))
          }
          if (tpt1.tpe.symbol hasFlag ABSTRACT)
            error(tree.pos, tpt1.tpe.symbol +
                            " is abstract; cannot be instantiated")
          else if (tpt1.tpe.symbol.thisSym != tpt1.tpe.symbol &&
                   !(tpt1.tpe <:< tpt1.tpe.typeOfThis) &&
                   !phase.erasedTypes)
            error(tree.pos, tpt1.tpe.symbol +
                  " cannot be instantiated because it does not conform to its self-type "+
                  tpt1.tpe.typeOfThis)
          copy.New(tree, tpt1).setType(tpt1.tpe)

        case Typed(expr, Function(List(), EmptyTree)) =>
          val expr1 = checkDead(typed1(expr, mode, pt))
          expr1.tpe match {
            case TypeRef(_, sym, _) if (sym == ByNameParamClass) =>
              val expr2 = Function(List(), expr1)
              new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2)
              typed1(expr2, mode, pt)
            case PolyType(List(), restpe) =>
              val expr2 = Function(List(), expr1)
              new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2)
              typed1(expr2, mode, pt)
            case PolyType(_, MethodType(formals, _)) =>
              if (isFunctionType(pt)) expr1
              else adapt(expr1, mode, functionType(formals map (t => WildcardType), WildcardType))
            case MethodType(formals, _) =>
              if (isFunctionType(pt)) expr1
              else expr1 match {
                case Select(qual, name) if (forMSIL &&
                                            pt != WildcardType &&
                                            pt != ErrorType &&
                                            isSubType(pt, definitions.DelegateClass.tpe)) =>
                  val scalaCaller = newScalaCaller(pt);
                  addScalaCallerInfo(scalaCaller, expr1.symbol)
                  val n: Name = scalaCaller.name
                  val del = Ident(DelegateClass) setType DelegateClass.tpe
                  val f = Select(del, n)
                  //val f1 = TypeApply(f, List(Ident(pt.symbol) setType pt))
                  val args: List[Tree] = if(expr1.symbol.isStatic) List(Literal(Constant(null)))
                                         else List(qual) // where the scala-method is located
                  val rhs = Apply(f, args);
                  typed(rhs)
                case _ =>
                  adapt(expr1, mode, functionType(formals map (t => WildcardType), WildcardType))
              }
            case ErrorType =>
              expr1
            case _ =>
              errorTree(expr1, "`&' must be applied to method; cannot be applied to " + expr1.tpe)
          }

        case Typed(expr, tpt @ Ident(name)) if (name == nme.WILDCARD_STAR.toTypeName) =>
          //todo: do this: errorTree(tree, "`: _*' annotation only legal for method arguments")
          val expr1 = typed(expr, mode & stickyModes, seqType(pt))
          expr1.tpe.baseType(SeqClass) match {
            case TypeRef(_, _, List(elemtp)) =>
              copy.Typed(tree, expr1, tpt setType elemtp) setType elemtp
            case _ =>
              setError(tree)
          }

        case Typed(expr, tpt) =>
          val tpt1 = typedType(tpt)
          val expr1 = typed(expr, mode & stickyModes, tpt1.tpe.deconst)
          val owntype =
            if ((mode & PATTERNmode) != 0) inferTypedPattern(tpt1.pos, tpt1.tpe, widen(pt))
            else tpt1.tpe
          //Console.println(typed pattern: "+tree+":"+", tp = "+tpt1.tpe+", pt = "+pt+" ==> "+owntype)//DEBUG
          copy.Typed(tree, expr1, tpt1) setType owntype

        case TypeApply(fun, args) =>
          val args1 = List.mapConserve(args)(typedType)
          // do args first in order to maintain conext.undetparams on the function side.
          typedTypeApply(typed(fun, funMode(mode) | TAPPmode, WildcardType), args1)

        case Apply(Block(stats, expr), args) =>
          typed1(atPos(tree.pos)(Block(stats, Apply(expr, args))), mode, pt)

        case Apply(fun, args) =>
          val stableApplication = (fun.symbol ne null) && fun.symbol.isMethod && fun.symbol.isStable
          if (stableApplication && (mode & PATTERNmode) != 0) {
            // treat stable function applications f() as expressions.
            typed1(tree, mode & ~PATTERNmode | EXPRmode, pt)
          } else {
            val funpt = if ((mode & PATTERNmode) != 0) pt else WildcardType
            silent(.typed(fun, funMode(mode), funpt)) match {
              case fun1: Tree =>
                val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1
                if (util.Statistics.enabled) appcnt = appcnt + 1
                if (phase.id <= currentRun.typerPhase.id &&
                    fun2.isInstanceOf[Select] &&
                    !fun2.tpe.isInstanceOf[ImplicitMethodType] &&
                    ((fun2.symbol eq null) || !fun2.symbol.isConstructor) &&
                    (mode & (EXPRmode | SNDTRYmode)) == EXPRmode) {
                  tryTypedApply(fun2, args)
                } else {
                  typedApply(tree, fun2, args, mode, pt)
                }
              case ex: TypeError =>
                fun match {
                  case Select(qual, name) =>
                    val qual1 = typedQualifier(qual)
                    if ((mode & PATTERNmode) == 0 &&
                        nme.isOpAssignmentName(name) &&
                        treeInfo.isVariableOrGetter(qual1)) {
                      convertToAssignment(fun, qual1, name, args, ex)
                    } else {
                      reportTypeError(fun.pos, ex)
                      setError(tree)
                    }
                  case _ =>
                    reportTypeError(fun.pos, ex)
                    setError(tree)
                }
            }
          }

        case ApplyDynamic(qual, args) =>
          val qual1 = typed(qual, AnyRefClass.tpe)
          val args1 = List.mapConserve(args)(arg => typed(arg, AnyRefClass.tpe))
          copy.ApplyDynamic(tree, qual1, args1) setType AnyRefClass.tpe

        case Super(qual, mix) =>
          val (clazz, selftype) =
            if (tree.symbol != NoSymbol) {
              (tree.symbol, tree.symbol.thisType)
            } else {
              val clazzContext = qualifyingClassContext(tree, qual)
              (clazzContext.owner, clazzContext.prefix)
            }
          if (clazz == NoSymbol) setError(tree)
          else {
            val owntype =
              if (mix.isEmpty)
                if ((mode & SUPERCONSTRmode) != 0) clazz.info.parents.head
                else intersectionType(clazz.info.parents)
              else {
                val ps = clazz.info.parents filter (p => p.symbol.name == mix)
                if (ps.isEmpty) {
                  if (settings.debug.value)
                    Console.println(clazz.info.parents map (.symbol.name))//debug
                  error(tree.pos, mix+" does not name a parent class of "+clazz)
                  ErrorType
                } else if (ps.tail.isEmpty) {
                  ps.head
                } else {
                  error(tree.pos, "ambiguous parent class qualifier")
                  ErrorType
                }
              }
            tree setSymbol clazz setType mkSuperType(selftype, owntype)
          }

        case This(qual) =>
          val (clazz, selftype) =
            if (tree.symbol != NoSymbol) {
              (tree.symbol, tree.symbol.thisType)
            } else {
              val clazzContext = qualifyingClassContext(tree, qual)
              (clazzContext.owner, clazzContext.prefix)
            }
          if (clazz == NoSymbol) setError(tree)
          else {
            val owntype = if (pt.isStable || (mode & QUALmode) != 0) selftype
                          else selftype.singleDeref
            tree setSymbol clazz setType owntype
          }

        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          val qual1 = typed(qual, EXPRmode | QUALmode | POLYmode | SUPERCONSTRmode, WildcardType)
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(qual1, nme.CONSTRUCTOR)

        case Select(qual, name) =>
          if (util.Statistics.enabled) selcnt = selcnt + 1
          var qual1 = checkDead(typedQualifier(qual))
          if (name.isTypeName) qual1 = checkStable(qual1)
          val tree1 = typedSelect(qual1, name)
          if (qual1.symbol == RootPackage) copy.Ident(tree1, name)
          else tree1

        case Ident(name) =>
          idcnt = idcnt + 1
          if ((name == nme.WILDCARD && (mode & (PATTERNmode | FUNmode)) == PATTERNmode) ||
              (name == nme.WILDCARD.toTypeName && (mode & TYPEmode) != 0))
            tree setType pt
          else
            typedIdent(name)

        case Literal(value) =>
          tree setType (
            if (value.tag == UnitTag) UnitClass.tpe
            else mkConstantType(value))

        case SingletonTypeTree(ref) =>
          val ref1 = checkStable(typed(ref, EXPRmode | QUALmode, AnyRefClass.tpe))
          tree setType ref1.tpe.resultType

        case SelectFromTypeTree(qual, selector) =>
          val sel = typedSelect(typedType(qual), selector)
          tree setSymbol sel.symbol setType typedSelect(typedType(qual), selector).tpe

        case tree @ CompoundTypeTree(templ: Template) =>
          (tree setType {
            val parents1 = List.mapConserve(templ.parents)(typedType)
            if (parents1 exists (.tpe.isError)) ErrorType
            else {
              val decls = newDecls(tree)
              val self = refinedType(parents1 map (.tpe), context.enclClass.owner, decls)
              newTyper(context.make(templ, self.symbol, decls)).typedRefinement(templ.body)
              self
            }
          }) : CompoundTypeTree

        case AppliedTypeTree(tpt, args) =>
          val tpt1 = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)
          val tparams = tpt1.symbol.typeParams
          val args1 = List.mapConserve(args)(typedType)
          if (tpt1.tpe.isError) {
            setError(tree)
          } else if (tparams.length == args1.length) {
            val argtypes = args1 map (.tpe)
            val owntype = if (tpt1.symbol.isClass) appliedType(tpt1.tpe, argtypes)
                          else tpt1.tpe.subst(tparams, argtypes)
            List.map2(args, tparams) { (arg, tparam) => arg match {
              // note: can't use args1 in selector, because Bind's got replaced
              case Bind(_, _) =>
                if (arg.symbol.isAbstractType)
                  arg.symbol setInfo
                    TypeBounds(lub(List(arg.symbol.info.bounds.lo, tparam.info.bounds.lo)),
                               glb(List(arg.symbol.info.bounds.hi, tparam.info.bounds.hi)))
              case _ =>
            }}
            TypeTree(owntype) setOriginal(tree) // setPos tree.pos
          } else if (tparams.length == 0) {
            errorTree(tree, tpt1.tpe+" does not take type parameters")
          } else {
            //Console.println("\{tpt1}:\{tpt1.symbol}:\{tpt1.symbol.info}")
            if (settings.debug.value) Console.println(tpt1+":"+tpt1.symbol+":"+tpt1.symbol.info);//debug
            errorTree(tree, "wrong number of type arguments for "+tpt1.tpe+", should be "+tparams.length)
          }

        case WildcardTypeTree(lo, hi) =>
          tree setType mkTypeBounds(typedType(lo).tpe, typedType(hi).tpe)

        case TypeTree() =>
          // we should get here only when something before failed
          // and we try again (@see tryTypedApply). In that case we can assign
          // whatever type to tree; we just have to survive until a real error message is issued.
          tree setType AnyClass.tpe

        case _ =>
          throw new Error("unexpected tree: " + tree)//debug
      }
    }

    /**
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     ...
     */
    def typed(tree: Tree, mode: int, pt: Type): Tree =
      try {
        if (settings.debug.value)
          assert(pt ne null, tree)//debug
        if (context.retyping &&
            (tree.tpe ne null) && (tree.tpe.isErroneous || !(tree.tpe <:< pt))) {
          tree.tpe = null
          if (tree.hasSymbol) tree.symbol = NoSymbol
        }
        //Console.println("typing "+tree+", "+context.undetparams);//DEBUG
        val tree1 = if (tree.tpe ne null) tree else typed1(tree, mode, pt)
        //Console.println("typed "+tree1+":"+tree1.tpe+", "+context.undetparams);//DEBUG

        val result = if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt)
        //Console.println("adapted "+tree1+":"+tree1.tpe+" to "+pt+", "+context.undetparams);//DEBUG
        result
      } catch {
        case ex: TypeError =>
          tree.tpe = null
          //Console.println("caught "+ex+" in typed");//DEBUG
          reportTypeError(tree.pos, ex)
          setError(tree)
        case ex: Throwable =>
          if (settings.debug.value)
            Console.println("exception when typing "+tree+", pt = "+pt)
          if ((context ne null) && (context.unit ne null) &&
              (context.unit.source ne null) && (tree ne null))
            logError("AT: " + context.unit.source.dbg(tree.pos), ex);
          throw(ex)
      }

    def atOwner(owner: Symbol): Typer =
      newTyper(context.make(context.tree, owner))

    def atOwner(tree: Tree, owner: Symbol): Typer =
      newTyper(context.make(tree, owner))

    /** Types expression or definition <code>tree</code>.
     *
     *  @param tree ...
     *  @return     ...
     */
    def typed(tree: Tree): Tree = {
      val ret = typed(tree, EXPRmode, WildcardType)
      ret
    }

    /** Types expression <code>tree</code> with given prototype <code>pt</code>.
     *
     *  @param tree ...
     *  @param pt   ...
     *  @return     ...
     */
    def typed(tree: Tree, pt: Type): Tree =
      typed(tree, EXPRmode, pt)

    /** Types qualifier <code>tree</code> of a select node.
     *  E.g. is tree occurs in a context like <code>tree.m</code>.
     *
     *  @param tree ...
     *  @return     ...
     */
    def typedQualifier(tree: Tree): Tree =
      typed(tree, EXPRmode | QUALmode | POLYmode, WildcardType)

    /** Types function part of an application */
    def typedOperator(tree: Tree): Tree =
      typed(tree, EXPRmode | FUNmode | POLYmode | TAPPmode, WildcardType)

    /** Types a pattern with prototype <code>pt</code> */
    def typedPattern(tree: Tree, pt: Type): Tree =
      typed(tree, PATTERNmode, pt)

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree): Tree =
      withNoGlobalVariance{ typed(tree, TYPEmode, WildcardType) }

    /** Types a type constructor tree used in a new or supertype */
    def typedTypeConstructor(tree: Tree): Tree = {
      val result = withNoGlobalVariance{ typed(tree, TYPEmode | FUNmode, WildcardType) }
      if (!phase.erasedTypes && result.tpe.isInstanceOf[TypeRef] && !result.tpe.prefix.isStable)
        error(tree.pos, result.tpe.prefix+" is not a legal prefix for a constructor")
      result
    }

    def computeType(tree: Tree, pt: Type): Type = {
      val tree1 = typed(tree, pt)
      transformed(tree) = tree1
      tree1.tpe
    }

    def transformedOrTyped(tree: Tree, pt: Type): Tree = transformed.get(tree) match {
      case Some(tree1) => transformed -= tree; tree1
      case None => typed(tree, pt)
    }

/*
    def convertToTypeTree(tree: Tree): Tree = tree match {
      case TypeTree() => tree
      case _ => TypeTree(tree.tpe)
    }
*/
    /* -- Views --------------------------------------------------------------- */

    private def tparamsToWildcards(tp: Type, tparams: List[Symbol]) =
      tp.subst(tparams, tparams map (t => WildcardType))

    private def depoly(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) => tparamsToWildcards(restpe, tparams)
      case _ => tp
    }

    private def containsError(tp: Type): boolean = tp match {
      case PolyType(tparams, restpe) => containsError(restpe)
      case MethodType(formals, restpe) => (formals exists (.isError)) || containsError(restpe)
      case _ => tp.isError
    }

    /** Try to construct a typed tree from given implicit info with given
     *  expected type.
     *
     *  @param pos     Position for error reporting
     *  @param info    The given implicit info describing the implicit definition
     *  @param pt      The expected type
     *  @param isLocal Is implicit definition visible without prefix?
     *  @return        A typed tree if the implicit info can be made to conform
     *                 to <code>pt</code>, EmptyTree otherwise.
     *  @pre           <code>info.tpe</code> does not contain an error
     */
    private def typedImplicit(pos: PositionType, info: ImplicitInfo, pt: Type, isLocal: boolean): Tree = {
      def isStable(tp: Type): boolean = tp match {
        case TypeRef(pre, sym, _) => sym.isPackageClass || sym.isModuleClass && isStable(pre)
        case _ => tp.isStable
      }
      if (isCompatible(depoly(info.tpe), pt) && isStable(info.pre)) {
        val tree = atPos(pos) {
          if (info.pre == NoPrefix/*isLocal*/) Ident(info.name)
          else Select(gen.mkAttributedQualifier(info.pre), info.name)
        }
        def fail(reason: String, sym1: Symbol, sym2: Symbol): Tree = {
          if (settings.debug.value)
            log(tree+" is not a valid implicit value because:\n"+reason + sym1+" "+sym2)
          EmptyTree
        }
        try {
//        if (!isLocal) tree setSymbol info.sym
          val tree1 = typed1(tree, EXPRmode, pt)
          //if (settings.debug.value) log("typed implicit "+tree1+":"+tree1.tpe+", pt = "+pt)
          val tree2 = adapt(tree1, EXPRmode, pt)
          //if (settings.debug.value) log("adapted implicit "+tree1.symbol+":"+tree2.tpe+" to "+pt)
          if (tree2.tpe.isError) EmptyTree
          else if (info.sym == tree1.symbol) tree2
          else fail("syms differ: ", tree1.symbol, info.sym)
        } catch {
          case ex: TypeError => fail(ex.getMessage(), NoSymbol, NoSymbol)
        }
      } else EmptyTree
    }

    /** Infer implicit argument or view.
     *
     *  @param  pos             position for error reporting
     *  @param  pt              the expected type of the implicit
     *  @param  isView          are we searching for a view? (this affects the error message)
     *  @param  reportAmbiguous should ambiguous errors be reported?
     *                          False iff we search for a view to find out
     *                          whether one type is coercible to another
     *  @return                 ...
     *  @see                    <code>isCoercible</code>
     */
    private def inferImplicit(pos: PositionType, pt: Type, isView: boolean, reportAmbiguous: boolean): Tree = {

      if (util.Statistics.enabled) implcnt = implcnt + 1
      val startTime = if (util.Statistics.enabled) currentTime else 0l

      val tc = newTyper(context.makeImplicit(reportAmbiguous))

      def ambiguousImplicitError(info1: ImplicitInfo, info2: ImplicitInfo,
                                 pre1: String, pre2: String, trailer: String) = {
        val coreMsg =
          pre1+" "+info1.sym+info1.sym.locationString+" of type "+info1.tpe+"\n "+
          pre2+" "+info2.sym+info2.sym.locationString+" of type "+info2.tpe+"\n "+
          trailer
        error(pos,
          if (isView) {
            val found = pt.typeArgs(0)
            val req = pt.typeArgs(1)
            typeErrorMsg(found, req)+
            "\nNote that implicit conversions are not applicable because they are ambiguous:\n "+
            coreMsg+"are possible conversion functions from "+ found+" to "+req
          } else {
            "ambiguous implicit values:\n "+coreMsg + "match expected type "+pt
          })
      }

      /** Search list of implicit info lists for one matching prototype
       *  <code>pt</code>. If found return a tree from found implicit info
       *  which is typed with expected type <code>pt</code>.
       *  Otherwise return EmptyTree
       *
       *  @param implicitInfoss The given list of lists of implicit infos
       *  @param isLocal        Is implicit definition visible without prefix?
       *                        If this is the case then symbols in preceding lists shadow
       *                        symbols of the same name in succeeding lists.
       *  @return               ...
       */
      def searchImplicit(implicitInfoss: List[List[ImplicitInfo]], isLocal: boolean): Tree = {
        def isSubClassOrObject(sym1: Symbol, sym2: Symbol): boolean =
          sym1 != NoSymbol && (sym1 isSubClass sym2) ||
          sym1.isModuleClass && isSubClassOrObject(sym1.linkedClassOfClass, sym2) ||
          sym2.isModuleClass && isSubClassOrObject(sym1, sym2.linkedClassOfClass)
        def improves(info1: ImplicitInfo, info2: ImplicitInfo) =
          (info2 == NoImplicitInfo) ||
          (info1 != NoImplicitInfo) &&
          isStrictlyBetter(info1.tpe, info2.tpe)
        val shadowed = new HashSet[Name](8)
        def isApplicable(info: ImplicitInfo): boolean =
          !containsError(info.tpe) &&
          !(isLocal && shadowed.contains(info.name)) &&
          (!isView || info.sym != Predef_identity) &&
          tc.typedImplicit(pos, info, pt, isLocal) != EmptyTree
        def applicableInfos(is: List[ImplicitInfo]) = {
          val result = is filter isApplicable
          if (isLocal)
            for (val i <- is) shadowed addEntry i.name
          result
        }
        val applicable = List.flatten(implicitInfoss map applicableInfos)
        val best = (NoImplicitInfo /: applicable) ((best, alt) => if (improves(alt, best)) alt else best)
        if (best == NoImplicitInfo) EmptyTree
        else {
          val competing = applicable dropWhile (alt => best == alt || improves(best, alt))
          if (!competing.isEmpty) ambiguousImplicitError(best, competing.head, "both", "and", "")
          for (val alt <- applicable)
            if (alt.sym.owner != best.sym.owner && isSubClassOrObject(alt.sym.owner, best.sym.owner)) {
              ambiguousImplicitError(best, alt,
                             "most specific definition is:",
                             "yet alternative definition  ",
                             "is defined in a subclass.\n Both definitions ")
            }
          tc.typedImplicit(pos, best, pt, isLocal)
        }
      }

      def implicitsOfType(tp: Type): List[List[ImplicitInfo]] = {
        def getParts(tp: Type, s: Set[Type]): unit = tp match {
          case TypeRef(pre, sym, args) if (!sym.isPackageClass) =>
            for (val bc <- sym.info.baseClasses)
              if (sym.isClass) s.addEntry(tp.baseType(bc))
            getParts(pre, s)
            for (val arg <- args) getParts(arg, s)
          case ThisType(_) =>
            getParts(tp.widen, s)
          case _: SingletonType =>
            getParts(tp.widen, s)
          case RefinedType(ps, _) =>
            for (val p <- ps) getParts(p, s)
          case _ =>
        }
        val tps = new HashSet[Type]
        getParts(tp, tps)
        tps.elements.map(implicitsOfClass).toList
      }

      def implicitsOfClass(tp: Type): List[ImplicitInfo] = tp match {
        case TypeRef(pre, clazz, _) =>
          clazz.initialize.linkedClassOfClass.info.members.toList.filter(.hasFlag(IMPLICIT)) map
            (sym => new ImplicitInfo(sym.name, pre.memberType(clazz.linkedModuleOfClass), sym))
        case _ =>
          List()
      }

      var tree = searchImplicit(context.implicitss, true)
      if (tree == EmptyTree) tree = searchImplicit(implicitsOfType(pt), false)
      if (util.Statistics.enabled)
        impltime = impltime + currentTime - startTime
      tree
    }

    def applyImplicitArgs(tree: Tree): Tree = tree.tpe match {
      case MethodType(formals, _) =>
        def implicitArg(pt: Type) = {
          val arg = inferImplicit(tree.pos, pt, false, true)
          if (arg != EmptyTree) arg
          else errorTree(tree, "no implicit argument matching parameter type "+pt+" was found.")
        }
        Apply(tree, formals map implicitArg) setPos tree.pos
      case ErrorType =>
        tree
    }
  }
}
