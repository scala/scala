/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly
package scala.tools.nsc
package typechecker

import scala.collection.mutable.{LinkedHashMap, ListBuffer}
import scala.tools.nsc.util.{HashSet, Set, SourceFile}
import symtab.Flags._
import util.Statistics._

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Implicits {
self: Analyzer =>

  import global._
  import definitions._

  def traceImplicits = printTypings
  import global.typer.{printTyping, deindentTyping, indentTyping}

  /** Search for an implicit value. See the comment on `result` at the end of class `ImplicitSearch`
   *  for more info how the search is conducted.
   *  @param tree             The tree for which the implicit needs to be inserted.
   *                          (the inference might instantiate some of the undetermined
   *                          type parameters of that tree.
   *  @param pt               The expected type of the implicit.
   *  @param reportAmbiguous  Should ambiguous implicit errors be reported?
   *                          False iff we search for a view to find out
   *                          whether one type is coercible to another.
   *  @param isView           We are looking for a view
   *  @param context          The current context
   *  @return                 A search result
   */
  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context): SearchResult = {
    printTyping("Beginning implicit search for "+ tree +" expecting "+ pt + (if(isView) " looking for a view" else ""))
    indentTyping()
    val rawTypeStart = startCounter(rawTypeImpl)
    val findMemberStart = startCounter(findMemberImpl)
    val subtypeStart = startCounter(subtypeImpl)
    val start = startTimer(implicitNanos)
    if (traceImplicits && !tree.isEmpty && !context.undetparams.isEmpty)
      println("typing implicit with undetermined type params: "+context.undetparams+"\n"+tree)
    val result = new ImplicitSearch(tree, pt, isView, context.makeImplicit(reportAmbiguous)).bestImplicit
    context.undetparams = context.undetparams filterNot (result.subst.from contains _)
    stopTimer(implicitNanos, start)
    stopCounter(rawTypeImpl, rawTypeStart)
    stopCounter(findMemberImpl, findMemberStart)
    stopCounter(subtypeImpl, subtypeStart)
    deindentTyping()
    printTyping("Implicit search yielded: "+ result)
    result
  }

  final val sizeLimit = 50000
  val implicitsCache = new LinkedHashMap[Type, List[List[ImplicitInfo]]]

  def resetImplicits() { implicitsCache.clear() }

  /** If type `pt` an instance of Manifest or OptManifest, or an abstract type lower-bounded
   *  by such an instance?
   */
  def isManifest(pt: Type): Boolean = pt.dealias match {
    case TypeRef(_, PartialManifestClass, List(_)) |
         TypeRef(_, FullManifestClass, List(_)) |
         TypeRef(_, OptManifestClass, List(_)) => true
    case TypeRef(_, tsym, _) => tsym.isAbstractType && isManifest(pt.bounds.lo)
    case _ => false
  }

  /** The result of an implicit search
   *  @param  tree    The tree representing the implicit
   *  @param  subst   A substituter that represents the undetermined type parameters
   *                  that were instantiated by the winning implicit.
   */
  class SearchResult(val tree: Tree, val subst: TreeTypeSubstituter) {
    override def toString = "SearchResult("+tree+", "+subst+")"
  }

  lazy val SearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter)

  /** A class that records an available implicit
   *  @param   name   The name of the implicit
   *  @param   pre    The prefix type of the implicit
   *  @param   sym    The symbol of the implicit
   */
  class ImplicitInfo(val name: Name, val pre: Type, val sym: Symbol) {
    private var tpeCache: Type = null

    /** Computes member type of implicit from prefix `pre` (cached). */
    def tpe: Type = {
      if (tpeCache eq null) tpeCache = pre.memberType(sym)
      tpeCache
    }

    /** Does type `tp` contain an Error type as parameter or result?
     */
    private def containsError(tp: Type): Boolean = tp match {
      case PolyType(tparams, restpe) =>
        containsError(restpe)
      case MethodType(params, restpe) =>
        for (p <- params)
          if (p.tpe.isError) return true
        containsError(restpe)
      case _ =>
        tp.isError
    }

    def isCyclicOrErroneous = try {
      containsError(tpe)
    } catch {
      case ex: CyclicReference =>
        true
    }

    override def equals(other: Any) = other match {
      case that: ImplicitInfo =>
          this.name == that.name &&
          this.pre =:= that.pre &&
          this.sym == that.sym
      case _ => false
    }

    override def hashCode = name.## + pre.## + sym.##

    override def toString = "ImplicitInfo(" + name + "," + pre + "," + sym + ")"
  }

  /** A sentinel indicating no implicit was found */
  val NoImplicitInfo = new ImplicitInfo(null, NoType, NoSymbol) {
    // equals used to be implemented in ImplicitInfo with an `if(this eq NoImplicitInfo)`
    // overriding the equals here seems cleaner and benchmarks show no difference in performance
    override def equals(other: Any) = other match { case that: AnyRef => that eq this  case _ => false }
    override def hashCode = 1
  }

  /** A constructor for types ?{ name: tp }, used in infer view to member
   *  searches.
   */
  def memberWildcardType(name: Name, tp: Type) = {
    val result = refinedType(List(WildcardType), NoSymbol)
    var psym = if (name.isTypeName) result.typeSymbol.newAbstractType(NoPosition, name)
               else result.typeSymbol.newValue(NoPosition, name)
    psym setInfo tp
    result.decls enter psym
    result
  }

  /** An extractor for types of the form ? { name: ? }
   */
  object HasMember {
    def apply(name: Name): Type = memberWildcardType(name, WildcardType)
    def unapply(pt: Type): Option[Name] = pt match {
      case RefinedType(List(WildcardType), decls) =>
        decls.toList match {
          case List(sym) if (sym.tpe == WildcardType) => Some(sym.name)
          case _ => None
        }
      case _ =>
        None
    }
  }

  /** An extractor for types of the form ? { name: (? >: argtpe <: Any*)restp }
   */
  object HasMethodMatching {
    def apply(name: Name, argtpes: List[Type], restpe: Type): Type = {
      def templateArgType(argtpe: Type) =
        new BoundedWildcardType(TypeBounds(argtpe, AnyClass.tpe))
      val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "typer$dummy")
      val mtpe = MethodType(dummyMethod.newSyntheticValueParams(argtpes map templateArgType), restpe)
      memberWildcardType(name, mtpe)
    }
    def unapply(pt: Type): Option[(Name, List[Type], Type)] = pt match {
      case RefinedType(List(WildcardType), decls) =>
        decls.toList match {
          case List(sym) =>
            sym.tpe match {
              case MethodType(params, restpe)
              if (params forall (_.tpe.isInstanceOf[BoundedWildcardType])) =>
                Some((sym.name, params map (_.tpe.bounds.lo), restpe))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** An extractor for unary function types arg => res
   */
  object Function1 {
    def unapply(tp: Type) = tp match {
      case TypeRef(_, sym, List(arg, res)) if (sym == FunctionClass(1)) => Some(arg, res)
      case _ => None
    }
  }

  /** A class that sets up an implicit search. For more info, see comments for `inferImplicit`.
   *  @param tree             The tree for which the implicit needs to be inserted.
   *  @param pt               The original expected type of the implicit.
   *  @param isView           We are looking for a view
   *  @param context0         The context used for the implicit search
   */
  class ImplicitSearch(tree: Tree, pt: Type, isView: Boolean, context0: Context)
    extends Typer(context0) {
    printTyping("begin implicit search: "+(tree, pt, isView, context.outer.undetparams))
//    assert(tree.isEmpty || tree.pos.isDefined, tree)

    import infer._
    /** Is implicit info `info1` better than implicit info `info2`?
     */
    def improves(info1: ImplicitInfo, info2: ImplicitInfo) = {
      incCounter(improvesCount)
      (info2 == NoImplicitInfo) ||
      (info1 != NoImplicitInfo) &&
      isStrictlyMoreSpecific(info1.tpe, info2.tpe, info1.sym, info2.sym)
    }

    /** Map all type params in given list to WildcardType
     *  @param   tp  The type in which to do the mapping
     *  @param   tparams  The list of type parameters to map
     */
    private def tparamsToWildcards(tp: Type, tparams: List[Symbol]) =
      tp.instantiateTypeParams(tparams, tparams map (t => WildcardType))

    /* Map a polytype to one in which all type parameters and argument-dependent types are replaced by wildcards.
     * Consider `implicit def b(implicit x: A): x.T = error("")`. We need to approximate DebruijnIndex types
     * when checking whether `b` is a valid implicit, as we haven't even searched a value for the implicit arg `x`,
     * so we have to approximate (otherwise it is excluded a priori).
     */
    private def depoly(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) => tparamsToWildcards(ApproximateDependentMap(restpe), tparams)
      case _ => ApproximateDependentMap(tp)
    }

    /** Does type `dtor` dominate type `dted`?
     *  This is the case if the stripped cores `dtor1` and `dted1` of both types are
     *  the same wrt `=:=`, or if they overlap and the complexity of `dtor1` is higher
     *  than the complexity of `dted1`.
     *  The _stripped core_ of a type is the type where
     *   - all refinements and annotations are dropped,
     *   - all universal and existential quantification is eliminated
     *     by replacing variables by their upper bounds,
     *   - all remaining free type parameters in the type are replaced by WildcardType.
     *  The _complexity_ of a stripped core type corresponds roughly to the number of
     *  nodes in its ast, except that singleton types are widened before taking the complexity.
     *  Two types overlap if they have the same type symbol, or
     *  if one or both are intersection types with a pair of overlapiing parent types.
     */
    private def dominates(dtor: Type, dted: Type): Boolean = {
      def core(tp: Type): Type = tp.normalize match {
        case RefinedType(parents, defs) => intersectionType(parents map core, tp.typeSymbol.owner)
        case AnnotatedType(annots, tp, selfsym) => core(tp)
        case ExistentialType(tparams, result) => core(result).subst(tparams, tparams map (t => core(t.info.bounds.hi)))
        case PolyType(tparams, result) => core(result).subst(tparams, tparams map (t => core(t.info.bounds.hi)))
        case _ => tp
      }
      def stripped(tp: Type): Type = {
        val tparams = freeTypeParametersNoSkolems.collect(tp)
        tp.subst(tparams, tparams map (t => WildcardType))
      }
      def sum(xs: List[Int]) = (0 /: xs)(_ + _)
      def complexity(tp: Type): Int = tp.normalize match {
        case NoPrefix =>
          0
        case SingleType(pre, sym) =>
          if (sym.isPackage) 0 else complexity(tp.widen)
        case TypeRef(pre, sym, args) =>
          complexity(pre) + sum(args map complexity) + 1
        case RefinedType(parents, _) =>
          sum(parents map complexity) + 1
        case _ =>
          1
      }
      def overlaps(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
        case (RefinedType(parents, _), _) => parents exists (overlaps(_, tp2))
        case (_, RefinedType(parents, _)) => parents exists (overlaps(tp1, _))
        case _ => tp1.typeSymbol == tp2.typeSymbol
      }
      val dtor1 = stripped(core(dtor))
      val dted1 = stripped(core(dted))
      overlaps(dtor1, dted1) && (dtor1 =:= dted1 || complexity(dtor1) > complexity(dted1))
    }

    incCounter(implicitSearchCount)

    /** Issues an error signalling ambiguous implicits */
    private def ambiguousImplicitError(info1: ImplicitInfo, info2: ImplicitInfo,
                               pre1: String, pre2: String, trailer: String) =
      if (!info1.tpe.isErroneous && !info2.tpe.isErroneous) {
        val coreMsg =
          pre1+" "+info1.sym+info1.sym.locationString+" of type "+info1.tpe+"\n "+
          pre2+" "+info2.sym+info2.sym.locationString+" of type "+info2.tpe+"\n "+
          trailer
        error(tree.pos,
          if (isView) {
            val found = pt.typeArgs(0)
            val req = pt.typeArgs(1)

            /** A nice spot to explain some common situations a little
             *  less confusingly.
             */
            def explanation = {
              if ((found =:= AnyClass.tpe) && (AnyRefClass.tpe <:< req))
                "Note: Any is not implicitly converted to AnyRef.  You can safely\n" +
                "pattern match x: AnyRef or cast x.asInstanceOf[AnyRef] to do so."
              else if ((found <:< AnyValClass.tpe) && (AnyRefClass.tpe <:< req))
                "Note: primitive types are not implicitly converted to AnyRef.\n" +
                "You can safely force boxing by casting x.asInstanceOf[AnyRef]."
              else
                "Note that implicit conversions are not applicable because they are ambiguous:\n "+
                coreMsg+"are possible conversion functions from "+ found+" to "+req
            }

            typeErrorMsg(found, req) + "\n" + explanation
          }
          else {
            "ambiguous implicit values:\n "+coreMsg + "match expected type "+pt
          })
        }

    /** The type parameters to instantiate */
    val undetParams = if (isView) List() else context.outer.undetparams

    /** Replace undetParams in type `tp` by Any/Nothing, according to variance */
    def approximate(tp: Type) =
      tp.instantiateTypeParams(undetParams, undetParams map (_ => WildcardType))

    val wildPt = approximate(pt)

    /** Try to construct a typed tree from given implicit info with given
     *  expected type.
     *  Detect infinite search trees for implicits.
     *
     *  @param info    The given implicit info describing the implicit definition
     *  @pre           <code>info.tpe</code> does not contain an error
     */
    private def typedImplicit(info: ImplicitInfo): SearchResult =
      (context.openImplicits find { case (tp, sym) => sym == tree.symbol && dominates(pt, tp)}) match {
         case Some(pending) =>
           // println("Pending implicit "+pending+" dominates "+pt+"/"+undetParams) //@MDEBUG
           throw DivergentImplicit
           SearchFailure
         case None =>
           try {
             context.openImplicits = (pt, tree.symbol) :: context.openImplicits
             // println("  "*context.openImplicits.length+"typed implicit "+info+" for "+pt) //@MDEBUG
             typedImplicit0(info)
           } catch {
             case DivergentImplicit =>
               // println("DivergentImplicit for pt:"+ pt +", open implicits:"+context.openImplicits) //@MDEBUG
               if (context.openImplicits.tail.isEmpty) {
                 if (!(pt.isErroneous))
                   context.unit.error(
                     tree.pos, "diverging implicit expansion for type "+pt+"\nstarting with "+
                     info.sym+info.sym.locationString)
                 SearchFailure
               } else {
                 throw DivergentImplicit
               }
           } finally {
             context.openImplicits = context.openImplicits.tail
           }
       }

    private def typedImplicit0(info: ImplicitInfo): SearchResult = {

      /** Todo reconcile with definition of stability given in Types.scala */
      def isStable(tp: Type): Boolean = tp match {
        case TypeRef(pre, sym, _) =>
          sym.isPackageClass ||
          sym.isModuleClass && isStable(pre) /*||
          sym.isAliasType && isStable(tp.normalize)*/
        case _ => tp.isStable
      }

      /** Does type `tp' match expected type `pt'
       *  This is the case if either `pt' is a unary function type with a
       *  HasMethodMatching type as result, and `tp' is a unary function
       *  or method type whose result type has a method whose name and type
       *  correspond to the HasMethodMatching type,
       *  or otherwise if `tp' is compatible with `pt'.
       *  This method is performance critical: 5-8% of typechecking time.
       */
      def matchesPt(tp: Type, pt: Type, undet: List[Symbol]) = {
        val start = startTimer(matchesPtNanos)
        val result = normSubType(tp, pt) || isView && {
          pt match {
            case Function1(arg, res) =>
              matchesPtView(tp, arg, res, undet)
            case _ =>
              false
          }
        }
        stopTimer(matchesPtNanos, start)
        result
      }

      def matchesPtView(tp: Type, ptarg: Type, ptres: Type, undet: List[Symbol]): Boolean = tp match {
        case mt @ MethodType(params, restpe) =>
          if (mt.isImplicit) matchesPtView(restpe, ptarg, ptres, undet)
          else params.length == 1 && matchesArgRes(params.head.tpe, restpe, ptarg, ptres, undet)
        case ExistentialType(tparams, qtpe) =>
          matchesPtView(normalize(qtpe), ptarg, ptres, undet)
        case Function1(arg1, res1) =>
          matchesArgRes(arg1, res1, ptarg, ptres, undet)
        case _ => false
      }

      def matchesArgRes(tparg: Type, tpres: Type, ptarg: Type, ptres: Type, undet: List[Symbol]): Boolean =
        (ptarg weak_<:< tparg) && {
          ptres match {
            case HasMethodMatching(name, argtpes, restpe) =>
              (tpres.member(name) filter (m =>
                isApplicableSafe(undet, m.tpe, argtpes, restpe))) != NoSymbol
            case _ =>
              tpres <:< ptres
          }
        }

      incCounter(plausiblyCompatibleImplicits)

      printTyping("typed impl for "+wildPt+"? "+info.name +":"+ depoly(info.tpe)+ " orig info= "+ info.tpe +"/"+undetParams+"/"+isPlausiblyCompatible(info.tpe, wildPt)+"/"+matchesPt(depoly(info.tpe), wildPt, List())+"/"+info.pre+"/"+isStable(info.pre))
      if (matchesPt(depoly(info.tpe), wildPt, List()) && isStable(info.pre)) {

        incCounter(matchingImplicits)

        val itree = atPos(tree.pos.focus) {
          if (info.pre == NoPrefix) Ident(info.name)
          else Select(gen.mkAttributedQualifier(info.pre), info.name)
        }
        printTyping("typedImplicit0 typing"+ itree +" with wildpt = "+ wildPt +" from implicit "+ info.name+":"+info.tpe)
        def fail(reason: String): SearchResult = {
          if (settings.XlogImplicits.value)
            inform(itree+" is not a valid implicit value for "+pt+" because:\n"+reason)
          SearchFailure
        }
        try {
          val itree1 =
            if (isView)
              typed1 (
                atPos(itree.pos) (
                  Apply(itree, List(Ident("<argument>").setType(approximate(pt.typeArgs.head))))),
                EXPRmode, approximate(pt.typeArgs.tail.head)
              )
            else
              typed1(itree, EXPRmode, wildPt)

          incCounter(typedImplicits)

          printTyping("typed implicit "+itree1+":"+itree1.tpe+", pt = "+wildPt)
          val itree2 = if (isView) (itree1: @unchecked) match { case Apply(fun, _) => fun }
                       else adapt(itree1, EXPRmode, wildPt)
          printTyping("adapted implicit "+itree1.symbol+":"+itree2.tpe+" to "+wildPt)
          def hasMatchingSymbol(tree: Tree): Boolean = (tree.symbol == info.sym) || {
            tree match {
              case Apply(fun, _) => hasMatchingSymbol(fun)
              case TypeApply(fun, _) => hasMatchingSymbol(fun)
              case Select(pre, name) => name == nme.apply && pre.symbol == info.sym
              case _ => false
            }
          }

          if (itree2.tpe.isError) SearchFailure
          else if (hasMatchingSymbol(itree1)) {
            val tvars = undetParams map freshVar
            if (matchesPt(itree2.tpe, pt.instantiateTypeParams(undetParams, tvars), undetParams)) {
              printTyping("tvars = "+tvars+"/"+(tvars map (_.constr)))
              val targs = solvedTypes(tvars, undetParams, undetParams map varianceInType(pt),
                                      false, lubDepth(List(itree2.tpe, pt)))

              // #2421: check that we correctly instantiated type parameters outside of the implicit tree:
              checkBounds(itree2.pos, NoPrefix, NoSymbol, undetParams, targs, "inferred ")

              // filter out failures from type inference, don't want to remove them from undetParams!
              // we must be conservative in leaving type params in undetparams
              val AdjustedTypeArgs(okParams, okArgs) = adjustTypeArgs(undetParams, targs)  // prototype == WildcardType: want to remove all inferred Nothing's
              val subst = new TreeTypeSubstituter(okParams, okArgs)
              subst traverse itree2

              // #2421b: since type inference (which may have been performed during implicit search)
              // does not check whether inferred arguments meet the bounds of the corresponding parameter (see note in solvedTypes),
              // must check again here:
              // TODO: I would prefer to just call typed instead of duplicating the code here, but this is probably a hotspot (and you can't just call typed, need to force re-typecheck)
              itree2 match {
                case TypeApply(fun, args) => typedTypeApply(itree2, EXPRmode, fun, args)
                case Apply(TypeApply(fun, args), _) => typedTypeApply(itree2, EXPRmode, fun, args) // t2421c
                case _ =>
              }

              val result = new SearchResult(itree2, subst)
              incCounter(foundImplicits)
              if (traceImplicits) println("RESULT = "+result)
              // println("RESULT = "+itree+"///"+itree1+"///"+itree2)//DEBUG
              result
            } else {
              printTyping("incompatible: "+itree2.tpe+" does not match "+pt.instantiateTypeParams(undetParams, tvars))

              SearchFailure
            }
          } else if (settings.XlogImplicits.value)
            fail("candidate implicit "+info.sym+info.sym.locationString+
                 " is shadowed by other implicit: "+itree1.symbol+itree1.symbol.locationString)
          else SearchFailure
        } catch {
          case ex: TypeError => fail(ex.getMessage())
        }
      } else {
        SearchFailure
      }
    }

    /** Should implicit definition symbol `sym' be considered for applicability testing?
     *  This is the case if one of the following holds:
     *   - the symbol's type is initialized
     *   - the symbol comes from a classfile
     *   - the symbol comes from a different sourcefile than the current one
     *   - the symbol and the accessed symbol's definitions come before, and do not contain the closest enclosing definition, // see #3373
     *   - the symbol's definition is a val, var, or def with an explicit result type
     *  The aim of this method is to prevent premature cyclic reference errors
     *  by computing the types of only those implicits for which one of these
     *  conditions is true.
     */
    def isValid(sym: Symbol) = {
      def hasExplicitResultType(sym: Symbol) = {
        def hasExplicitRT(tree: Tree) = tree match {
          case ValDef(_, _, tpt, _) => !tpt.isEmpty
          case DefDef(_, _, _, _, tpt, _) => !tpt.isEmpty
          case _ => false
        }
        sym.rawInfo match {
          case tc: TypeCompleter => hasExplicitRT(tc.tree)
          case PolyType(_, tc: TypeCompleter) => hasExplicitRT(tc.tree)
          case _ => true
        }
      }
      def comesBefore(sym: Symbol, owner: Symbol) = {
        val ownerPos = owner.pos.pointOrElse(Integer.MAX_VALUE)
        sym.pos.pointOrElse(0) < ownerPos && (
          if(sym isGetterOrSetter) {
            val symAcc = sym.accessed // #3373
            symAcc.pos.pointOrElse(0) < ownerPos &&
            !(owner.ownerChain exists (o => (o eq sym) || (o eq symAcc))) // probably faster to iterate only once, don't feel like duplicating hasTransOwner for this case
          } else !(owner hasTransOwner sym)) // faster than owner.ownerChain contains sym
      }

      sym.isInitialized ||
      sym.sourceFile == null ||
      (sym.sourceFile ne context.unit.source.file) ||
      hasExplicitResultType(sym) ||
      comesBefore(sym, context.owner)
    }

    /** Computes from a list of lists of implicit infos a map which takes
     *  infos which are applicable for given expected type `pt` to their attributed trees.
     *  Computes invalid implicits as a side effect (used for better error message).
     *  @param iss            The given list of lists of implicit infos
     *  @param isLocal        Is implicit definition visible without prefix?
     *                        If this is the case then symbols in preceding lists shadow
     *                        symbols of the same name in succeeding lists.
     */
    def applicableInfos(iss: List[List[ImplicitInfo]],
                        isLocal: Boolean,
                        invalidImplicits: ListBuffer[Symbol]): Map[ImplicitInfo, SearchResult] = {

      val start = startCounter(subtypeAppInfos)

      /** A set containing names that are shadowed by implicit infos */
      lazy val shadowed = new HashSet[Name]("shadowed", 512)

      // #3453
      // in addition to the implicit symbols that may shadow the implicit with name `name`,
      // this method tests whether there's a non-implicit symbol with name `name` in scope
      // inspired by logic in typedIdent
      def nonImplicitSynonymInScope(name: Name) = {
        val defEntry = context.scope.lookupEntry(name)
        (defEntry ne null) &&
        reallyExists(defEntry.sym) &&
        !defEntry.sym.isImplicit // the implicit ones are handled by the `shadowed` set above
        // also, subsumes the test that defEntry.sym ne info.sym
        // (the `info` that's in scope at the call to nonImplicitSynonymInScope in tryImplicit)
      }

      /** Is `sym' the standard conforms method in Predef?
       *  Note: DON't replace this by sym == Predef_conforms, as Predef_conforms is a `def'
       *  which does a member lookup (it can't be a lazy val because we might reload Predef
       *  during resident compilations).
       */
      def isConformsMethod(sym: Symbol) =
        sym.name == nme.conforms && sym.owner == PredefModule.moduleClass

      /** Try implicit `info` to see whether it is applicable for expected type `pt`.
       *  This is the case if all of the following holds:
       *   - the info's type is not erroneous,
       *   - the info is not shadowed by another info with the same name,
       *   - we're not trying to infer a view that amounts to the identity function (specifically, Predef.identity or Predef.conforms)
       *   - the result of typedImplicit is non-empty.
       *   @return A search result with an attributed tree containing the implicit if succeeded,
       *           SearchFailure if not.
       *   @note Extreme hotspot!
       */
      def tryImplicit(info: ImplicitInfo): SearchResult = {
        incCounter(triedImplicits)
        if (info.isCyclicOrErroneous ||
            (isLocal && (shadowed.contains(info.name) || nonImplicitSynonymInScope(info.name))) ||
            (isView && isConformsMethod(info.sym)) ||
            //@M this condition prevents no-op conversions, which are a problem (besides efficiency),
            // one example is removeNames in NamesDefaults, which relies on the type checker failing in case of ambiguity between an assignment/named arg
            !isPlausiblyCompatible(info.tpe, wildPt))
           SearchFailure
        else
          typedImplicit(info)
      }

      def addAppInfos(is: List[ImplicitInfo], m: Map[ImplicitInfo, SearchResult]): Map[ImplicitInfo, SearchResult] = {
        var applicable = m
        for (i <- is)
          if (!isValid(i.sym)) invalidImplicits += i.sym
          else {
            val result = tryImplicit(i)
            if (result != SearchFailure) applicable += (i -> result)
          }
        if (isLocal)
          for (i <- is) shadowed addEntry i.name
        applicable
      }

      // #3453 -- alternative fix, seems not to be faster than encoding the set as the boolean predicate nonImplicitSynonymInScope
      // in addition to the *implicit* symbols that may shadow the implicit with name `name` (added to shadowed by addAppInfos)
      // add names of non-implicit symbols that are in scope (accessible without prefix)
      // for(sym <- context.scope; if !sym.isImplicit) shadowed addEntry sym.name

      var applicable = Map[ImplicitInfo, SearchResult]()
      for (is <- iss) applicable = addAppInfos(is, applicable)

      stopCounter(subtypeAppInfos, start)
      applicable
    }

    /** Search list of implicit info lists for one matching prototype
     *  <code>pt</code>. If found return a search result with a tree from found implicit info
     *  which is typed with expected type <code>pt</code>.
     *  Otherwise return SearchFailure.
     *
     *  @param implicitInfoss The given list of lists of implicit infos
     *  @param isLocal        Is implicit definition visible without prefix?
     *                        If this is the case then symbols in preceding lists shadow
     *                        symbols of the same name in succeeding lists.
     */
    def searchImplicit(implicitInfoss: List[List[ImplicitInfo]], isLocal: Boolean): SearchResult = {

      /** The implicits that are not valid because they come later in the source
       *  and lack an explicit result type. Used for error diagnostics only.
       */
      val invalidImplicits = new ListBuffer[Symbol]

      /** A map which takes applicable infos to their attributed trees. */
      val applicable = applicableInfos(implicitInfoss, isLocal, invalidImplicits)

      if (applicable.isEmpty && !invalidImplicits.isEmpty) {
        setAddendum(tree.pos, () =>
          "\n Note: implicit "+invalidImplicits.head+" is not applicable here"+
          " because it comes after the application point and it lacks an explicit result type")
      }

      val start = startCounter(subtypeImprovCount)

      /** A candidate for best applicable info wrt `improves` */
      val best = (NoImplicitInfo /: applicable.keysIterator) (
        (best, alt) => if (improves(alt, best)) alt else best)
      if (best == NoImplicitInfo) SearchFailure
      else {
        /** The list of all applicable infos which are not improved upon by `best`. */
        val competing = applicable.keySet dropWhile (alt => best == alt || improves(best, alt))
        if (!competing.isEmpty) ambiguousImplicitError(best, competing.head, "both", "and", "")

        stopCounter(subtypeImprovCount, start)
        applicable(best)
      }
    } // end searchImplicit

    /** The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     *    - for alias types and abstract types, we take instead the parts
     *    - of their upper bounds.
     *  @return For those parts that refer to classes with companion objects that
     *  can be accessed with unambiguous stable prefixes, the implicits infos
     *  which are members of these companion objects.
     */
    private def companionImplicits(tp: Type): List[List[ImplicitInfo]] = {

      val partMap = new LinkedHashMap[Symbol, Type]

      /** Enter all parts of `tp` into `parts` set.
       *  This method is performance critical: about 2-4% of all type checking is spent here
       */
      def getParts(tp: Type) {
        tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isClass) {
              if (!((sym.name == nme.REFINE_CLASS_NAME.toTypeName) ||
                    (sym.name startsWith nme.ANON_CLASS_NAME) ||
                    (sym.name == nme.ROOT.toTypeName)))
                partMap get sym match {
                  case Some(pre1) =>
                    if (!(pre =:= pre1)) partMap(sym) = NoType // ambiguous prefix - ignore implicit members
                  case None =>
                    if (pre.isStable) partMap(sym) = pre
                    val bts = tp.baseTypeSeq
                    var i = 1
                    while (i < bts.length) {
                      getParts(bts(i))
                      i += 1
                    }
                    getParts(pre)
                    args foreach getParts
                }
            } else if (sym.isAliasType) {
              getParts(tp.normalize)
            } else if (sym.isAbstractType) {
              getParts(tp.bounds.hi)
            }
          case ThisType(_) =>
            getParts(tp.widen)
          case _: SingletonType =>
            getParts(tp.widen)
          case RefinedType(ps, _) =>
            for (p <- ps) getParts(p)
          case AnnotatedType(_, t, _) =>
            getParts(t)
          case ExistentialType(_, t) =>
            getParts(t)
          case PolyType(_, t) =>
            getParts(t)
          case _ =>
        }
      }

      getParts(tp)
      val buf = new ListBuffer[List[ImplicitInfo]]
      for ((clazz, pre) <- partMap) {
        if (pre != NoType) {
          val companion = clazz.companionModule
          companion.moduleClass match {
            case mc: ModuleClassSymbol =>
              buf += (mc.implicitMembers map (im =>
                new ImplicitInfo(im.name, SingleType(pre, companion), im)))
            case _ =>
          }
        }
      }
      //println("companion implicits of "+tp+" = "+buf.toList) // DEBUG
      buf.toList
    }

    /** The implicits made available by type `pt`.
     *  These are all implicits found in companion objects of classes C
     *  such that some part of `tp` has C as one of its superclasses.
     */
    private def implicitsOfExpectedType: List[List[ImplicitInfo]] = implicitsCache get pt match {
      case Some(implicitInfoss) =>
        incCounter(implicitCacheHits)
        implicitInfoss
      case None                 =>
        incCounter(implicitCacheMisses)
        val start = startTimer(subtypeETNanos)
        val implicitInfoss = companionImplicits(pt)
        stopTimer(subtypeETNanos, start)
        implicitsCache(pt) = implicitInfoss
        if (implicitsCache.size >= sizeLimit)
          implicitsCache -= implicitsCache.keysIterator.next
        implicitInfoss
    }

    /** Creates a tree that calls the relevant factory method in object
      * reflect.Manifest for type 'tp'. An EmptyTree is returned if
      * no manifest is found. todo: make this instantiate take type params as well?
      */
    private def manifestOfType(tp: Type, full: Boolean): SearchResult = {

      /** Creates a tree that calls the factory method called constructor in object reflect.Manifest */
      def manifestFactoryCall(constructor: String, tparg: Type, args: Tree*): Tree =
        if (args contains EmptyTree) EmptyTree
        else typedPos(tree.pos.focus) {
          Apply(
            TypeApply(
              Select(gen.mkAttributedRef(if (full) FullManifestModule else PartialManifestModule), constructor),
              List(TypeTree(tparg))
            ),
            args.toList
          )
        }

      /** Creates a tree representing one of the singleton manifests.*/
      def findSingletonManifest(name: String) = typedPos(tree.pos.focus) {
        Select(gen.mkAttributedRef(FullManifestModule), name)
      }

      /** Re-wraps a type in a manifest before calling inferImplicit on the result */
      def findManifest(tp: Type, manifestClass: Symbol = if (full) FullManifestClass else PartialManifestClass) =
        inferImplicit(tree, appliedType(manifestClass.typeConstructor, List(tp)), true, false, context).tree

      def findSubManifest(tp: Type) = findManifest(tp, if (full) FullManifestClass else OptManifestClass)
      def mot(tp0: Type)(implicit from: List[Symbol] = List(), to: List[Type] = List()): SearchResult = {
        implicit def wrapResult(tree: Tree): SearchResult =
          if (tree == EmptyTree) SearchFailure else new SearchResult(tree, new TreeTypeSubstituter(from, to))

        val tp1 = tp0.normalize
        tp1 match {
          case ThisType(_) | SingleType(_, _) if !(tp1 exists {tp => tp.typeSymbol.isExistentiallyBound}) => // can't generate a reference to a value that's abstracted over by an existential
            manifestFactoryCall("singleType", tp, gen.mkAttributedQualifier(tp1))
          case ConstantType(value) =>
            manifestOfType(tp1.deconst, full)
          case TypeRef(pre, sym, args) =>
            if (isValueClass(sym) || isPhantomClass(sym)) {
              findSingletonManifest(sym.name.toString)
            } else if (sym == ObjectClass || sym == AnyRefClass) {
              findSingletonManifest("Object")
            } else if (sym == RepeatedParamClass || sym == ByNameParamClass) {
              EmptyTree
            } else if (sym == ArrayClass && args.length == 1) {
              manifestFactoryCall("arrayType", args.head, findManifest(args.head))
            } else if (sym.isClass) {
              val classarg0 = gen.mkClassOf(tp1)
              val classarg = tp match {
                case ExistentialType(_, _) =>
                  TypeApply(Select(classarg0, Any_asInstanceOf),
                            List(TypeTree(appliedType(ClassClass.typeConstructor, List(tp)))))
                case _ =>
                  classarg0
              }
              val suffix = classarg :: (args map findSubManifest)
              manifestFactoryCall(
                "classType", tp,
                (if ((pre eq NoPrefix) || pre.typeSymbol.isStaticOwner) suffix
                 else findSubManifest(pre) :: suffix): _*)
            } else if (sym.isExistentiallyBound && full) {
              manifestFactoryCall("wildcardType", tp,
                                  findManifest(tp.bounds.lo), findManifest(tp.bounds.hi))
            } else if(undetParams contains sym) { // looking for a manifest of a type parameter that hasn't been inferred by now, can't do much, but let's not fail
              mot(NothingClass.tpe)(sym :: from, NothingClass.tpe :: to) // #3859: need to include the mapping from sym -> NothingClass.tpe in the SearchResult
            } else {
              EmptyTree  // a manifest should have been found by normal searchImplicit
            }
          case RefinedType(parents, decls) =>
            // refinement is not generated yet
            if (parents.length == 1) findManifest(parents.head)
            else if (full) manifestFactoryCall("intersectionType", tp, parents map (findSubManifest(_)): _*)
            else mot(erasure.erasure.intersectionDominator(parents))
          case ExistentialType(tparams, result) =>
            mot(tp1.skolemizeExistential)
          case _ =>
            EmptyTree
        }
      }

      mot(tp)
    }

    def wrapResult(tree: Tree): SearchResult =
      if (tree == EmptyTree) SearchFailure else new SearchResult(tree, EmptyTreeTypeSubstituter)

    /** The manifest corresponding to type `pt`, provided `pt` is an instance of Manifest.
     */
    private def implicitManifestOrOfExpectedType(pt: Type): SearchResult = pt.dealias match {
      case TypeRef(_, FullManifestClass, List(arg)) =>
        manifestOfType(arg, true)
      case TypeRef(_, PartialManifestClass, List(arg)) =>
        manifestOfType(arg, false)
      case TypeRef(_, OptManifestClass, List(arg)) =>
        val res = manifestOfType(arg, false)
        if (res == SearchFailure) wrapResult(gen.mkAttributedRef(NoManifest)) else res
      case TypeRef(_, tsym, _) if (tsym.isAbstractType) =>
        implicitManifestOrOfExpectedType(pt.bounds.lo)
      case _ =>
        searchImplicit(implicitsOfExpectedType, false)
    }

    /** The result of the implicit search:
     *  First search implicits visible in current context.
     *  If that fails, search implicits in expected type `pt`.
     *  If that fails, and `pt` is an instance of Manifest, try to construct a manifest.
     *  If all fails return SearchFailure
     */
    def bestImplicit: SearchResult = {
      val failstart = startTimer(inscopeFailNanos)
      val succstart = startTimer(inscopeSucceedNanos)

      var result = searchImplicit(context.implicitss, true)

      if (result == SearchFailure) {
        stopTimer(inscopeFailNanos, failstart)
      } else {
        stopTimer(inscopeSucceedNanos, succstart)
        incCounter(inscopeImplicitHits)
      }
      if (result == SearchFailure) {
        val failstart = startTimer(oftypeFailNanos)
        val succstart = startTimer(oftypeSucceedNanos)

        result = implicitManifestOrOfExpectedType(pt)

        if (result == SearchFailure) {
          stopTimer(oftypeFailNanos, failstart)
        } else {
          stopTimer(oftypeSucceedNanos, succstart)
          incCounter(oftypeImplicitHits)
        }
      }

      if (result == SearchFailure && settings.debug.value)
        log("no implicits found for "+pt+" "+pt.typeSymbol.info.baseClasses+" "+implicitsOfExpectedType)

      result
    }

    def allImplicits: List[SearchResult] = {
      val invalidImplicits = new ListBuffer[Symbol]
      def search(iss: List[List[ImplicitInfo]], isLocal: Boolean) =
        applicableInfos(iss, isLocal, invalidImplicits).values.toList
      search(context.implicitss, true) ::: search(implicitsOfExpectedType, false)
    }
  }

  object ImplicitNotFoundMsg {
    def unapply(sym: Symbol): Option[(Message)] = sym.implicitNotFoundMsg map (m => (new Message(sym, m)))
    // check the message's syntax: should be a string literal that may contain occurences of the string "${X}",
    // where `X` refers to a type parameter of `sym`
    def check(sym: Symbol): Option[String] =
      sym.getAnnotation(ImplicitNotFoundClass).flatMap(_.stringArg(0) match {
        case Some(m) => new Message(sym, m) validate
        case None => Some("Missing argument `msg` on implicitNotFound annotation.")
      })


    class Message(sym: Symbol, msg: String) {
      // http://dcsobral.blogspot.com/2010/01/string-interpolation-in-scala-with.html
      private def interpolate(text: String, vars: Map[String, String]) = { import scala.util.matching.Regex
        """\$\{([^}]+)\}""".r.replaceAllIn(text, (_: Regex.Match) match {
          case Regex.Groups(v) => vars.getOrElse(v, "")
        })}

      private lazy val typeParamNames: List[String] = sym.typeParams.map(_.decodedName)

      def format(paramName: Name, paramTp: Type): String = format(paramTp.typeArgs map (_.toString))
      def format(typeArgs: List[String]): String =
        interpolate(msg, Map((typeParamNames zip typeArgs): _*)) // TODO: give access to the name and type of the implicit argument, etc?

      def validate: Option[String] = {
        import scala.util.matching.Regex; import collection.breakOut
        // is there a shorter way to avoid the intermediate toList?
        val refs = Set("""\$\{([^}]+)\}""".r.findAllIn(msg).matchData.map(_.group(1)).toList : _*)
        val decls = Set(typeParamNames : _*)
        (refs &~ decls) match {
          case s if s isEmpty => None
          case unboundNames =>
            val singular = unboundNames.size == 1
            Some("The type parameter"+( if(singular) " " else "s " )+ unboundNames.mkString(", ")  +
                  " referenced in the message of the @implicitNotFound annotation "+( if(singular) "is" else "are" )+
                  " not defined by "+ sym +".")
        }
      }
    }
  }

  private val DivergentImplicit = new Exception()
}
