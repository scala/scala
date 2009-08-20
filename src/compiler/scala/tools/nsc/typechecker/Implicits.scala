/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Typers.scala 17229 2009-03-02 19:09:42Z extempore $

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly
package scala.tools.nsc
package typechecker

import scala.collection.mutable.{LinkedHashMap, ListBuffer}
import scala.tools.nsc.util.{HashSet, Position, Set, NoPosition, SourceFile}
import symtab.Flags._
import util.HashSet

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Implicits {
self: Analyzer =>

  import global._
  import definitions._

  final val traceImplicits = false

  var implicitTime = 0L
  var inscopeSucceed = 0L
  var inscopeFail = 0L
  var oftypeSucceed = 0L
  var oftypeFail = 0L
  var manifSucceed = 0L
  var manifFail = 0L
  var hits = 0
  var misses = 0
  var uncached = 0

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
    if (traceImplicits && !tree.isEmpty && !context.undetparams.isEmpty)
      println("typing implicit with undetermined type params: "+context.undetparams+"\n"+tree)
    val result = new ImplicitSearch(tree, pt, isView, context.makeImplicit(reportAmbiguous)).bestImplicit
    context.undetparams = context.undetparams remove (result.subst.from contains _)
    result
  }

  final val sizeLimit = 50000
  val implicitsCache = new LinkedHashMap[AnyRef, SearchResult]

  def resetImplicits() { implicitsCache.clear() }

  /** If type `pt` an instance of Manifest or OptManifest, or an abstract type lower-bounded
   *  by such an instance?
   */
  def isManifest(pt: Type): Boolean = pt.dealias match {
    case TypeRef(_, ManifestClass, List(_)) |
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

    override def equals(other: Any) = other match {
      case that: ImplicitInfo =>
        if (this eq NoImplicitInfo) that eq this
        else
          this.name == that.name &&
          this.pre =:= that.pre &&
          this.sym == that.sym
      case _ =>
        false
    }

    override def hashCode =
      name.hashCode + pre.hashCode + sym.hashCode

    override def toString = "ImplicitInfo(" + name + "," + pre + "," + sym + ")"
  }

  /** A sentinel indicating no implicit was found */
  val NoImplicitInfo = new ImplicitInfo(null, NoType, NoSymbol)

  /** A class that sets up an implicit search. For more info, see comments for `inferImplicit`.
   *  @param tree             The tree for which the implicit needs to be inserted.
   *  @param pt               The original expected type of the implicit.
   *  @param isView           We are looking for a view
   *  @param context0         The context used for the implicit search
   */
  class ImplicitSearch(tree: Tree, pt: Type, isView: Boolean, context0: Context)
    extends Typer(context0) {

//    assert(tree.isEmpty || tree.pos.isDefined, tree)

    import infer._

    /** Is implicit info `info1` better than implicit info `info2`?
     */
    def improves(info1: ImplicitInfo, info2: ImplicitInfo) =
      (info2 == NoImplicitInfo) ||
      (info1 != NoImplicitInfo) &&
      isStrictlyMoreSpecific(info1.tpe, info2.tpe, info1.sym, info2.sym)

    /** Map all type params in given list to WildcardType
     *  @param   tp  The type in which to do the mapping
     *  @param   tparams  The list of type parameters to map
     */
    private def tparamsToWildcards(tp: Type, tparams: List[Symbol]) =
      tp.instantiateTypeParams(tparams, tparams map (t => WildcardType))

    /* Map a polytype to one in which all type parameters are replaced by wildcards.
     */
    private def depoly(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) => tparamsToWildcards(restpe, tparams)
      case _ => tp
    }

    /** Does type `tp` contain an Error type as parameter or result?
     */
    private def containsError(tp: Type): Boolean = tp match {
      case PolyType(tparams, restpe) => containsError(restpe)
      case MethodType(params, restpe) => (params map (_.tpe) exists (_.isError)) || containsError(restpe)
      case _ => tp.isError
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
     *  nodes in its ast, except that singleton types are widened befoe taking the complexity.
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

    if (util.Statistics.enabled) implcnt += 1

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
            typeErrorMsg(found, req)+
            "\nNote that implicit conversions are not applicable because they are ambiguous:\n "+
            coreMsg+"are possible conversion functions from "+ found+" to "+req
          } else {
            "ambiguous implicit values:\n "+coreMsg + "match expected type "+pt
          })
        }

    /** The type parameters to instantiate */
    val undetParams = if (isView) List() else context.outer.undetparams

    /** Try to construct a typed tree from given implicit info with given
     *  expected type.
     *  Detect infinite search trees for implicits.
     *
     *  @param info    The given implicit info describing the implicit definition
     *  @pre           <code>info.tpe</code> does not contain an error
     */
    private def typedImplicit(info: ImplicitInfo): SearchResult =
       context.openImplicits find (dominates(pt, _)) match {
         case Some(pending) =>
           // println("Pending implicit "+pending+" dominates "+pt+"/"+undetParams) //@MDEBUG
           throw DivergentImplicit
           SearchFailure
         case None =>
           try {
             context.openImplicits = pt :: context.openImplicits
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

      /** Replace undetParams in type `tp` by Any/Nothing, according to variance */
      def approximate(tp: Type) =
        tp.instantiateTypeParams(undetParams, undetParams map (_ => WildcardType))

      /** Instantiated `pt' so that undetermined type parameters are replaced by wildcards
       */
      val wildPt = approximate(pt)

      //if (traceImplicits) println("typed impl for "+wildPt+"? "+info.name+":"+info.tpe+"/"+undetParams)
      if (isPlausiblyCompatible(info.tpe, wildPt) &&
          isCompatible(depoly(info.tpe), wildPt) &&
          isStable(info.pre)) {

        val itree = atPos(tree.pos.focus) {
          if (info.pre == NoPrefix) Ident(info.name)
          else Select(gen.mkAttributedQualifier(info.pre), info.name)
        }
        //if (traceImplicits) println("typed impl?? "+info.name+":"+info.tpe+" ==> "+itree+" with "+wildPt)
        def fail(reason: String): SearchResult = {
          if (settings.XlogImplicits.value)
            inform(itree+" is not a valid implicit value for "+pt+" because:\n"+reason)
          SearchFailure
        }
        try {
          val itree1 =
            if (isView)
              typed1(
                atPos(itree.pos) (
                  Apply(itree, List(Ident("<argument>").setType(approximate(pt.typeArgs.head))))),
                  EXPRmode, approximate(pt.typeArgs.tail.head))
            else
              typed1(itree, EXPRmode, wildPt)

          if (traceImplicits) println("typed implicit "+itree1+":"+itree1.tpe+", pt = "+wildPt)
          val itree2 = if (isView) (itree1: @unchecked) match { case Apply(fun, _) => fun }
                       else adapt(itree1, EXPRmode, wildPt)
          if (traceImplicits) println("adapted implicit "+itree1.symbol+":"+itree2.tpe+" to "+wildPt)
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
            if (isCompatible(itree2.tpe, pt.instantiateTypeParams(undetParams, tvars))) {
              if (traceImplicits) println("tvars = "+tvars+"/"+(tvars map (_.constr)))
              val targs = solvedTypes(tvars, undetParams, undetParams map varianceInType(pt),
                                      false, lubDepth(List(itree2.tpe, pt)))
              val subst = new TreeTypeSubstituter(undetParams, targs)
              subst traverse itree2
              // todo: remove type params that have been instantiated to Nothing, similar
              // to methTypeArgs
              val result = new SearchResult(itree2, subst)
              if (traceImplicits) println("RESULT = "+result)
              // println("RESULT = "+itree+"///"+itree1+"///"+itree2)//DEBUG
              result
            } else {
              if (traceImplicits) println("incompatible???")
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
     *   - the symbol's definition comes before, and does not contain the closest enclosing definition,
     *   - the symbol's definition is a val, var, or def with an explicit result type
     *  The aim of this method is to prevent premature cyclic reference errors
     *  by computing the types of only those implicitis for which one of these
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
      def comesBefore(sym: Symbol, owner: Symbol) =
        sym.pos.offset.getOrElse(0) < owner.pos.offset.getOrElse(Integer.MAX_VALUE) &&
        !(owner.ownerChain contains sym)

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

      /** A set containing names that are shadowed by implicit infos */
      val shadowed = new HashSet[Name](8)

      /** Try implicit `info` to see whether it is applicable for expected type `pt`.
       *  This is the case if all of the following holds:
       *   - the info's type is not erroneous,
       *   - the info is not shadowed by another info with the same name,
       *   - the result of typedImplicit is non-empty.
       *   @return A search result with an attributed tree containing the implicit if succeeded,
       *           SearchFailure if not.
       */
      def tryImplicit(info: ImplicitInfo): SearchResult =
        if (containsError(info.tpe) ||
            (isLocal && shadowed.contains(info.name)) //  || (isView && (info.sym == Predef_identity || info.sym == Predef_conforms))
           ) SearchFailure
        else typedImplicit(info)

      def appInfos(is: List[ImplicitInfo]): Map[ImplicitInfo, SearchResult] = {
        var applicable = Map[ImplicitInfo, SearchResult]()
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

      (Map[ImplicitInfo, SearchResult]() /: (iss map appInfos))(_ ++ _)
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
        infer.setAddendum(tree.pos, () =>
          "\n Note: implicit "+invalidImplicits.first+" is not applicable here"+
          "\n because it comes after the application point and it lacks an explicit result type")
      }

      /** A candidate for best applicable info wrt `improves` */
      val best = (NoImplicitInfo /: applicable.keys) (
        (best, alt) => if (improves(alt, best)) alt else best)
      if (best == NoImplicitInfo) SearchFailure
      else {
        /** The list of all applicable infos which are not improved upon by `best`. */
        val competing = applicable.keySet dropWhile (alt => best == alt || improves(best, alt))
        if (!competing.isEmpty) ambiguousImplicitError(best, competing.iterator.next, "both", "and", "") // !!! streamline when new collection is there

        // Also check that applicable infos that did not get selected are not
        // in (a companion object of) a subclass of (a companion object of) the class
        // containing the winning info.
        // (no longer needed; rules have changed)
        /*
        for (alt <- applicable.keySet) {
          if (isProperSubClassOrObject(alt.sym.owner, best.sym.owner)) {
            ambiguousImplicitError(best, alt,
                                   "most specific definition is:",
                                   "yet alternative definition  ",
                                   "is defined in a subclass.\n Both definitions ")
          }
        }
        */
        applicable(best)
      }
    } // end searchImplicit

    /** The implicits made available directly by class type `tp`.
     *  If `tp` refers to class C, these are all implicit members of the companion object of C.
     */
    private def implicitsOfClass(tp: Type): List[ImplicitInfo] = tp match {
      case TypeRef(pre, clazz, _) =>
        clazz.initialize.linkedClassOfClass.info.members.toList.filter(_.hasFlag(IMPLICIT)) map
        (sym => new ImplicitInfo(sym.name, pre.memberType(clazz.linkedModuleOfClass), sym))
      case _ =>
        List()
    }

    /** The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     */
    private def parts(tp: Type): List[Type] = {
      val partMap = new collection.mutable.LinkedHashMap[Symbol, List[Type]]
      /** Add a new type to partMap, unless a subtype of it with the same
       *  type symbol exists already.
       */
      def addType(newtp: Type): Boolean = {
        val tsym = newtp.typeSymbol
        partMap.get(tsym) match {
          case Some(ts) =>
            if (ts exists (_ <:< newtp)) false
            else { partMap.put(tsym, newtp :: ts); true }
          case None =>
            partMap.put(tsym, List(newtp)); true
        }
      }
      /** Enter all parts of `tp` into `partMap`
       */
      def getParts(tp: Type) {
        tp match {
          case TypeRef(pre, sym, args) if (!sym.isPackageClass) =>
            if (sym.isClass && !sym.isRefinementClass && !sym.isAnonymousClass) {
              if (addType(tp)) {
                for (bc <- sym.info.baseClasses.tail)
                  getParts(tp.baseType(bc))
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
          case ExistentialType(tparams, t) =>
            getParts(t)
          case _ =>
        }
      }
      /** Gives a list of typerefs with the same type symbol,
       *  remove all those that have a prefix which is a supertype
       *  of some other elements's prefix.
       */
      def compactify(ts: List[Type]): List[Type] = ts match {
        case List() => ts
        case (t @ TypeRef(pre, _, _)) :: ts1 =>
          if (ts1 exists (_.prefix <:< pre)) compactify(ts1)
          else t :: compactify(ts1 remove (pre <:< _.prefix))
      }
      getParts(tp)
      for ((k, ts) <- partMap.iterator.toList; t <- compactify(ts)) yield t
    }

    /** The implicits made available by type `pt`.
     *  These are all implicits found in companion objects of classes C
     *  such that some part of `tp` has C as one of its superclasses.
     */
    private def implicitsOfExpectedType: List[List[ImplicitInfo]] =
      parts(pt).iterator.map(implicitsOfClass).toList

    /** The manifest corresponding to type `pt`, provided `pt` is an instance of Manifest.
     */
    private def implicitManifest(pt: Type): Tree = pt match {
      case TypeRef(_, ManifestClass, List(arg)) =>
        manifestOfType(arg)
      case TypeRef(_, OptManifestClass, List(arg)) =>
        val itree = manifestOfType(arg)
        if (itree == EmptyTree) gen.mkAttributedRef(NoManifest) else itree
      case TypeRef(_, tsym, _) if (tsym.isAbstractType) =>
        implicitManifest(pt.bounds.lo)
      case _ =>
        EmptyTree
    }

    /** Creates a tree that calls the relevant factory method in object
      * reflect.Manifest for type 'tp'. An EmptyTree is returned if
      * no manifest is found. todo: make this instantiate take type params as well?
      */
    private def manifestOfType(tp: Type): Tree = {

      /** Creates a tree that calls the factory method called constructor in object reflect.Manifest */
      def manifestFactoryCall(constructor: String, args: Tree*): Tree =
        if (args contains EmptyTree) EmptyTree
        else
          typed { atPos(tree.pos.focus) {
            Apply(
              TypeApply(
                Select(gen.mkAttributedRef(ManifestModule), constructor),
                List(TypeTree(tp))
              ),
              args.toList
            )
          }}

      /** Re-wraps a type in a manifest before calling inferImplicit on the result */
      def findManifest(tp: Type): Tree =
        inferImplicit(tree, appliedType(ManifestClass.typeConstructor, List(tp)), true, false, context).tree

      tp.normalize match {
        case ThisType(_) | SingleType(_, _) =>
          manifestFactoryCall("singleType", gen.mkAttributedQualifier(tp))
        case ConstantType(value) =>
          findManifest(tp.deconst)
        case TypeRef(pre, sym, args) =>
          if (isValueClass(sym)) {
            typed { atPos(tree.pos.focus) {
              Select(gen.mkAttributedRef(ManifestModule), sym.name.toString)
            }}
          }
          else if (sym.isClass) {
            val suffix = gen.mkClassOf(tp) :: (args map findManifest)
            manifestFactoryCall(
              "classType",
              (if ((pre eq NoPrefix) || pre.typeSymbol.isStaticOwner) suffix
               else findManifest(pre) :: suffix): _*)
          }
          else if (sym.isTypeParameterOrSkolem) {
            EmptyTree  // a manifest should have been found by normal searchImplicit
          }
          else {
            manifestFactoryCall(
              "abstractType",
              findManifest(pre) :: Literal(sym.name.toString) :: findManifest(tp.bounds.hi) :: (args map findManifest): _*)
          }
        case RefinedType(parents, decls) =>
          // refinement is not generated yet
          if (parents.length == 1) findManifest(parents.head)
          else manifestFactoryCall("intersectionType", parents map findManifest: _*)
        case _ =>
          EmptyTree
      }
    }

    /** An extractor for types of the form ? { name: ? }
     */
    object WildcardName {
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

    /** Return cached search result if found. Otherwise update cache
     *  but keep within sizeLimit entries
     */
    def cacheResult(key: AnyRef): SearchResult = implicitsCache get key match {
      case Some(sr: SearchResult) =>
        hits += 1
        if (sr == SearchFailure) sr
        else {
          val result = new SearchResult(sr.tree.duplicate, sr.subst)
          for (t <- result.tree) t.setPos(tree.pos.focus)
          result
        }
      case None =>
        misses += 1
        val r = searchImplicit(implicitsOfExpectedType, false)
        //println("new fact: search implicit of "+key+" = "+r)
        implicitsCache(key) = r
        if (implicitsCache.size >= sizeLimit)
          implicitsCache -= implicitsCache.keysIterator.next
        r
    }

    /** The result of the implicit search:
     *  First search implicits visible in current context.
     *  If that fails, search implicits in expected type `pt`.
     *  If that fails, and `pt` is an instance of Manifest, try to construct a manifest.
     *  If all fails return SearchFailure
     */
    def bestImplicit: SearchResult = {
      val start = System.nanoTime()
      var result = searchImplicit(context.implicitss, true)
      val timer1 = System.nanoTime()
      if (result == SearchFailure) inscopeFail += timer1 - start else inscopeSucceed += timer1 - start
      if (result == SearchFailure) {
        result = pt match {
          case _: UniqueType => cacheResult(pt)
          case WildcardName(name) => cacheResult(name)
          case _ => uncached += 1; searchImplicit(implicitsOfExpectedType, false)
         }
      }

      val timer2 = System.nanoTime()
      if (result == SearchFailure) oftypeFail += timer2 - timer1 else oftypeSucceed += timer2 - timer1
      if (result == SearchFailure) {
        val resultTree = implicitManifest(pt)
        if (resultTree != EmptyTree) result = new SearchResult(resultTree, EmptyTreeTypeSubstituter)
      }
      val timer3 = System.nanoTime()
      if (result == SearchFailure) manifFail += timer3 - timer2 else manifSucceed += timer3 - timer2
      if (result == SearchFailure && settings.debug.value)
        println("no implicits found for "+pt+" "+pt.typeSymbol.info.baseClasses+" "+parts(pt)+implicitsOfExpectedType)
      implicitTime += System.nanoTime() - start
      result
    }

    def allImplicits: List[SearchResult] = {
      val invalidImplicits = new ListBuffer[Symbol]
      def search(iss: List[List[ImplicitInfo]], isLocal: Boolean) =
        applicableInfos(iss, isLocal, invalidImplicits).values.toList
      search(context.implicitss, true) ::: search(implicitsOfExpectedType, false)
    }
  }

  private val DivergentImplicit = new Exception()
}
