/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{LinkedHashMap, LinkedHashSet, TreeSet}

abstract class LambdaLift extends InfoTransform {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "lambdalift"

  /**
    * Since we move classes out of methods, must change the corresponding typerefs.
    */
  private val localTypeRefToLifted = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(NoPrefix, sym, Nil) if sym.isClass && !sym.isPackageClass => // we know sym.owner.isLocal since prefix == NoPrefix
        val newPrefix = apply(liftedOwner(sym).thisType)
        TypeRef(newPrefix, sym, Nil)

      // this case is not in the standard TypeMap, but we need to treat parent types here (not sure why decls are skipped...)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = parents mapConserve this
        if (parents1 eq parents) tp
        else ClassInfoType(parents1, decls, clazz)

      case _ =>
        mapOver(tp)
    }
  }

  // We must rigidly lift to this owner. We can't use the outcome of the tree analysis, since the info/type transform happens before the tree transform...
  private def liftedOwner(sym: Symbol): Symbol = sym.owner.enclClass

  private def classCanStoreFields(sym: Symbol): Boolean = sym.isClass && !sym.isTrait

  /** Each scala.runtime.*Ref class has a static method `create(value)` that simply instantiates the Ref to carry that value. */
  private lazy val refCreateMethod: Map[Symbol, Symbol] = {
    mapFrom(allRefClasses.toList)(x => getMemberMethod(x.companionModule, nme.create))
  }

  /** Quite frequently a *Ref is initialized with its zero (e.g., null, 0.toByte, etc.) Method `zero()` of *Ref class encapsulates that pattern. */
  private lazy val refZeroMethod: Map[Symbol, Symbol] = {
    mapFrom(allRefClasses.toList)(x => getMemberMethod(x.companionModule, nme.zero))
  }

  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isCapturedVariable) capturedVariableType(sym, tpe = localTypeRefToLifted(tp), erasedTypes = true)
    else localTypeRefToLifted(tp)

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new LambdaLifter(unit)

  class LambdaLifter(unit: CompilationUnit) extends explicitOuter.OuterPathTransformer(unit) {
    private type SymSet = TreeSet[Symbol]
    private val ord = Ordering.fromLessThan[Symbol](_ isLess _)
    private def newSymSet: SymSet = TreeSet.empty[Symbol](ord)
    private def symSet(f: LinkedHashMap[Symbol, SymSet], sym: Symbol): SymSet = f.getOrElseUpdate(sym, newSymSet)

    /** A hashtable storing calls between functions */
    private val called = new LinkedHashMap[Symbol, SymSet]

    /** Symbols that are called from an inner class. */
    private val calledFromInner = new LinkedHashSet[Symbol]

    /** A map storing free variables of functions and classes */
    private val free = new LinkedHashMap[Symbol, SymSet]

    /** A map storing the free variable proxies of functions and classes
      *
      * For every function and class, this is a map from the free variables
      * of that function or class to the proxy symbols accessing them.
      */
    private val proxyMap = new LinkedHashMap[Symbol, Map[Symbol, Symbol]]

    // needed to generate a fresh name no more than once for each symbol (??? -- it's definitely needed, but not sure why it crashed)
    private val proxyNameOfFree = new LinkedHashMap[Symbol, TermName]

    /** Local methods and classes that need lifting. */
    private val needsLifting = newSymSet

    /** Buffers for lifted out classes and methods */
    private val liftedDefs = new LinkedHashMap[Symbol, mutable.ListBuffer[Tree]]

    /** A flag to indicate whether new free variables have been found */
    private var changedFreeVars: Boolean = _

    private def currentEnclosure = currentOwner.enclosingMethodOrClass

    // TODO: rename to inCurrentEnclosure?
    private def isSameOwnerEnclosure(sym: Symbol) = sym.enclosure == currentEnclosure

    /** A symbol is local if it is owned by a term or a local trait,
      *  or if it is a constructor of a local symbol.
      *
      * A "local" symbol cannot store the value of a captured variable,
      * so it must receive its captures as arguments or get to them via an outer path.
      * (If there's a path from its outer to an enclosing class that stores the capture.)
      * A trait does not have storage, so it must be treated like a method.
      */
    @tailrec private def isLocal(sym: Symbol): Boolean = {
      val owner = sym.owner
      owner.isTerm || ((owner.isTrait || sym.isConstructor) && isLocal(owner))
    }


    //    private val liftedOwner = new LinkedHashMap[Symbol, Symbol]
    /** A flag to indicate whether lifted owners have changed */
//    private var changedLiftedOwner: Boolean = _

    // def needsLifting(local: Symbol): Boolean = liftedOwner.contains(local)

    def liftLocalTo(local: Symbol, newOwner: Symbol): Unit = needsLifting += local // liftedOwner(local) = newOwner

    /** Set `liftedOwner(sym)` to `newOwner` if `newOwner` is more deeply nested
      *  than the previous value of `liftedOwner(sym)`.
      */
//    def lowerLiftedOwner(sym: Symbol, newOwner: Symbol) = if (newOwner != sym) {
//      val origOwner = liftedOwner(sym)
//      if (newOwner != origOwner && sym.owner.isTerm && (newOwner hasTransOwner origOwner)) {
//        log(s"narrow lifted $sym to $newOwner")
//        changedLiftedOwner = true
//        liftLocalTo(sym, newOwner)
//      }
//    }

    /** Compute final liftedOwner map by closing over caller dependencies */
//    private def computeLiftedOwners(): Unit = do {
//        changedLiftedOwner = false
//
//        for { (caller, callees) <- called } {
//          // newOwners is a set
//          val newOwners = callees map { callee =>
//            val calleeOrConstructedClass = callee.skipConstructor
//            val calleeOwner = calleeOrConstructedClass.owner
//
//            if (calleeOwner.isTerm) liftedOwner(calleeOrConstructedClass)
//            else {
//              assert(calleeOwner.isTrait)
//              // Methods nested inside local trait methods cannot be lifted out beyond the trait.
//              // Note that we can also call a trait method through a qualifier;
//              // in that case no restriction to lifted owner arises.
//              if (caller hasTransOwner calleeOwner) calleeOwner
//              else caller // marker not to change anything
//            }
//          }
//
//          newOwners.foreach(lowerLiftedOwner(caller, _))
//        }
//      } while (changedLiftedOwner)


    private def markFree(local: Symbol, site: Symbol): Unit = {
      assert(
        isLocal(local) && local.isTerm && !local.isMethod
          && (site.isMethod || site.isClass))

      // track `sym` as `free` in `site`
      def registerFree(site: Symbol): Unit =
        if (symSet(free, site) add local) {
          debuglog(s"free: $local free in $site")
          changedFreeVars = true
          if (local.isVariable) local setFlag CAPTURED
        }

      val enclosureChain = {
        val localDefSite = local.enclosure

        @tailrec def loop(enclosure: Symbol, chain: List[Symbol]): List[Symbol] =
          if (enclosure == localDefSite || enclosure == NoSymbol || enclosure.isPackageClass) chain
          else loop(enclosure.enclosure, enclosure :: chain)

        loop(site, Nil)
      }

      debuglog(s"chain for $local ref from ${site.fullLocationString}: $enclosureChain")

      // Find a storage site that we can access via outer pointers, or else we must consider the symbol free.
      // Only non-trait classes can store captures.
      // Not all classes have outer pointers (e.g., anonymous function).
      // Constructors must be marked free if their owner was marked free (so we can pass in the values).
      // We can only add arguments to local methods.
      // All references to local values must occur in local methods (or the captured local would not have been in scope).
      var path = enclosureChain
      var storage: Symbol = null

      while (path.nonEmpty) {
        val curr = path.head
        val currClass = curr.enclClass

        if (currClass == storage) {
          // must get the captures to the constructor, so we can store them in fields in the class
          if (curr.isConstructor) registerFree(curr)
        } else if ((storage ne null) && !explicitOuter.outerAccessor(currClass).exists) {
          // we had stored the capture, but we can't reach it since we don't have an outer pointer
          debuglog(s"lost access to storage $storage in $curr")
          storage = null
        }

        if (storage eq null) {
          // A new opportunity to store our capture, but we'll need to pass it in.
          // Subsequent elements on the chain may be able to access this storage.
          if (classCanStoreFields(curr)) {
            storage = curr
            registerFree(curr)
          } else {
            // We can't add arguments to non-local methods since we don't see all invocations
            val canHaveFree = curr.isMethod && isLocal(curr)

            if (canHaveFree) {
              // trait T { def foo(x: Int) = { trait U { def bar = x } } }
              registerFree(curr)
            } else {
              debuglog(s"can't mark $local free in $curr in chain $enclosureChain")
            }
          }
        }

        path = path.tail
      }
    }



    private def markCalled(callee: Symbol, caller: Symbol): Unit = {
      registerCalled(callee, caller)

      if (callee.enclClass != caller.enclClass)
        calledFromInner += callee
    }

    private def registerCalled(callee: Symbol, caller: Symbol): Unit = {
      assert(isLocal(callee))
      if (symSet(called, caller) add callee)
        debuglog(s"called: $caller calls ${callee.fullLocationString}")
    }

    /** The traverse function */
    private class CollectDependencies extends Traverser {
      private def currentEnclosure = currentOwner.enclosingMethodOrClass

      private def markThisRef(thisClass: Symbol) = {} // if (currentEnclosure hasTransOwner thisClass) lowerLiftedOwner(currentEnclosure, thisClass)

      override def traverse(tree: Tree) {
        val sym = tree.symbol
        tree match {

          case _: ClassDef =>
            liftedDefs(sym) = new mutable.ListBuffer

            if (sym.owner.isTerm) {
              liftLocalTo(sym, sym.enclosingTopLevelClass.owner)
            }

          case _: DefDef =>
            val owner = sym.owner

            if (owner.isTerm) {
              liftLocalTo(sym, sym.enclosingTopLevelClass)
              sym setFlag (PrivateLocal | FINAL)
            } else if (sym.isPrimaryConstructor && !owner.isTrait && isLocal(owner)) {
              // add a call edge from the constructor of a local non-trait class to
              // the class itself. This is done so that the constructor inherits
              // the free variables of the class.
              registerCalled(owner, sym)
            }

          case ref: Ident if sym.isTerm && !sym.isLabel && isLocal(sym) =>
            if (sym.isMethod) markCalled(sym, currentEnclosure)
            else markFree(sym, currentEnclosure)

            // shouldn't `ref` have been a `Select(This(sym.owner), sym.name)` then???
            if (sym.owner.isClass) markThisRef(sym.owner)

          case ref: Select if sym.isMethod && isLocal(sym) => markCalled(sym, currentEnclosure)
          case tree: This => markThisRef(tree.symbol)


          case _ =>
        }

        super.traverse(tree)

      }
    }

    /** Compute free variables map `fvs`.
     */
    private def computeFreeVars(): Unit = {
      (new CollectDependencies).traverse(unit.body)

      do {
        changedFreeVars = false
        for {
          (caller, callees) <- called; callee <- callees
          fvs <- free get callee; fv <- fvs
        } markFree(fv, caller)
      } while (changedFreeVars)
    }

    /**
      * The new name for a free variable *proxy*.
      *
      * If we simply renamed the free variables, we would transform:
      * {{{
      *   def closure(x: Int) = { () => x }
      * }}}
      *
      * To:
      * {{{
      *   def closure(x$1: Int) = new anonFun$1(this, x$1)
      *   class anonFun$1(outer$: Outer, x$1: Int) { def apply() => x$1 }
      * }}}
      *
      * This is fatally bad for named arguments (0e170e4b), extremely impolite to tools
      * reflecting on the method parameter names in the generated bytecode (SI-6028),
      * and needlessly bothersome to anyone using a debugger.
      *
      * Instead, we transform to:
      * {{{
      *   def closure(x: Int) = new anonFun$1(this, x)
      *   class anonFun$1(outer$: Outer, x$1: Int) { def apply() => x$1 }
      * }}}
      */
    private def freshName(sym: Symbol): TermName = {
      val originalName = sym.name
      def freshen(prefix: String): Name =
        if (originalName.isTypeName) unit.freshTypeName(prefix)
        else unit.freshTermName(prefix)

      val join = nme.NAME_JOIN_STRING
      val freshName =
        if (sym.isAnonymousFunction && sym.owner.isMethod) {
          freshen(sym.name + join + nme.ensureNonAnon(sym.owner.name.toString) + join)
        } else {
          val name = freshen(sym.name + join)
          // SI-5652 If the lifted symbol is accessed from an inner class, it will be made public. (where?)
          //         Generating a unique name, mangled with the enclosing full class name (including
          //         package - subclass might have the same name), avoids a VerifyError in the case
          //         that a sub-class happens to lifts out a method with the *same* name.
          if (originalName.isTermName && calledFromInner(sym))
            newTermNameCached(nme.ensureNonAnon(sym.enclClass.fullName('$')) + nme.EXPAND_SEPARATOR_STRING + name)
          else
            name
        }

      debuglog(s"fresh name $originalName => $freshName in ${sym.owner.fullLocationString}")
      freshName.toTermName
    }

    def renameNonFree() = {
      // TODO: can we avoid iterating over all free variables?
      val isFree = free.flatMap{ case (_, values) => values }.toSet

      needsLifting.filterNot(isFree).foreach { sym => sym setName freshName(sym) }
    }

    private def generateProxies(): Unit  = afterOwnPhase {
      for ((owner, freeValues) <- free.toIterator) {
        val newFlags = SYNTHETIC | (
          if (owner.isClass) PARAMACCESSOR | PrivateLocal
          else PARAM)

        proxyMap(owner) = {
          for (fv <- freeValues.toList) yield {
            val proxyName = proxyNameOfFree.getOrElseUpdate(fv, freshName(fv))
            debuglog(s"new proxy ${proxyName} in ${owner.fullLocationString}")
            val proxy = owner.newValue(proxyName, owner.pos, newFlags.toLong) setInfo fv.info
            if (owner.isClass) owner.info.decls enter proxy
            (fv, proxy)
          }
        }.toMap
      }
    }

    private def proxy(sym: Symbol) = {
      @tailrec def searchIn(enclosure: Symbol): Symbol = {
        if (enclosure eq NoSymbol) throw new IllegalArgumentException(s"Could not find proxy for ${sym.defString} in ${sym.ownerChain} (currentOwner= $currentOwner )")
        debuglog(s"searching for $sym (${sym.owner}) in $enclosure ${enclosure.owner}")

        proxyMap get enclosure match {
          case Some(pmap) =>
            pmap get sym match {
              case Some(proxy) => return proxy
              case none =>
            }
          case none =>
        }
        searchIn(enclosure.owner.skipConstructor) // in the constructor, the proxies are ctor arguments that are stored in fields
      }

      debuglog(s"proxy ${sym.debugLocationString} from ${currentOwner.debugLocationString} has logical enclosure ${sym.enclosure.debugLocationString}")

      if (isSameOwnerEnclosure(sym)) sym else searchIn(currentOwner)
    }

    private def memberRef(sym: Symbol): Tree = {
      val clazz = sym.owner.enclClass
      // println(s"memberRef from $currentClass to $sym in $clazz (currentClass=$currentClass)")
      def prematureSelfReference(): Tree = {
        val what =
          if (clazz.isStaticOwner) clazz.fullLocationString
          else s"the unconstructed `this` of ${clazz.fullLocationString}"
        val msg = s"Implementation restriction: access of ${sym.fullLocationString} from ${currentClass.fullLocationString}, would require illegal premature access to $what"
        reporter.error(curTree.pos, msg)
        EmptyTree
      }
      def qual =
        if (clazz == currentClass) gen.mkAttributedThis(clazz)
        else {
          sym resetFlag (LOCAL | PRIVATE)
          if (isUnderConstruction(clazz)) prematureSelfReference()
          else if (clazz.isStaticOwner) gen.mkAttributedQualifier(clazz.thisType)
          else outerValue match {
            case EmptyTree => prematureSelfReference()
            case o         => outerPath(o, currentClass.outerClass, clazz)
          }
        }

      qual match {
        case EmptyTree => EmptyTree
        case qual      => Select(qual, sym) setType sym.tpe
      }
    }

    private def proxyRef(sym: Symbol) = {
      val psym = proxy(sym)
      if (psym.isLocalToBlock) gen.mkAttributedIdent(psym)
      else memberRef(psym)
    }

    // for ctor, preserve outer param's position as first arg (or why do we need the special case?),
    // otherwise, make lambda body a viable target MethodHandle for LambdaMetafactory (SI-8359)
//    private def addFree[A](sym: Symbol)(free: List[A], original: List[A]): List[A] = if (sym.isConstructor) original ++ free else free ++ original
    private def addFree[A](sym: Symbol)(free: List[A], original: List[A]): List[A] = {
      val prependFree = (
        !sym.isConstructor // this condition is redundant for now. It will be needed if we remove the second condition in 2.12.x
        && (settings.Ydelambdafy.value == "method" && sym.isDelambdafyTarget) // SI-8359 Makes the lambda body a viable as the target MethodHandle for a call to LambdaMetafactory
        )
      if (prependFree) free ::: original
      else original ::: free
    }

    private def addFreeArgs(pos: Position, sym: Symbol, args: List[Tree]) =
      free get sym match {
        case Some(fvs) => addFree(sym)(fvs.toList.map(fv => atPos(pos)(proxyRef(fv))), args)
        case _ => args
      }

    def freeVars(sym: Symbol): List[Symbol] = free.getOrElse(sym, Nil).toList
    def proxyOf(sym: Symbol)(fv: Symbol)    = proxyMap.getOrElse(sym, Map.empty)(fv)
    def proxies(sym: Symbol): List[Symbol]  = freeVars(sym).map(proxyOf(sym))
    def proxyDefs(proxies: List[Symbol], pos: Position): List[ValDef] = proxies map (p => ValDef(p) setPos pos setType NoType)

    // @pre proxies.nonEmpty
    private def addFreeParams(tree: Tree, sym: Symbol, proxies: List[Symbol]): Tree =
      tree match {
        case DefDef(_, _, _, vparams :: _, _, _) =>
          val infoWithFreeArgs = MethodType(
            addFree(sym)(cloneSymbols(proxies).map(_.setFlag(PARAM)), sym.info.params),
            sym.info.resultType)

          // TODO: not sure we can avoid it, but we really shouldn't mutate info in a tree transform (that's InfoTransformer's job)
          sym.updateInfo(localTypeRefToLifted(infoWithFreeArgs))

          copyDefDef(tree)(vparamss = List(addFree(sym)(proxyDefs(proxies, tree.pos), vparams)))

        case ClassDef(_, _, _, _) =>
          deriveClassDef(tree)(impl => deriveTemplate(impl)(_ ::: proxyDefs(proxies, tree.pos)))

        case _ => tree
      }

    private def liftDef(tree: Tree): Tree = {
      val sym = tree.symbol
      val oldOwner = sym.owner
      if (sym.isMethod && isUnderConstruction(sym.owner.owner)) {
        // # bug 1909
        if (sym.isModule) {
          // Yes, it can be a module and a method, see comments on `isModuleNotMethod`!
          // TODO promote to an implementation restriction if we can reason that this *always* leads to VerifyError.
          // See neg/t1909-object.scala
          def msg = s"SI-1909 Unable to STATICally lift $sym, which is defined in the self- or super-constructor call of ${sym.owner.owner}. A VerifyError is likely."
          devWarning(tree.pos, msg)
        } else sym setFlag STATIC
      }

      sym.owner = liftedOwner(sym)
      if (sym.isMethod) sym setFlag LIFTED
      liftedDefs(sym.owner) += tree
      // TODO: this modifies the ClassInfotype of the enclosing class, which is associated with another phase (explicitouter).
      // This breaks type history: in a phase travel to before lambda lift, the ClassInfoType will contain lifted classes.
      sym.owner.info.decls enterUnique sym
      debuglog("lifted: " + sym + " from " + oldOwner + " to " + sym.owner)
      EmptyTree
    }

    private def postTransform(tree: Tree, isBoxedRef: Boolean = false): Tree = {
      val sym = tree.symbol
      tree match {
        case _: ClassDef | _: DefDef =>
          val withFreeParams = proxies(sym) match { case Nil => tree case proxies => addFreeParams(tree, sym, proxies) }

          if (needsLifting(sym)) liftDef(withFreeParams)
          else withFreeParams

        case ValDef(mods, name, tpt, rhs) =>
          if (sym.isCapturedVariable) {
            val tpt1 = TypeTree(sym.tpe) setPos tpt.pos

            val refTypeSym = sym.tpe.typeSymbol

            val factoryCall = typer.typedPos(rhs.pos) {
              rhs match {
                case EmptyTree =>
                  val zeroMSym   = refZeroMethod(refTypeSym)
                  gen.mkMethodCall(zeroMSym, Nil)
                case arg =>
                  val createMSym = refCreateMethod(refTypeSym)
                  gen.mkMethodCall(createMSym, arg :: Nil)
              }
            }

            treeCopy.ValDef(tree, mods, name, tpt1, factoryCall)
          } else tree
        case Return(Block(stats, value)) =>
          Block(stats, treeCopy.Return(tree, value)) setType tree.tpe setPos tree.pos
        case Return(expr) =>
          assert(sym == currentMethod, sym)
          tree
        case Apply(fn, args) =>
          treeCopy.Apply(tree, fn, addFreeArgs(tree.pos, sym, args))
        case Assign(Apply(TypeApply(sel @ Select(qual, _), _), List()), rhs) =>
          // eliminate casts introduced by selecting a captured variable field
          // on the lhs of an assignment.
          assert(sel.symbol == Object_asInstanceOf)
          treeCopy.Assign(tree, qual, rhs)
        case Ident(name) =>
          val refToCaptured =
            if (!sym.isTerm || sym.isLabel) tree
            else if (sym.isMethod) memberRef(sym)
            else if (sym.isLocalToBlock && !isSameOwnerEnclosure(sym)) proxyRef(sym)
            else tree

          val derefCaptured =
            if (!isBoxedRef && sym.isCapturedVariable) {
              val origTp = tree.tpe
              val elemTree = typer typed Select(refToCaptured setType sym.tpe, nme.elem)
              // TODO: more sane check whether we need to cast...
              val needsCast = elemTree.tpe.typeSymbol != origTp.typeSymbol
              if (needsCast) gen.mkAttributedCast(elemTree, origTp)
              else elemTree
            } else refToCaptured

          if (tree ne derefCaptured) atPos(tree.pos)(derefCaptured)
          else tree

        case Block(stats, expr0) =>
          val (lzyVals, rest) = stats partition {
            case stat: ValDef => stat.symbol.isLazy || stat.symbol.isModuleVar
            case _            => false
          }
          if (lzyVals.isEmpty) tree
          else treeCopy.Block(tree, lzyVals ::: rest, expr0)
        case _ =>
          tree
      }
    }

    private def preTransform(tree: Tree) = super.transform(tree) setType localTypeRefToLifted(tree.tpe)

    override def transform(tree: Tree): Tree = tree match {
      case Select(ReferenceToBoxed(idt), elem) if elem == nme.elem =>
        postTransform(preTransform(idt), isBoxedRef = false)
      case ReferenceToBoxed(idt) =>
        postTransform(preTransform(idt), isBoxedRef = true)
      case _ =>
        postTransform(preTransform(tree))
    }

    /** Transform statements and add lifted definitions to them. */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      def addLifted(stat: Tree): Tree = stat match {
        case ClassDef(_, _, _, _) =>
          val lifted = liftedDefs remove stat.symbol match {
            case Some(xs) => xs.toList reverseMap addLifted
            case _        => log("unexpectedly no lifted defs for " + stat.symbol) ; Nil
          }
          deriveClassDef(stat)(impl => deriveTemplate(impl)(_ ::: lifted))

        case DefDef(_, _, _, _, _, Block(Nil, expr)) if !stat.symbol.isConstructor =>
          deriveDefDef(stat)(_ => expr)
        case _ =>
          stat
      }
      super.transformStats(stats, exprOwner) map addLifted
    }

    override def transformUnit(unit: CompilationUnit) {
      computeFreeVars()
//      computeLiftedOwners()

      renameNonFree()

      generateProxies()

      afterOwnPhase {
        super.transformUnit(unit)
      }
      assert(liftedDefs.isEmpty, liftedDefs.keys mkString ", ")
    }
  } // class LambdaLifter
}
