/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.tools.nsc.util.ClassPath

abstract class AddInterfaces extends InfoTransform { self: Erasure =>
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  /** The phase sets lateINTERFACE for non-interface traits that now
   *  become interfaces. It sets lateDEFERRED for formerly concrete
   *  methods in such traits.
   */
  override def phaseNewFlags: Long = lateDEFERRED | lateINTERFACE

  /** A lazily constructed map that associates every non-interface trait with
   *  its implementation class.
   */
  private val implClassMap = perRunCaches.newMap[Symbol, Symbol]()

  /** A lazily constructed map that associates every concrete method in a non-interface
   *  trait that's currently compiled with its corresponding method in the trait's
   *  implementation class.
   */
  private val implMethodMap = perRunCaches.newMap[Symbol, Symbol]()

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
    implClassMap.clear()
    implMethodMap.clear()
    super.newPhase(prev)
  }

  /** Is given trait member symbol a member of the trait's interface
   *  after this transform is performed?
   */
  private def isInterfaceMember(sym: Symbol) = (
    sym.isType || {
      sym.info  // initialize to set lateMETHOD flag if necessary

      (     sym.isMethod
        && !sym.isLabel
        && !sym.isPrivate
        && (!(sym hasFlag BRIDGE) || sym.hasBridgeAnnotation) // count @bridge annotated classes as interface members
        && !sym.isConstructor
        && !sym.isImplOnly
      )
    }
  )

  /** Does symbol need an implementation method? */
  def needsImplMethod(sym: Symbol) = (
       sym.isMethod
    && isInterfaceMember(sym)
    && (!sym.hasFlag(DEFERRED | SUPERACCESSOR) || (sym hasFlag lateDEFERRED))
  )

  def implClassPhase = currentRun.erasurePhase.next

  private def newImplClass(iface: Symbol): Symbol = {
    val inClass   = iface.owner.isClass
    val implName  = tpnme.implClassName(iface.name)
    val implFlags = (iface.flags & ~(INTERFACE | lateINTERFACE)) | IMPLCLASS

    val impl0 = {
      if (!inClass) NoSymbol
      else {
        val typeInfo = iface.owner.info
        typeInfo.decl(implName) match {
          case NoSymbol => NoSymbol
          case implSym =>
            // Unlink a pre-existing symbol only if the implementation class is
            // visible on the compilation classpath. In general this is true under
            // -optimise and not otherwise, but the classpath can use arbitrary
            // logic so the classpath must be queried.
            // TODO this is not taken into account by flat classpath yet
            classPath match {
              case cp: ClassPath[_] if !cp.context.isValidName(implName + ".class") =>
                log(s"not unlinking $iface's existing implClass ${implSym.name} because it is not on the classpath.")
                implSym
              case _ =>
                typeInfo.decls unlink implSym
                NoSymbol
            }
        }
      }
    }

    val impl = impl0 orElse {
      val impl = iface.owner.newImplClass(implName, iface.pos, implFlags)
      if (iface.thisSym != iface) {
        impl.typeOfThis = iface.typeOfThis
        impl.thisSym setName iface.thisSym.name
      }
      impl.associatedFile = iface.sourceFile
      if (inClass)
        iface.owner.info.decls enter impl

      impl
    }
    if (currentRun compiles iface)
      currentRun.symSource(impl) = iface.sourceFile

    implClassMap(iface) = impl
    impl setInfo new LazyImplClassType(iface)
  }

  /** Return the implementation class of a trait; create a new one if one does not yet exist */
  def implClass(iface: Symbol): Symbol = {
    iface.info

    implClassMap.getOrElse(iface, enteringPhase(implClassPhase) {
      if (iface.implClass eq NoSymbol)
        debuglog(s"${iface.fullLocationString} has no implClass yet, creating it now.")
      else
        log(s"${iface.fullLocationString} impl class is ${iface.implClass.nameString}")

      newImplClass(iface)
    })
  }

  /** A lazy type to set the info of an implementation class
   *  The parents of an implementation class for trait iface are:
   *
   *  - superclass: Object
   *  - mixin classes: mixin classes of iface where every non-interface
   *  trait is mapped to its implementation class, followed by iface itself.
   *
   *  The declarations of a mixin class are:
   *  - for every interface member of iface: its implementation method, if one is needed
   *  - every former member of iface that is implementation only
   */
  private class LazyImplClassType(iface: Symbol) extends LazyType with FlagAgnosticCompleter {
    /** Compute the decls of implementation class implClass,
     *  given the decls ifaceDecls of its interface.
     */
    private def implDecls(implClass: Symbol, ifaceDecls: Scope): Scope = {
      debuglog("LazyImplClassType calculating decls for " + implClass)

      val decls = newScope
      if ((ifaceDecls lookup nme.MIXIN_CONSTRUCTOR) == NoSymbol) {
        log("Adding mixin constructor to " + implClass)

        decls enter (
          implClass.newMethod(nme.MIXIN_CONSTRUCTOR, implClass.pos)
            setInfo MethodType(Nil, UnitTpe)
        )
      }

      for (sym <- ifaceDecls) {
        if (isInterfaceMember(sym)) {
          if (needsImplMethod(sym)) {
            val clone = sym.cloneSymbol(implClass).resetFlag(lateDEFERRED)
            if (currentRun.compiles(implClass)) implMethodMap(sym) = clone
            decls enter clone
            sym setFlag lateDEFERRED
            if (!sym.isSpecialized)
              log(s"Cloned ${sym.name} from ${sym.owner} into implClass ${implClass.fullName}")
          }
        }
        else {
          log(s"Destructively modifying owner of $sym from ${sym.owner} to $implClass")
          sym.owner = implClass
          // note: OK to destructively modify the owner here,
          // because symbol will not be accessible from outside the sourcefile.
          // mixin constructors are corrected separately; see TermSymbol.owner
          decls enter sym
        }
      }

      decls
    }

    override def complete(implSym: Symbol) {
      debuglog("LazyImplClassType completing " + implSym)

      /* If `tp` refers to a non-interface trait, return a
       * reference to its implementation class. Otherwise return `tp`.
       */
      def mixinToImplClass(tp: Type): Type = AddInterfaces.this.erasure(implSym) {
        tp match { //@MATN: no normalize needed (comes after erasure)
          case TypeRef(pre, sym, _) if sym.needsImplClass =>
            typeRef(pre, implClass(sym), Nil)
          case _ =>
            tp
        }
      }
      def implType(tp: Type): Type = tp match {
        case ClassInfoType(parents, decls, _) =>
          assert(phase == implClassPhase, tp)
          // Impl class parents: Object first, matching interface last.
          val implParents = ObjectTpe +: (parents.tail map mixinToImplClass filter (_.typeSymbol != ObjectClass)) :+ iface.tpe
          ClassInfoType(implParents, implDecls(implSym, decls), implSym)
        case PolyType(_, restpe) =>
          implType(restpe)
      }
      implSym setInfo implType(enteringErasure(iface.info))
    }

    override def load(clazz: Symbol) { complete(clazz) }
  }

  def transformMixinInfo(tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) if clazz.isPackageClass || !clazz.isJavaDefined =>
      if (clazz.needsImplClass)
        implClass(clazz setFlag lateINTERFACE) // generate an impl class

      val parents1 = parents match {
        case Nil      => Nil
        case hd :: tl =>
          assert(!hd.typeSymbol.isTrait, clazz)
          if (clazz.isTrait) ObjectTpe :: tl
          else parents
      }
      val decls1 = scopeTransform(clazz)(
        decls filter (sym =>
          if (clazz.isInterface) isInterfaceMember(sym)
          else sym.isClass || sym.isTerm
        )
      )
      ClassInfoType(parents1, decls1, clazz)
    case _ =>
      tp
  }

// Tree transformation --------------------------------------------------------------

  private class ChangeOwnerAndReturnTraverser(oldowner: Symbol, newowner: Symbol)
          extends ChangeOwnerTraverser(oldowner, newowner) {
    override def traverse(tree: Tree) {
      tree match {
        case _: Return => change(tree.symbol)
        case _         =>
      }
      super.traverse(tree)
    }
  }

  private def createMemberDef(tree: Tree, isForInterface: Boolean)(create: Tree => Tree) = {
    val isInterfaceTree = tree.isDef && isInterfaceMember(tree.symbol)
    if (isInterfaceTree && needsImplMethod(tree.symbol))
      create(tree)
    else if (isInterfaceTree == isForInterface)
      tree
    else
      EmptyTree
  }
  private def implMemberDef(tree: Tree): Tree  = createMemberDef(tree, false)(implMethodDef)
  private def ifaceMemberDef(tree: Tree): Tree = createMemberDef(tree, true)(t => DefDef(t.symbol, EmptyTree))

  private def ifaceTemplate(templ: Template): Template =
    treeCopy.Template(templ, templ.parents, noSelfType, templ.body map ifaceMemberDef)

  /** Transforms the member tree containing the implementation
   *  into a member of the impl class.
   */
  private def implMethodDef(tree: Tree): Tree = {
    val impl = implMethodMap.getOrElse(tree.symbol, abort("implMethod missing for " + tree.symbol))

    val newTree = if (impl.isErroneous) tree else { // e.g. res/t687
      // SI-5167: Ensure that the tree that we are grafting refers the parameter symbols from the
      // new method symbol `impl`, rather than the symbols of the original method signature in
      // the trait. `tree setSymbol impl` does *not* suffice!
      val DefDef(_, _, _, vparamss, _, _) = tree
      val oldSyms = vparamss.flatten.map(_.symbol)
      val newSyms = impl.info.paramss.flatten
      assert(oldSyms.length == newSyms.length, (oldSyms, impl, impl.info))
      tree.substituteSymbols(oldSyms, newSyms)
    }
    new ChangeOwnerAndReturnTraverser(newTree.symbol, impl)(newTree setSymbol impl)
  }

  /** Add mixin constructor definition
   *    def $init$(): Unit = ()
   *  to `stats` unless there is already one.
   */
  private def addMixinConstructorDef(clazz: Symbol, stats: List[Tree]): List[Tree] =
    if (treeInfo.firstConstructor(stats) != EmptyTree) stats
    else DefDef(clazz.primaryConstructor, Block(List(), Literal(Constant(())))) :: stats

  private def implTemplate(clazz: Symbol, templ: Template): Template = atPos(templ.pos) {
    val templ1 = (
      Template(templ.parents, noSelfType, addMixinConstructorDef(clazz, templ.body map implMemberDef))
        setSymbol clazz.newLocalDummy(templ.pos)
    )
    templ1.changeOwner(templ.symbol.owner -> clazz, templ.symbol -> templ1.symbol)
    templ1
  }

  def implClassDefs(trees: List[Tree]): List[Tree] = {
    trees collect {
      case cd: ClassDef if cd.symbol.needsImplClass =>
        val clazz = implClass(cd.symbol).initialize
        ClassDef(clazz, implTemplate(clazz, cd.impl))
    }
  }

  /** Add calls to supermixin constructors
   *    `super[mix].$init$()`
   *  to tree, which is assumed to be the body of a constructor of class clazz.
   */
  private def addMixinConstructorCalls(tree: Tree, clazz: Symbol): Tree = {
    def mixinConstructorCall(impl: Symbol): Tree = atPos(tree.pos) {
      Apply(Select(This(clazz), impl.primaryConstructor), List())
    }
    val mixinConstructorCalls: List[Tree] = {
      for (mc <- clazz.mixinClasses.reverse
           if mc.hasFlag(lateINTERFACE))
      yield mixinConstructorCall(implClass(mc))
    }
    tree match {
      case Block(Nil, expr) =>
        // AnyVal constructor - have to provide a real body so the
        // jvm doesn't throw a VerifyError. But we can't add the
        // body until now, because the typer knows that Any has no
        // constructor and won't accept a call to super.init.
        assert((clazz isSubClass AnyValClass) || clazz.info.parents.isEmpty, clazz)
        Block(List(Apply(gen.mkSuperInitCall, Nil)), expr)

      case Block(stats, expr) =>
        // needs `hasSymbolField` check because `supercall` could be a block (named / default args)
        val (presuper, supercall :: rest) = stats span (t => t.hasSymbolWhich(_ hasFlag PRESUPER))
        treeCopy.Block(tree, presuper ::: (supercall :: mixinConstructorCalls ::: rest), expr)
    }
  }

  protected val mixinTransformer = new Transformer {
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      (super.transformStats(stats, exprOwner) :::
       super.transformStats(implClassDefs(stats), exprOwner))
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      val tree1 = tree match {
        case ClassDef(mods, _, _, impl) if sym.needsImplClass =>
          implClass(sym).initialize // to force lateDEFERRED flags
          copyClassDef(tree)(mods = mods | INTERFACE, impl = ifaceTemplate(impl))
        case DefDef(_,_,_,_,_,_) if sym.isClassConstructor && sym.isPrimaryConstructor && sym.owner != ArrayClass =>
          deriveDefDef(tree)(addMixinConstructorCalls(_, sym.owner)) // (3)
        case Template(parents, self, body) =>
          val parents1 = sym.owner.info.parents map (t => TypeTree(t) setPos tree.pos)
          treeCopy.Template(tree, parents1, noSelfType, body)
        case This(_) if sym.needsImplClass =>
          val impl = implClass(sym)
          var owner = currentOwner
          while (owner != sym && owner != impl) owner = owner.owner;
          if (owner == impl) This(impl) setPos tree.pos
          else tree
        //TODO what about this commented out code?
/* !!!
        case Super(qual, mix) =>
          val mix1 = mix
            if (mix == tpnme.EMPTY) mix
            else {
              val ps = enteringErasure {
                sym.info.parents dropWhile (p => p.symbol.name != mix)
              }
              assert(!ps.isEmpty, tree);
              if (ps.head.symbol.needsImplClass) implClass(ps.head.symbol).name
              else mix
            }
          if (sym.needsImplClass) Super(implClass(sym), mix1) setPos tree.pos
          else treeCopy.Super(tree, qual, mix1)
*/
        case _ =>
          tree
      }
      super.transform(tree1)
    }
  }
}
