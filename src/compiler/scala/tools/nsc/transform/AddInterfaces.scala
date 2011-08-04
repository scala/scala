/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import collection.mutable.ListBuffer

abstract class AddInterfaces extends InfoTransform {
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  /** The phase sets lateINTERFACE for non-interface traits that now
   *  become interfaces. It sets lateDEFERRED for formerly concrete
   *  methods in such traits.
   */
  override def phaseNewFlags: Long = lateDEFERRED | lateINTERFACE

  /** Type reference after erasure; defined in Erasure.
   */
  def erasedTypeRef(sym: Symbol): Type

  /** Erasure calculation; defined in Erasure.
   */
  def erasure(sym: Symbol, tpe: Type): Type

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
      // !!! Shouldn't the following code be equivalent to leaving
      // out the "sym.info" call and starting with "sym.initialize.isMethod" ?
      // Because, it is not, which I found a little disturbing.  The compiler
      // fails to bootstrap with an error somewhere.
      sym.info    // initialize to set lateMETHOD flag if necessary

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
  private def needsImplMethod(sym: Symbol) = (
       sym.isMethod
    && isInterfaceMember(sym)
    && (!sym.hasFlag(DEFERRED | SUPERACCESSOR) || (sym hasFlag lateDEFERRED))
  )

  def implClassPhase = currentRun.erasurePhase.next

  /** Return the implementation class of a trait; create a new one of one does not yet exist */
  def implClass(iface: Symbol): Symbol = implClassMap.getOrElse(iface, {
    atPhase(implClassPhase) {
      val implName = nme.implClassName(iface.name)
      var impl = if (iface.owner.isClass) iface.owner.info.decl(implName) else NoSymbol
      if (impl != NoSymbol && settings.XO.value) {
        log("unlinking impl class " + impl)
        iface.owner.info.decls.unlink(impl)
        impl = NoSymbol
      }
      if (impl == NoSymbol) {
        impl = iface.cloneSymbolImpl(iface.owner)
        impl.name = implName
        impl.sourceFile = iface.sourceFile
        if (iface.owner.isClass)
          iface.owner.info.decls enter impl
      }
      if (currentRun.compiles(iface)) currentRun.symSource(impl) = iface.sourceFile
      impl setPos iface.pos
      impl.flags = iface.flags & ~(INTERFACE | lateINTERFACE) | IMPLCLASS
      impl setInfo new LazyImplClassType(iface)
      implClassMap(iface) = impl
      debuglog("generating impl class " + impl + " in " + iface.owner)//debug
      impl
    }
  })

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
  private class LazyImplClassType(iface: Symbol) extends LazyType {
    /** Compute the decls of implementation class implClass,
     *  given the decls ifaceDecls of its interface.
     */
    private def implDecls(implClass: Symbol, ifaceDecls: Scope): Scope = {
      val decls = new Scope
      if ((ifaceDecls lookup nme.MIXIN_CONSTRUCTOR) == NoSymbol)
        decls enter (implClass.newMethod(implClass.pos, nme.MIXIN_CONSTRUCTOR)
                     setInfo MethodType(List(), UnitClass.tpe))
      for (sym <- ifaceDecls.iterator) {
        if (isInterfaceMember(sym)) {
          if (needsImplMethod(sym)) {
            val impl = sym.cloneSymbol(implClass).resetFlag(lateDEFERRED)
            if (currentRun.compiles(implClass)) implMethodMap(sym) = impl
            decls enter impl
            sym setFlag lateDEFERRED
          }
        } else {
          sym.owner = implClass
          // note: OK to destructively modify the owner here,
          // because symbol will not be accessible from outside the sourcefile.
          // mixin constructors are corrected separately; see TermSymbol.owner
          decls enter sym
        }
      }
      decls
    }

    override def complete(sym: Symbol) {
      /** If `tp` refers to a non-interface trait, return a
       *  reference to its implementation class. Otherwise return `tp`.
       */
      def mixinToImplClass(tp: Type): Type = erasure(sym,
        tp match { //@MATN: no normalize needed (comes after erasure)
          case TypeRef(pre, sym, args) if sym.needsImplClass =>
            typeRef(pre, implClass(sym), args)
          case _ =>
            tp
        }
      )
      def implType(tp: Type): Type = tp match {
        case ClassInfoType(parents, decls, _) =>
          assert(phase == implClassPhase)
          ClassInfoType(
            ObjectClass.tpe +: (parents.tail map mixinToImplClass filter (_.typeSymbol != ObjectClass)) :+ iface.tpe,
            implDecls(sym, decls),
            sym
          )
        case PolyType(_, restpe) =>
          implType(restpe)
      }
      sym setInfo implType(atPhase(currentRun.erasurePhase)(iface.info))
    }

    override def load(clazz: Symbol) { complete(clazz) }
  }

  def transformMixinInfo(tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      if (clazz.needsImplClass) {
        clazz setFlag lateINTERFACE
        implClass(clazz) // generate an impl class
      }
      val parents1 = parents match {
        case Nil      => Nil
        case hd :: tl =>
          assert(!hd.typeSymbol.isTrait, clazz)
          if (clazz.isTrait) erasedTypeRef(ObjectClass) :: tl
          else parents
      }
      val decls1 = decls filter (sym =>
        if (clazz.isInterface) isInterfaceMember(sym)
        else (!sym.isType || sym.isClass))

      //if (!clazz.isPackageClass) System.out.println("Decls of "+clazz+" after explicitOuter = " + decls1);//DEBUG
      //if ((parents1 eq parents) && (decls1 eq decls)) tp
      //else
      ClassInfoType(parents1, decls1, clazz)
    case _ =>
      tp
  }

// Tree transformation --------------------------------------------------------------

  private class ChangeOwnerAndReturnTraverser(oldowner: Symbol, newowner: Symbol)
          extends ChangeOwnerTraverser(oldowner, newowner) {
    override def traverse(tree: Tree) {
      tree match {
        case Return(expr) =>
          if (tree.symbol == oldowner) tree.symbol = newowner
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private def ifaceMemberDef(tree: Tree): Tree =
    if (!tree.isDef || !isInterfaceMember(tree.symbol)) EmptyTree
    else if (needsImplMethod(tree.symbol)) DefDef(tree.symbol, EmptyTree)
    else tree

  private def ifaceTemplate(templ: Template): Template =
    treeCopy.Template(templ, templ.parents, emptyValDef, templ.body map ifaceMemberDef)

  private def implMethodDef(tree: Tree, ifaceMethod: Symbol): Tree =
    implMethodMap.get(ifaceMethod) match {
      case Some(implMethod) =>
        tree.symbol = implMethod
        new ChangeOwnerAndReturnTraverser(ifaceMethod, implMethod)(tree)
      case None =>
        abort("implMethod missing for " + ifaceMethod)
    }

  private def implMemberDef(tree: Tree): Tree =
    if (!tree.isDef || !isInterfaceMember(tree.symbol)) tree
    else if (needsImplMethod(tree.symbol)) implMethodDef(tree, tree.symbol)
    else EmptyTree

  /** Add mixin constructor definition
   *    def $init$(): Unit = ()
   *  to `stats` unless there is already one.
   */
  private def addMixinConstructorDef(clazz: Symbol, stats: List[Tree]): List[Tree] =
    if (treeInfo.firstConstructor(stats) != EmptyTree) stats
    else DefDef(clazz.primaryConstructor, Block(List(), Literal(Constant()))) :: stats

  private def implTemplate(clazz: Symbol, templ: Template): Template = atPos(templ.pos) {
    val templ1 = atPos(templ.pos) {
      Template(templ.parents, emptyValDef,
               addMixinConstructorDef(clazz, templ.body map implMemberDef))
        .setSymbol(clazz.newLocalDummy(templ.pos))
    }
    new ChangeOwnerTraverser(templ.symbol.owner, clazz)(
      new ChangeOwnerTraverser(templ.symbol, templ1.symbol)(templ1))
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
           if mc.hasFlag(lateINTERFACE) && mc != ScalaObjectClass)
      yield mixinConstructorCall(implClass(mc))
    }
    (tree: @unchecked) match {
      case Block(stats, expr) =>
        // needs `hasSymbol` check because `supercall` could be a block (named / default args)
        val (presuper, supercall :: rest) = stats span (t => t.hasSymbolWhich(_ hasFlag PRESUPER))
        // assert(supercall.symbol.isClassConstructor, supercall)
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
        case ClassDef(mods, name, tparams, impl) if (sym.needsImplClass) =>
          implClass(sym).initialize // to force lateDEFERRED flags
          treeCopy.ClassDef(tree, mods | INTERFACE, name, tparams, ifaceTemplate(impl))
        case DefDef(mods, name, tparams, vparamss, tpt, rhs)
        if (sym.isClassConstructor && sym.isPrimaryConstructor && sym.owner != ArrayClass) =>
          treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                      addMixinConstructorCalls(rhs, sym.owner)) // (3)
        case Template(parents, self, body) =>
          val parents1 = sym.owner.info.parents map (t => TypeTree(t) setPos tree.pos)
          treeCopy.Template(tree, parents1, emptyValDef, body)
        case This(_) =>
          if (sym.needsImplClass) {
            val impl = implClass(sym)
            var owner = currentOwner
            while (owner != sym && owner != impl) owner = owner.owner;
            if (owner == impl) This(impl) setPos tree.pos
            else tree
          } else tree
/* !!!
        case Super(qual, mix) =>
          val mix1 = mix
            if (mix == tpnme.EMPTY) mix
            else {
              val ps = atPhase(currentRun.erasurePhase) {
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
/*
    val ensureNoEscapes = new TypeTraverser {
      def ensureNoEscape(sym: Symbol) {
        if (sym.hasFlag(PRIVATE)) {
          var o = currentOwner;
          while (o != NoSymbol && o != sym.owner && !o.isLocal && !o.hasFlag(PRIVATE))
          o = o.owner
          if (o == sym.owner) sym.makeNotPrivate(base);
        }
      }
      def traverse(t: Type): TypeTraverser = {
        t match {
          case TypeRef(qual, sym, args) =>
            ensureNoEscape(sym)
            mapOver(t)
          case ClassInfoType(parents, decls, clazz) =>
            parents foreach { p => traverse; () }
            traverse(t.typeOfThis)
          case _ =>
            mapOver(t)
        }
        this
      }
    }

*/
