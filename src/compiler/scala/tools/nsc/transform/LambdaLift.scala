/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import util.TreeSet
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.tools.nsc.util.{Position, NoPosition}

abstract class LambdaLift extends InfoTransform {
  import global._
  import definitions._
  import typer.{typed, typedOperator}

  /** the following two members override abstract members in Transform */
  val phaseName: String = "lambdalift"

  private val lifted = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) =>
        if (pre == NoPrefix && sym.isClass && !sym.isPackageClass) {
          assert(args.isEmpty);
          typeRef(apply(sym.owner.enclClass.thisType), sym, args)
        } else mapOver(tp)
      case ClassInfoType(parents, decls, clazz) =>
        val parents1 = List.mapConserve(parents)(this)
        if (parents1 eq parents) tp
        else ClassInfoType(parents1, decls, clazz)
      case _ =>
        mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type): Type = lifted(tp)

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new LambdaLifter(unit)

  class LambdaLifter(unit: CompilationUnit) extends explicitOuter.OuterPathTransformer(unit) {

    /** A map storing free variables of functions and classes */
    private val free = new HashMap[Symbol, SymSet]

    /** A map storing the free variable proxies of functions and classes */
    private val proxies = new HashMap[Symbol, List[Symbol]]

    /** A hashtable storing calls between functions */
    private val called = new HashMap[Symbol, SymSet]

    /** The set of symbols that need to be renamed. */
    private val renamable = newSymSet

    /** A flag to indicate whether new free variables have been found */
    private var changedFreeVars: Boolean = _

    /** Buffers for lifted out classes and methods */
    private val liftedDefs = new HashMap[Symbol, ListBuffer[Tree]]

    private type SymSet = TreeSet[Symbol]

    private def newSymSet = new TreeSet[Symbol]((x, y) => x.isLess(y))

    private def symSet(f: HashMap[Symbol, SymSet], sym: Symbol): SymSet = f.get(sym) match {
      case Some(ss) => ss
      case None => val ss = newSymSet; f(sym) = ss; ss
    }

    private def outer(sym: Symbol): Symbol =
      if (sym.isConstructor) sym.owner.owner else sym.owner;

    /** The method or class which logically encloses the current symbol.
     *  If the symbol is defined in the initialization part of a template
     *  this is the template's primary constructor, otherwise it is
     *  the physically enclosing method or class.
     *
     *  Example 1:
     *
     *  def f() { val x = { def g() = ...; g() } }
     *
     *  In this case the owner chain of `g' is `x', followed by `f' and
     *  enclMethOrClass(`g') == `f'.
     *
     *  Example 2:
     *
     *  class C {
     *    def <init> = { ... }
     *    val x = { def g() = ...; g() } }
     *  }
     *
     *  In this case the owner chain of `g' is `x', followed by `C' but
     *  enclMethOrClass(`g') is the primary constructor symbol `<init>'
     *  (or, for traits: `$init') of `C'.
     *
     */
    private def enclMethOrClass(sym: Symbol): Symbol = {
      def localToConstr(sym: Symbol) =
        if (sym.isLocalDummy) sym.owner.primaryConstructor else sym;
      var encl = localToConstr(sym)
      while (!encl.isMethod && !encl.isClass) {
        encl = localToConstr(outer(encl));
      }
      encl
    }

    /** Mark symbol `sym' as being free in `owner', unless `sym'
     *  is defined in `owner' or there is a class between `owner's owner
     *  and the owner of `sym'.
     *  Return `true' if there is no class between `owner' and
     *  the owner of sym.
     *  pre: sym.isLocal, (owner.isMethod || owner.isClass)
     *
     *  The idea of `markFree' is illustrated with an example:
     *
     *  def f(x: int) = {
     *    class C {
     *      class D {
     *        val y = x
     *      }
     *    }
     *  }
     *
     *  In this case `x' is free in the primary constructor of class `C'.
     *  but it is not free in `D', because after lambda lift the code would be transformed
     *  as follows:
     *
     *  def f(x$0: int) {
     *    class C(x$0: int) {
     *      val x$1 = x$0
     *      class D {
     *        val y = outer.x$1
     *      }
     *    }
     *  }
     */
    private def markFree(sym: Symbol, owner: Symbol): Boolean = {
      if (settings.debug.value)
        log("mark " + sym + " of " + sym.owner + " free in " + owner)
      if (owner == enclMethOrClass(sym.owner)) true
      else if (owner.isPackageClass || !markFree(sym, enclMethOrClass(outer(owner)))) false
      else {
        val ss = symSet(free, owner)
        if (!(ss contains sym)) {
          ss addEntry sym
          renamable addEntry sym
          changedFreeVars = true
          if (settings.debug.value) log("" + sym + " is free in " + owner);
          if (sym.isVariable && !(sym hasFlag CAPTURED)) {
            sym setFlag CAPTURED
            val symClass = sym.tpe.typeSymbol;
            atPhase(phase.next) {
              sym updateInfo (
                if (isValueClass(symClass)) refClass(symClass).tpe else ObjectRefClass.tpe)
            }
          }
        }
        !owner.isClass
      }
    }

    private def markCalled(sym: Symbol, owner: Symbol) {
      if (settings.debug.value)
        log("mark " + sym + " of " + sym.owner + " called by " + owner);
      symSet(called, owner) addEntry sym
    }
/*
      if (owner == enclMethOrClass(sym.owner)) true
      else if (owner.isPackageClass || !markCalled(sym, enclMethOrClass(outer(owner)))) false
      else {
        val ss = symSet(called, owner);
        if (!(ss contains sym)) {
          ss addEntry sym;
          if (settings.debug.value) log("" + sym + " is called by " + owner);
        }
        !owner.isClass
      }
    }
*/
    def freeVars(sym: Symbol): Iterator[Symbol] = free.get(sym) match {
      case Some(ss) => ss.iterator
      case None => Iterator.empty
    }

    /** The traverse function */
    private val freeVarTraverser = new Traverser {
      override def traverse(tree: Tree) {
       try { //debug
        val sym = tree.symbol;
        tree match {
          case ClassDef(_, _, _, _) =>
            liftedDefs(tree.symbol) = new ListBuffer
            if (sym.isLocal) renamable addEntry sym
          case DefDef(_, _, _, _, _, _) =>
            if (sym.isLocal) {
              renamable addEntry sym
              sym setFlag (PRIVATE | LOCAL | FINAL)
            } else if (sym.isPrimaryConstructor) {
              symSet(called, sym) addEntry sym.owner
            }
          case Ident(name) =>
            if (sym == NoSymbol) {
              assert(name == nme.WILDCARD)
            } else if (sym.isLocal) {
              val owner = enclMethOrClass(currentOwner)
              if (sym.isTerm && !sym.isMethod) markFree(sym, owner)
              else if (sym.isMethod) markCalled(sym, owner)
                //symSet(called, owner) addEntry sym
            }
          case Select(_, _) =>
            if (sym.isConstructor && sym.owner.isLocal) {
              val owner = enclMethOrClass(currentOwner);
              markCalled(sym, owner) //symSet(called, owner) addEntry sym
            }
          case _ =>
        }
        super.traverse(tree)
       } catch {//debug
         case ex: Throwable =>
           Console.println("exception when traversing " + tree)
           throw ex
       }
      }
    }

    /** Compute free variables map `fvs'.
     *  Also assign unique names to all
     *  value/variable/let that are free in some function or class, and to
     *  all class/function symbols that are owned by some function.
     */
    private def computeFreeVars {
      freeVarTraverser.traverse(unit.body)

      do {
        changedFreeVars = false
        for (caller <- called.keys;
             callee <- called(caller).iterator;
             fv <- freeVars(callee))
          markFree(fv, caller)
      } while (changedFreeVars);

      for (sym <- renamable.iterator) {
        val base =
          if (sym.isAnonymousFunction && sym.owner.isMethod)
            sym.name.toString() + "$" + sym.owner.name.toString() + "$"
          else sym.name.toString() + "$"
        val fresh = unit.fresh.newName(sym.pos, base)
        sym.name = if (sym.name.isTypeName) fresh.toTypeName else fresh
        if (settings.debug.value) log("renamed: " + sym.name)
      }

      atPhase(phase.next) {
        for (owner <- free.keys) {
          if (settings.debug.value)
            log("free(" + owner + owner.locationString + ") = " + free(owner).iterator.toList);
          proxies(owner) =
            for (fv <- free(owner).iterator.toList) yield {
              val proxy = owner.newValue(owner.pos, fv.name)
                .setFlag(if (owner.isClass) PARAMACCESSOR | PRIVATE | LOCAL else PARAM)
                .setFlag(SYNTHETIC)
                .setInfo(fv.info);
              if (owner.isClass) owner.info.decls enter proxy;
              proxy
            }
        }
      }
    }

    private def proxy(sym: Symbol) = {
      def searchIn(owner: Symbol): Symbol = {
        if (settings.debug.value)
          log("searching for " + sym + "(" + sym.owner + ") in " + owner +
              " " + enclMethOrClass(owner));//debug
        proxies.get(enclMethOrClass(owner)) match {
          case Some(ps) =>
            ps filter (p => p.name == sym.name) match {
              case List(p) => p
              case List() => searchIn(outer(owner))
            }
          case None => searchIn(outer(owner))
        }
      }
      if (settings.debug.value)
        log("proxy " + sym + " in " + sym.owner + " from " + currentOwner.ownerChain +
            " " + enclMethOrClass(sym.owner));//debug
      if (enclMethOrClass(sym.owner) == enclMethOrClass(currentOwner)) sym
      else searchIn(currentOwner)
    }

    private def memberRef(sym: Symbol) = {
      val clazz = sym.owner.enclClass
      //Console.println("memberRef from "+currentClass+" to "+sym+" in "+clazz)
      val qual = if (clazz == currentClass) gen.mkAttributedThis(clazz)
                 else {
                   sym resetFlag(LOCAL | PRIVATE)
                   if (clazz.isStaticOwner) gen.mkAttributedQualifier(clazz.thisType)
                   else outerPath(outerValue, currentClass.outerClass, clazz)
                 }
      Select(qual, sym) setType sym.tpe
    }

    private def proxyRef(sym: Symbol) = {
      val psym = proxy(sym)
      if (psym.isLocal) gen.mkAttributedIdent(psym) else memberRef(psym)
    }

    private def addFreeArgs(pos: Position, sym: Symbol, args: List[Tree]) = {
      def freeArg(fv: Symbol) = atPos(pos)(proxyRef(fv))
      val fvs = freeVars(sym).toList
      if (fvs.isEmpty) args else args ::: (fvs map freeArg)
    }

    private def addFreeParams(tree: Tree, sym: Symbol): Tree = proxies.get(sym) match {
      case Some(ps) =>
        val freeParams = ps map (p => ValDef(p) setPos tree.pos setType NoType);
        tree match {
          case DefDef(mods, name, tparams, List(vparams), tpt, rhs) =>
            sym.updateInfo(
              lifted(MethodType(sym.info.paramTypes ::: (ps map (_.tpe)), sym.info.resultType)));
            copy.DefDef(tree, mods, name, tparams, List(vparams ::: freeParams), tpt, rhs)
          case ClassDef(mods, name, tparams, impl @ Template(parents, self, body)) =>
            copy.ClassDef(tree, mods, name, tparams,
                          copy.Template(impl, parents, self, body ::: freeParams))
        }
      case None =>
        tree
    }

    private def liftDef(tree: Tree): Tree = {
      val sym = tree.symbol
      sym.owner = sym.owner.enclClass
      if (sym.isClass) sym.owner = sym.owner.toInterface
      if (sym.isMethod) sym setFlag LIFTED
      liftedDefs(sym.owner) += tree
      sym.owner.info.decls enterUnique sym
      if (settings.debug.value) log("lifted: " + sym + sym.locationString)
      EmptyTree
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case ClassDef(_, _, _, _) =>
          val tree1 = addFreeParams(tree, sym)
          if (sym.isLocal) liftDef(tree1) else tree1
        case DefDef(_, _, _, _, _, _) =>
          val tree1 = addFreeParams(tree, sym)
          if (sym.isLocal) liftDef(tree1) else tree1
        case ValDef(mods, name, tpt, rhs) =>
          if (sym.isCapturedVariable) {
            val tpt1 = TypeTree(sym.tpe) setPos tpt.pos
            val rhs1 =
              atPos(rhs.pos) {
                typed {
                  Apply(Select(New(TypeTree(sym.tpe)), nme.CONSTRUCTOR), List(rhs))
                }
              }
            copy.ValDef(tree, mods, name, tpt1, rhs1)
          } else tree
        case Return(Block(stats, value)) =>
          Block(stats, copy.Return(tree, value)) setType tree.tpe setPos tree.pos
        case Return(expr) =>
          assert(sym == currentMethod, sym)
          tree
        case Apply(fn, args) =>
          copy.Apply(tree, fn, addFreeArgs(tree.pos, sym, args))
        case Assign(Apply(TypeApply(sel @ Select(qual, _), _), List()), rhs) =>
          // eliminate casts introduced by selecting a captured variable field
          // on the lhs of an assignment.
          assert(sel.symbol == Object_asInstanceOf)
          copy.Assign(tree, qual, rhs)
        case Ident(name) =>
          val tree1 =
            if (sym != NoSymbol && sym.isTerm && !sym.isLabel)
              if (sym.isMethod)
                atPos(tree.pos)(memberRef(sym))
              else if (sym.isLocal && enclMethOrClass(sym.owner) != enclMethOrClass(currentOwner))
                atPos(tree.pos)(proxyRef(sym))
              else tree
            else tree;
          if (sym.isCapturedVariable)
            atPos(tree.pos) {
              val tp = tree.tpe
              val elemTree = typed { Select(tree1 setType sym.tpe, nme.elem) }
              if (elemTree.tpe.typeSymbol != tp.typeSymbol) gen.mkAttributedCast(elemTree, tp) else elemTree
            }
          else tree1
        case _ =>
          tree
      }
    }

    override def transform(tree: Tree): Tree =
      postTransform(super.transform(tree) setType lifted(tree.tpe))

    /** Transform statements and add lifted definitions to them. */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      def addLifted(stat: Tree): Tree = stat match {
        case ClassDef(mods, name, tparams, impl @ Template(parents, self, body)) =>
          val lifted = liftedDefs(stat.symbol).toList map addLifted
          val result = copy.ClassDef(
            stat, mods, name, tparams, copy.Template(impl, parents, self, body ::: lifted))
          liftedDefs -= stat.symbol
          result
        case DefDef(mods, name, tp, vp, tpt, Block(Nil, expr))
        if !stat.symbol.isConstructor =>
          copy.DefDef(stat, mods, name, tp, vp, tpt, expr)
        case _ =>
          stat
      }
      super.transformStats(stats, exprOwner) map addLifted
    }

    override def transformUnit(unit: CompilationUnit) {
      computeFreeVars
      atPhase(phase.next)(super.transformUnit(unit))
      assert(liftedDefs.size == 0, liftedDefs.keys.toList)
    }
  } // class LambdaLifter

}
