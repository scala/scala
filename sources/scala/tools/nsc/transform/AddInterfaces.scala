/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

import symtab._;
import Flags._;
import util.ListBuffer;
import collection.mutable.HashMap;

abstract class AddInterfaces extends InfoTransform {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import posAssigner.atPos;         // for filling in tree positions

  override def phaseNewFlags: long = lateDEFERRED | lateINTERFACE;

// Type transformation

  def erasedTypeRef(sym: Symbol): Type;

  private val implClassMap = new HashMap[Symbol, Symbol];
  private val implMethodMap = new HashMap[Symbol, Symbol];

  private def implClassName(sym: Symbol): Name =
    newTermName(sym.name.toString() + nme.IMPL_CLASS_SUFFIX);

  private def needsImplClass(sym: Symbol): boolean =
    sym.isTrait && (!(sym hasFlag INTERFACE) || (sym hasFlag lateINTERFACE)) && !sym.isImplClass;

  private def needsImplMethod(sym: Symbol): boolean =
    sym.isMethod && isInterfaceMember(sym) &&
    (!(sym hasFlag DEFERRED) || (sym hasFlag lateDEFERRED));

  private def isInterfaceMember(sym: Symbol): boolean =
    sym.isType ||
    sym.isMethod && !(sym hasFlag (PRIVATE | BRIDGE | LABEL)) && !sym.isConstructor;

  private def implClass(iface: Symbol): Symbol = implClassMap.get(iface) match {
    case Some(c) => c
    case None =>
      atPhase(erasurePhase) {
        val impl = iface.cloneSymbolImpl(iface.owner)
	  setFlag (iface.flags & ~(INTERFACE | lateINTERFACE))
	  setInfo new LazyImplClassType(iface);
        impl.name = implClassName(iface);
        //includeInTypeOfThis(iface, impl);
        //includeInTypeOfThis(impl, impl);
        implClassMap(iface) = impl;
        if (settings.debug.value) log("generating impl class " + impl);
        impl
      }
  }

  private class LazyImplClassType(iface: Symbol) extends LazyType {

    def implDecls(implClass: Symbol, ifaceDecls: Scope): Scope = {
      val decls = new Scope();
      for (val sym <- ifaceDecls.elements) {
        if (isInterfaceMember(sym)) {
          if (needsImplMethod(sym)) {
	    val impl = sym.cloneSymbol(implClass).setInfo(sym.info);
	    if (!impl.isExternal) implMethodMap(sym) = impl;
	    decls enter impl;
	    sym setFlag lateDEFERRED
          }
        } else {
	  sym.owner = implClass;
          decls enter sym;
	}
      }
      decls
    }

    override def complete(sym: Symbol): unit = {
      def implType(tp: Type): Type = tp match {
	case ClassInfoType(parents, decls, _) =>
	  ClassInfoType(traitToImplClass(parents) ::: List(iface.tpe), implDecls(sym, decls), sym)
	case PolyType(tparams, restpe) =>
	  PolyType(tparams, implType(restpe))
      }
      sym.setInfo(atPhase(erasurePhase)(implType(iface.info)));
    }

    override def load(clazz: Symbol): unit = complete(clazz)
  }

  private def traitToImplClass(tp: Type): Type = tp match {
    case TypeRef(pre, sym, args) if (needsImplClass(sym)) =>
      typeRef(pre, implClass(sym), args)
    case _ =>
      tp
  }

  private def traitToImplClass(parents: List[Type]): List[Type] =
    parents.head :: (parents.tail map traitToImplClass);

  private def addImplClasses(decls: Scope): Scope = {
    for (val sym <- decls.elements)
      if (needsImplClass(sym)) decls enter implClass(sym);
    decls
  }

  def transformTraitInfo(tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      if (needsImplClass(clazz)) clazz setFlag lateINTERFACE;
      var parents1 =
        if (parents.isEmpty) List()
        else {
          assert(!parents.head.symbol.isTrait);
          if (clazz hasFlag INTERFACE) erasedTypeRef(ObjectClass) :: parents.tail
          else if (clazz.isImplClass || clazz == ArrayClass) parents
	  else traitToImplClass(parents)
        }
      val decls1 = addImplClasses(
        if (clazz hasFlag INTERFACE) new Scope(decls.toList filter isInterfaceMember)
        else new Scope(decls.toList));
      ClassInfoType(parents1, decls1, clazz)
    case _ =>
      tp
  }

// Tree transformation --------------------------------------------------------------

  private class ChangeOwnerAndReturnTraverser(oldowner: Symbol, newowner: Symbol)
          extends ChangeOwnerTraverser(oldowner, newowner) {
    override def traverse(tree: Tree): unit = {
      tree match {
        case Return(expr) =>
          if (tree.symbol == oldowner) tree.symbol = newowner;
        case _ =>
      }
      super.traverse(tree)
    }
  }

  private def ifaceMemberDef(tree: Tree): Tree =
    if (!tree.isDef || !isInterfaceMember(tree.symbol)) EmptyTree
    else if (needsImplMethod(tree.symbol)) DefDef(tree.symbol, vparamss => EmptyTree)
    else tree;

  private def ifaceTemplate(templ: Template): Template =
    copy.Template(templ, templ.parents, templ.body map ifaceMemberDef);

  private def implMethodDef(tree: Tree, ifaceMethod: Symbol): Tree =
    implMethodMap.get(ifaceMethod) match {
      case Some(implMethod) =>
        tree.symbol = implMethod;
        new ChangeOwnerAndReturnTraverser(ifaceMethod, implMethod).traverse(tree);
        tree
      case None =>
        throw new Error("implMethod missing for " + ifaceMethod + " " + ifaceMethod.isExternal)
    }

  private def implMemberDef(tree: Tree): Tree =
    if (!tree.isDef || !isInterfaceMember(tree.symbol)) tree
    else if (needsImplMethod(tree.symbol)) implMethodDef(tree, tree.symbol)
    else EmptyTree;

  private def implTemplate(clazz: Symbol, templ: Template): Template = atPos(templ.pos){
    val templ1 = Template(templ.parents, templ.body map implMemberDef)
      setPos templ.pos
      setSymbol clazz.newLocalDummy(templ.pos);
    new ChangeOwnerTraverser(templ.symbol.owner, clazz).traverse(templ1);
    new ChangeOwnerTraverser(templ.symbol, templ1.symbol).traverse(templ1);
    templ1
  }

  def implClassDefs(trees: List[Tree]): List[Tree] = {
    val buf = new ListBuffer[Tree];
    for (val tree <- trees)
      tree match {
	case ClassDef(_, _, _, _, impl) =>
	  if (needsImplClass(tree.symbol))
            buf += {
              val clazz = implClass(tree.symbol).initialize;
              ClassDef(clazz, implTemplate(clazz, impl))
            }
	case _ =>
      }
    buf.toList
  }

  protected val traitTransformer = new Transformer {
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      super.transformStats(stats, exprOwner) :::
      super.transformStats(implClassDefs(stats), exprOwner);
    override def transform(tree: Tree): Tree = {
      val tree1 = tree match {
	case ClassDef(mods, name, tparams, tpt, impl) =>
	  if (needsImplClass(tree.symbol)) {
            implClass(tree.symbol).initialize; // to force lateDEFERRED flags
	    copy.ClassDef(tree, mods | INTERFACE, name, tparams, tpt, ifaceTemplate(impl))
          }
	  else tree
	case Template(parents, body) =>
          val parents1 = tree.symbol.owner.info.parents map (t => TypeTree(t) setPos tree.pos);
          copy.Template(tree, parents1, body)
	case This(_) =>
	  if (needsImplClass(tree.symbol)) {
            val impl = implClass(tree.symbol);
            var owner = currentOwner;
            while (owner != tree.symbol && owner != impl) owner = owner.owner;
            if (owner == impl) This(impl) setPos tree.pos
            else tree
          } else tree
	case Super(qual, mix) =>
	  val mix1 =
	    if (mix == nme.EMPTY.toTypeName) mix
	    else {
	      val ps = atPhase(erasurePhase) {
                tree.symbol.info.parents dropWhile (p => p.symbol.name != mix)
              }
	      assert(!ps.isEmpty, tree);
	      if (needsImplClass(ps.head.symbol)) implClass(ps.head.symbol).name
	      else mix
	    }
	  if (needsImplClass(tree.symbol)) Super(implClass(tree.symbol), mix1) setPos tree.pos
	  else copy.Super(tree, qual, mix1)
	case _ =>
	  tree
      }
      super.transform(tree1)
    }
  }
}
/*
    val ensureNoEscapes = new TypeTraverser {
      def ensureNoEscape(sym: Symbol): unit = {
        if (sym.hasFlag(PRIVATE)) {
          var o = currentOwner;
          while (o != NoSymbol && o != sym.owner && !o.isLocal && !o.hasFlag(PRIVATE))
          o = o.owner;
          if (o == sym.owner) makeNotPrivate(sym);
        }
      }
      def traverse(t: Type): TypeTraverser = {
        t match {
          case TypeRef(qual, sym, args) =>
            ensureNoEscape(sym);
            mapOver(t);
          case ClassInfoType(parents, decls, clazz) =>
            parents foreach { p => traverse; () }
            traverse(t.typeOfThis);
          case _ =>
            mapOver(t)
        }
        this
      }
    }

*/
