/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import scala.collection.mutable.HashMap;
import scala.collection.immutable.ListMap;
import scala.collection.mutable.ListBuffer;
import scala.tools.nsc.util.FreshNameCreator;

mixin class Codification requires Analyzer {

  import global._;

  case class FreeValue(tree: Tree) extends reflect.Code;

  class ReifyEnvironment extends HashMap[Symbol, reflect.Symbol] {
    var targets = new HashMap[String, Option[reflect.LabelSymbol]]()
    def addTarget(name: String, target: reflect.LabelSymbol): Unit =
      targets.update(name, Some(target))
    def getTarget(name: String): Option[reflect.LabelSymbol] =
      targets.get(name) match {
        case None =>
          targets.update(name, None)
          None
        case Some(None) => None
        case Some(tgt) => tgt
      }
    def hasAllTargets: Boolean =
      targets.elements.map(._2).forall {
        case Some(_) => true
        case None => false
      }
    override def update(sym: Symbol, rsym: reflect.Symbol) =
      super.update(sym,rsym)
  }

  class Reifier(env: ReifyEnvironment, currentOwner: reflect.Symbol) {

    def testCompletion: Boolean = env.hasAllTargets

    def reify(tree: Tree): reflect.Code = tree match {
      case Ident(_) =>
        val rsym = reify(tree.symbol);
        if (rsym == reflect.NoSymbol) FreeValue(tree)
        else reflect.Ident(rsym)
      case Select(qual, _) =>
        val rsym = reify(tree.symbol);
        if (rsym == reflect.NoSymbol) throw new TypeError("cannot reify symbol: " + tree.symbol)
        else reflect.Select(reify(qual), reify(tree.symbol))
      case Literal(constant) =>
        reflect.Literal(constant.value)
      case Apply(name, args) if name.toString().startsWith("label$") =>
        env.getTarget(name.toString()) match {
          case None => throw new TypeError("cannot reify tree (no forward jumps allowed): " + tree)
          case Some(label) => reflect.Goto(label)
        }
      case Apply(fun, args) =>
        reflect.Apply(reify(fun), args map reify)
      case TypeApply(fun, args) =>
        reflect.TypeApply(reify(fun), args map (.tpe) map reify)
      case Function(vparams, body) =>
        var env1 = env;
        for (val vparam <- vparams) {
          val local = reflect.LocalValue(
            currentOwner, vparam.symbol.name.toString(), reify(vparam.symbol.tpe));
          env1.update(vparam.symbol, local);
        }
        reflect.Function(vparams map (.symbol) map env1,
                         new Reifier(env1, currentOwner).reify(body))
      case This(_) =>
        reflect.This(reify(tree.symbol))
      case Block(stats, expr) =>
        reflect.Block(stats.map(reify), reify(expr))
      case New(clazz) =>
        val reifiedClass = reify(clazz)
        reflect.New(reifiedClass)
      case Typed(t, _) => reify(t)
      case If(cond, thenp, elsep) => reflect.If(reify(cond), reify(thenp), reify(elsep))
      case Assign(lhs, rhs) => reflect.Assign(reify(lhs), reify(rhs))
      case LabelDef(name, Nil, body) =>
        val sym = new reflect.LabelSymbol(name.toString())
        env.addTarget(name.toString(), sym)
        val res = reflect.Target(sym, reify(body))
        res
      case _ =>
        throw new TypeError("cannot reify tree: " + tree)
    }

    private def mkGlobalSymbol(fullname: String, sym: Symbol): reflect.Symbol =
      if (sym.isClass) reflect.Class(fullname)
      else if (sym.isType) reflect.TypeField(fullname, reify(sym.info))
      else if (sym.isMethod) reflect.Method(fullname, reify(sym.info))
      else reflect.Field(fullname, reify(sym.info));

    def reify(sym: Symbol): reflect.Symbol = env.get(sym) match {
      case Some(rsym) =>
        rsym
      case None =>
        if (sym.isRoot || sym.isRootPackage || sym.isEmptyPackageClass || sym.isEmptyPackage)
          reflect.RootSymbol
        else reify(sym.owner) match {
          case reflect.NoSymbol =>
            reflect.NoSymbol;
          case reflect.RootSymbol =>
            mkGlobalSymbol(sym.name.toString(), sym)
          case reflect.Class(ownername) =>
            mkGlobalSymbol(ownername + "." + sym.name, sym)
          case _ =>
            reflect.NoSymbol
        }
    }

    def reify(tp: Type): reflect.Type = null /*tp match {
      case NoPrefix =>
        reflect.NoPrefix
      case NoType =>
        reflect.NoType
      case TypeRef(pre, sym, args) =>
        val tp = if (sym.owner.isPackageClass) reflect.NamedType(sym.fullNameString);
                 else reflect.PrefixedType(reify(pre), reify(sym));
        if (args.isEmpty) tp else reflect.AppliedType(tp, args map reify)
      case SingleType(pre, sym) =>
        reflect.SingleType(reify(pre), reify(sym))
      case ThisType(clazz) =>
        reflect.ThisType(reify(clazz))
      case TypeBounds(lo, hi) =>
        reflect.TypeBounds(reify(lo), reify(hi))
      case MethodType(formals, restp) =>
        val formals1 = formals map reify;
        val restp1 = reify(restp);
        if (tp.isInstanceOf[ImplicitMethodType]) new reflect.ImplicitMethodType(formals1, restp1)
        else reflect.MethodType(formals1, restp1)
      //case PolyType(typeParams, ClassInfoType(parents, decls, symbol)) => reflect.PolyType(typeParams map reify, Nil, resultType)
      //case PolyType(typeParams, SingleType(pre, sym)) => reflect.PolyType(typeParams map reify, Nil, resultType)
      case PolyType(typeParams, MethodType(paramsList, resultType)) =>
        System.err.println("poly polyyyy");
        reflect.PolyType(Nil, Nil , reify(resultType)) //typeParams map mkTypeBounds
      case _ =>
        throw new TypeError("cannot reify type: " + tp)
    }*/
  }

  type InjectEnvironment = ListMap[reflect.Symbol, Name];

  class Injector(env: InjectEnvironment, fresh: FreshNameCreator) {

    // todo replace className by caseName in CaseClass once we have switched to nsc.
    def className(value: CaseClass): String = value match {
      case _ :: _ => "scala.$colon$colon"
      case reflect.Ident(_) => "scala.reflect.Ident"
      case reflect.Select(_, _) => "scala.reflect.Select"
      case reflect.Literal(_) => "scala.reflect.Literal"
      case reflect.Apply(_, _) => "scala.reflect.Apply"
      case reflect.TypeApply(_, _) => "scala.reflect.TypeApply"
      case reflect.Function(_, _) => "scala.reflect.Function"
      case reflect.Class(_) => "scala.reflect.Class"
      case reflect.Method(_, _) => "scala.reflect.Method"
      case reflect.Field(_, _) => "scala.reflect.Field"
      case reflect.TypeField(_, _) => "scala.reflect.TypeField"
      case reflect.LocalValue(_, _, _) => "scala.reflect.LocalValue"
      case reflect.LocalMethod(_, _, _) => "scala.reflect.LocalMethod"
      case reflect.LabelSymbol(_) => "scala.reflect.LabelSymbol"
      case reflect.This(_) => "scala.reflect.This"
      case reflect.Block(_,_) => "scala.reflect.Block"
      case reflect.New(_) => "scala.reflect.New"
      case reflect.If(_,_,_) => "scala.reflect.If"
      case reflect.Assign(_,_) => "scala.reflect.Assign"
      case reflect.Target(_,_) => "scala.reflect.Target"
      case reflect.Goto(_) => "scala.reflect.Goto"
      case reflect.NamedType(_) => "scala.reflect.NamedType"
      case reflect.PrefixedType(_, _) => "scala.reflect.PrefixedType"
      case reflect.SingleType(_, _) => "scala.reflect.SingleType"
      case reflect.ThisType(_) => "scala.reflect.ThisType"
      case reflect.AppliedType(_, _) => "scala.reflect.AppliedType"
      case reflect.TypeBounds(_, _) => "scala.reflect.TypeBounds"
      case reflect.MethodType(_, _) =>
        if (value.isInstanceOf[reflect.ImplicitMethodType]) "scala.reflect.ImplicitMethodType" else "scala.reflect.MethodType"
      case reflect.PolyType(_, _, _) => "scala.reflect.PolyType"
      case _ =>
        ""
    }

    def objectName(value: Any): String = value match {
      case Nil => "scala.Nil"
      case reflect.NoSymbol => "scala.reflect.NoSymbol"
      case reflect.RootSymbol => "scala.reflect.RootSymbol"
      case reflect.NoPrefix => "scala.reflect.NoPrefix"
      case reflect.NoType => "scala.reflect.NoType"
      case _ => ""
    }

    def injectType(name: String): Tree = TypeTree(definitions.getClass(name).initialize.tpe);

    def inject(value: Any): Tree = value match {
      case FreeValue(tree) =>
        tree
      case () => Literal(Constant(()))
      case x: String => Literal(Constant(x))
      case x: Boolean => Literal(Constant(x))
      case x: Byte => Literal(Constant(x))
      case x: Short => Literal(Constant(x))
      case x: Char => Literal(Constant(x))
      case x: Int => Literal(Constant(x))
      case x: Long => Literal(Constant(x))
      case x: Float => Literal(Constant(x))
      case x: Double => Literal(Constant(x))
      case c: CaseClass =>
        val name = objectName(c);
        if (name.length() != 0) gen.mkRef(definitions.getModule(name))
        else {
          val name = className(c);
          if (name.length() == 0) throw new Error("don't know how to inject " + value);
          val injectedArgs = new ListBuffer[Tree];
          for (val i <- Iterator.range(0, c.caseArity))
            injectedArgs += inject(c.caseElement(i));
          New(Ident(definitions.getClass(name)), List(injectedArgs.toList))
        }
      case null => gen.mkRef(definitions.getModule("scala.reflect.NoType"))
      case _ => throw new Error("don't know how to inject " + value);
    }
  }

  def reify(tree: Tree): reflect.Code =
    new Reifier(new ReifyEnvironment(), reflect.NoSymbol).reify(tree);

  def inject(code: reflect.Code): Tree =
    new Injector(ListMap.Empty, new FreshNameCreator).inject(code);

  /** returns
   *  < new TypedCode[T](tree1) >
   * where T = tree.tpe
   *       tree1 = inject(reify(tree))
   */
  def codify(tree: Tree): Tree = {
    val reified = reify(tree);
    if (settings.debug.value) log("reified = " + reified);
    val injected = inject(reified);
    if (settings.debug.value) log("injected = " + injected);
    New(TypeTree(appliedType(definitions.TypedCodeClass.typeConstructor, List(tree.tpe))),
        List(List(injected)));

  }
}
