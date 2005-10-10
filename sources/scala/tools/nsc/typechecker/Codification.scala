/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import scala.collection.immutable.ListMap;
import scala.tools.nsc.util.{ListBuffer, FreshNameCreator};

[_trait_] abstract class Codification: Analyzer {

  import global._;

  def codify(tree: Tree): Tree =
    New(TypeTree(appliedType(definitions.TypedCodeClass.typeConstructor, List(tree.tpe))),
        List(List(inject(reify(tree)))));

  case class FreeValue(tree: Tree) extends reflect.Code;

  type ReifyEnvironment = ListMap[Symbol, reflect.Symbol];

  class Reifier(env: ReifyEnvironment, currentOwner: reflect.Symbol) {

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
      case Apply(fun, args) =>
        reflect.Apply(reify(fun), args map reify)
      case TypeApply(fun, args) =>
        reflect.TypeApply(reify(fun), args map (.tpe) map reify)
      case Function(vparams, body) =>
        var env1 = env;
        for (val vparam <- vparams) {
          val local = reflect.LocalValue(
            currentOwner, vparam.symbol.name.toString(), reify(vparam.symbol.tpe));
          env1 = env1.update(vparam.symbol, local);
        }
        reflect.Function(vparams map (.symbol) map env1,
                         new Reifier(env1, currentOwner).reify(body))
      case _ =>
        throw new TypeError("cannot reify tree: " + tree)
    }

    def reify(sym: Symbol): reflect.Symbol = env.get(sym) match {
      case Some(rsym) =>
        rsym
      case None =>
        reify(sym.owner) match {
          case reflect.NoSymbol =>
            reflect.NoSymbol;
          case reflect.Class(ownername) =>
            val fullname = ownername + "." + sym.name;
            if (sym.isClass) reflect.Class(fullname)
            else if (sym.isType) reflect.TypeField(fullname, reify(sym.info))
            else if (sym.isMethod) reflect.Method(fullname, reify(sym.info))
            else reflect.Field(fullname, reify(sym.info))
          case _ =>
            reflect.NoSymbol
        }
    }

    def reify(tp: Type): reflect.Type = tp match {
      case NoPrefix =>
        reflect.NoPrefix
      case NoType =>
        reflect.NoType
      case TypeRef(pre, sym, args) =>
        val tp = reflect.TypeIdent(reify(pre), reify(sym));
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
      case _ =>
        throw new TypeError("cannot reify type: " + tp)
    }
  }

  type InjectEnvironment = ListMap[reflect.Symbol, Name];

  class Injector(env: InjectEnvironment, fresh: FreshNameCreator) {

    // todo replace className by caseName in CaseClass once we have switched to nsc.
    def className(value: CaseClass): String = value match {
      case reflect.Ident(_) => "Ident"
      case reflect.Select(_, _) => "Select"
      case reflect.Literal(_) => "Literal"
      case reflect.Apply(_, _) => "Apply"
      case reflect.TypeApply(_, _) => "TypeApply"
      case reflect.Function(_, _) => "Function"
      case reflect.Class(_) => "Class"
      case reflect.Method(_, _) => "Method"
      case reflect.Field(_, _) => "Field"
      case reflect.TypeIdent(_, _) => "TypeIdent"
      case reflect.SingleType(_, _) => "SingleType"
      case reflect.ThisType(_) => "ThisType"
      case reflect.AppliedType(_, _) => "AppliedType"
      case reflect.TypeBounds(_, _) => "TypeBounds"
      case reflect.MethodType(_, _) =>
        if (value.isInstanceOf[reflect.ImplicitMethodType]) "ImplicitMethodType" else "MethodType"
      case _ =>
        ""
    }

    def objectName(value: Any): String = value match {
      case reflect.NoSymbol => "NoSymbol"
      case reflect.RootSymbol => "RootSymbol"
      case reflect.NoPrefix => "NoPrefix"
      case reflect.NoType => "NoType"
      case _ => ""
    }

    def injectType(name: String): Tree = TypeTree(definitions.getClass("scala.reflect." + name).tpe);

    def inject(value: Any): Tree = value match {
      case FreeValue(tree) =>
        tree
      case reflect.Function(params, body) =>
        var env1 = env;
        val vdefs = for (val param <- params) yield {
          val lname = newTermName(fresh.newName());
          env1 = env1.update(param, lname);
          ValDef(0, lname, injectType("LocalValue"),
                 New(injectType("LocalValue"),
                     List(List(inject(param.owner), inject(param.name), inject(param.tpe)))))
        }
        Block(vdefs, new Injector(env1, fresh).inject(body))
      case rsym: reflect.LocalSymbol =>
        Ident(env(rsym))
      case s: String =>
        Literal(Constant(s))
      case c: CaseClass =>
        val name = className(c);
        if (name.length == 0) throw new Error("don't know how to inject " + value);
        val injectedArgs = new ListBuffer[Tree];
        for (val i <- Iterator.range(0, c.caseArity))
          injectedArgs += inject(c.caseElement(i));
        New(injectType(name), List(injectedArgs.toList))
      case _ =>
        val name = objectName(value);
        if (name.length == 0) throw new Error("don't know how to inject " + value);
        gen.mkRef(definitions.getModule("scala.reflect." + name))
    }
  }

  def reify(tree: Tree): reflect.Code =
    new Reifier(ListMap.Empty, reflect.NoSymbol).reify(tree);

  def inject(code: reflect.Code): Tree =
    new Injector(ListMap.Empty, new FreshNameCreator).inject(code);
}
