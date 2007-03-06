/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Gilles Dubochet
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import symtab.Flags._
import scala.collection.immutable.ListMap
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.tools.nsc.util.{FreshNameCreator, TreeSet}

/** This abstract class ...
 *
 *  @author Gilles Dubochet
 *  @version 1.0
 */
abstract class LiftCode extends Transform {

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftcode"

  def newTransformer(unit: CompilationUnit): Transformer =
    new AddRefFields(unit)

  class AddRefFields(unit: CompilationUnit) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Apply(TypeApply(Select(x@Ident(_), nme.lift_), _), List(tree))
      if x.symbol == CodeModule =>
        typed(atPos(tree.pos)(codify(tree)))
      case _ =>
        super.transform(tree)
    }
  }

  case class FreeValue(tree: Tree) extends reflect.Tree

  class ReifyEnvironment extends HashMap[Symbol, reflect.Symbol] {
    var targets = new HashMap[String, Option[reflect.LabelSymbol]]()
    def addTarget(name: String, target: reflect.LabelSymbol): Unit =
      targets.update(name, Some(target))
    def getTarget(name: String): Option[reflect.LabelSymbol] =
      targets.get(name) match {
        case None =>
          targets.update(name, None)
          None
        //case Some(None) => None //bq:redundant
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

    def reify(tree: Tree): reflect.Tree = tree match {
      case Ident(_) =>
        val rsym = reify(tree.symbol);
        //Console.println("LiftCode: seen ident")
        if (rsym == reflect.NoSymbol) {
          //Console.println("  free = "+tree)
          FreeValue(tree)
        } else {
          //Console.println("  rsym = "+rsym)
          reflect.Ident(rsym)
        }
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

      case vd @ ValDef(mods, name, tpt, rhs) =>
        val rtpe = reify(vd.tpe) // will return null, currently?!
        val sym  = reflect.LocalValue(currentOwner, name.toString(), rtpe)
        env(vd.symbol) = sym // bq: despite Scala's scoping rules, this should work because references to vd.symbol were type checked.
        val rhs_ = reify(rhs)
        reflect.ValDef(sym, rhs_)

      case cd @ ClassDef(mods, name, tparams, self, impl) =>
        if(!tparams.isEmpty)
          throw new TypeError("cannot handle polymorphic ClassDef ("+name+"): " + tparams)
        val rsym = reify(cd.symbol)
        val rimp = reify(impl)
        val rtpe = reify(self.tpt.tpe) //todo: update
        reflect.ClassDef(rsym, rtpe, rimp.asInstanceOf[reflect.Template])

      case tmpl @ Template(parents, body) =>
        val rparents = for(val p <- parents) yield { reify(p.tpe) }
        reflect.Template(rparents, body.map(reify))

      case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        if(!tparams.isEmpty)
          throw new TypeError("cannot handle polymorphic DefDef ("+name+"): " + tparams)
        val rsym   = reify(dd.symbol)
        val rparss = vparamss map { x => x map (reify) }
        val rret   = reify(tpt.tpe)
        val rrhs   = reify(rhs)
        reflect.DefDef(rsym, rparss, rret, rrhs)

      case sp @ Super(qual: Name, mix: Name) =>
        val rsym = reify(sp.symbol)
        reflect.Super(rsym)

      case _ =>
        throw new TypeError("cannot reify tree ("+tree.getClass()+"): " + tree)
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
        else if (sym.owner.isTerm)
          reflect.NoSymbol
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

    var _log_reify_type_ = false

    def reify(tp: Type): reflect.Type = tp match {
      case ErrorType =>
        if (_log_reify_type_) Console.println("cannot handle ErrorType"); null
      case WildcardType =>
        if (_log_reify_type_) Console.println("cannot handle WildcardType"); null
      case NoType =>
        if (_log_reify_type_) Console.println("cannot handle NoType"); null
      case NoPrefix =>
        if (_log_reify_type_) Console.println("cannot handle NoPrefix"); null
      case ThisType(sym) =>
        if (_log_reify_type_) Console.println("ThisType ("+sym+")")
        val rsym = reify(sym)
        if (_log_reify_type_) Console.println("reified is "+rsym+" cannot handle ThisType "+tp); null
      case SingleType(pre, sym) =>
        if (_log_reify_type_) Console.println("cannot handle SingleType "+tp); null
      case ConstantType(value) =>
        if (_log_reify_type_) Console.println("cannot handle ConstantType("+value+")  "+tp); null
      case TypeRef(pre, sym, args) =>
        if (_log_reify_type_) Console.println("TypeRef! try to handle prefix")
        val rpre = reify(pre)
        if (_log_reify_type_) Console.println("cannot handle TypeRef("+pre+","+sym+","+args+") == "+tp+")"); null

      case TypeBounds(lo, hi) =>
        if (_log_reify_type_) Console.println("cannot handle TypeBounds "+tp); null
      case RefinedType(parents, defs) =>
        if (_log_reify_type_) Console.println("cannot handle RefinedType "+tp); null
      case ClassInfoType(parents, defs, clazz) =>
        if (_log_reify_type_) Console.println("cannot handle ClassInfoType "+tp); null
      case MethodType(paramtypes, result) =>
        if (_log_reify_type_) Console.println("cannot handle MethodType "+tp); null
      case PolyType(tparams, result) =>
        if (_log_reify_type_) Console.println("cannot handle PolyType  "+tp); null;
      case AnnotatedType(attribs, tp) =>
        reify(tp)
      case _ =>
        null
    }

  }

  type InjectEnvironment = ListMap[reflect.Symbol, Name]

  class Injector(env: InjectEnvironment, fresh: FreshNameCreator) {

    // todo replace className by caseName in CaseClass once we have switched to nsc.
    def className(value: AnyRef): String = value match {
      case _ :: _ => "scala.$colon$colon"
      case reflect.MethodType(_, _) =>
        if (value.isInstanceOf[reflect.ImplicitMethodType])
          "scala.reflect.ImplicitMethodType"
        else
          "scala.reflect.MethodType"
      case x:Product =>
        "scala.reflect."+x.productPrefix //caseName
      //case _ => // bq:unreachable code
      //  ""
    }

    def objectName(value: Any): String = value match {
      case Nil                => "scala.Nil"
      case reflect.NoSymbol   => "scala.reflect.NoSymbol"
      case reflect.RootSymbol => "scala.reflect.RootSymbol"
      case reflect.NoPrefix   => "scala.reflect.NoPrefix"
      case reflect.NoType     => "scala.reflect.NoType"
      case _ => ""
    }

    def inject(value: Any): Tree = {
	  def treatProduct(c:Product) = {
        val name = objectName(c);
        if (name.length() != 0) gen.mkAttributedRef(definitions.getModule(name))
        else {
          val name = className(c);
          if (name.length() == 0) throw new Error("don't know how to inject " + value);
          val injectedArgs = new ListBuffer[Tree];
          for (val i <- 0 until c.arity /*caseArity*/)
            injectedArgs += inject(c.element(i));
          New(Ident(definitions.getClass(name)), List(injectedArgs.toList))
        }
	  }
	  value match {
      case FreeValue(tree) =>
        New(Ident(definitions.getClass("scala.reflect.Literal")), List(List(tree)))
      case ()           => Literal(Constant(()))
      case x: String    => Literal(Constant(x))
      case x: Boolean   => Literal(Constant(x))
      case x: Byte      => Literal(Constant(x))
      case x: Short     => Literal(Constant(x))
      case x: Char      => Literal(Constant(x))
      case x: Int       => Literal(Constant(x))
      case x: Long      => Literal(Constant(x))
      case x: Float     => Literal(Constant(x))
      case x: Double    => Literal(Constant(x))
      case c: Product   => treatProduct(c)
      case null =>
        gen.mkAttributedRef(definitions.getModule("scala.reflect.NoType"))
      case _ =>
        throw new Error("don't know how to inject " + value)
    }
   }
  }

  def reify(tree: Tree): reflect.Tree =
    new Reifier(new ReifyEnvironment(), reflect.NoSymbol).reify(tree)

  def inject(code: reflect.Tree): Tree =
    new Injector(ListMap.empty, new FreshNameCreator).inject(code)

  def codify (tree: Tree): Tree =
    New(TypeTree(appliedType(definitions.CodeClass.typeConstructor,
                             List(tree.tpe))),
        List(List(inject(reify(tree)))))

}

// case EmptyTree =>
// case LiftPoint(tree) =>
// case PackageDef(name, stats) =>
// case ClassDef(mods, name, tparams, self, impl) =>
// case ValDef(mods, name, tpt, rhs) =>
// case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
// case AbsTypeDef(mods, name, lo, hi) =>
// case AliasTypeDef(mods, name, tparams, rhs) =>
// case LabelDef(name, params, rhs) =>
// case Template(parents, body) =>
// case Block(stats, expr) =>
// case ArrayValue(elemtpt, trees) =>
// case Assign(lhs, rhs) =>
// case If(cond, thenp, elsep) =>
// case Match(selector, cases) =>
// case Return(expr) =>
// case Try(block, catches, finalizer) =>
// case Throw(expr) =>
// case New(tpt) =>
// case Typed(expr, tpt) =>
// case TypeApply(fun, args) =>
// case Apply(fun, args) =>
// case Super(qual, mix) =>
// case This(qual) =>
// case Select(qualifier, selector) =>
// case Ident(name) =>
// case Literal(value) =>
// case TypeTree() =>
// /* Pattern matching */
// case CaseDef(pat, guard, body) =>
// case Sequence(trees) =>
// case Alternative(trees) =>
// case Star(elem) =>
// case Bind(name, body) =>
