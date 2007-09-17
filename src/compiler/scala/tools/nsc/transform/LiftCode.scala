/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
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

/** Translate expressions of the form reflect.Code.lift(exp)
 *  to the lifted "reflect trees" representation of exp.
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
      case Apply(lift, List(tree))
      if lift.symbol == Code_lift =>
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
      targets.elements.map(_._2).forall {
        case Some(_) => true
        case None => false
      }
    override def update(sym: Symbol, rsym: reflect.Symbol) =
      super.update(sym,rsym)
  }


  class Reifier(env: ReifyEnvironment, currentOwner: reflect.Symbol)
  extends SymbolReifier
  {
    val symbols: global.type = global


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

      case _ : StubTree => reflect.Literal(0)
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
        reflect.TypeApply(reify(fun), args map (_.tpe) map reify)

      case Function(vparams, body) =>
        var env1 = env
        for (vparam <- vparams) {
          val local = reflect.LocalValue(
            currentOwner, vparam.symbol.name.toString(), reify(vparam.symbol.tpe));
          env1.update(vparam.symbol, local);
        }
        reflect.Function(vparams map (_.symbol) map env1,
                         new Reifier(env1, currentOwner).reify(body))
      case tree@This(_) if tree.symbol.isModule =>
        // there is no reflect node for a module's this, so
        // represent it as a selection of the module
	reify(typed(
	  Select(This(tree.symbol.owner), tree.symbol.name)))
      case This(_) =>
        reflect.This(reify(tree.symbol))
      case Block(stats, expr) =>
        reflect.Block(stats.map(reify), reify(expr))
      case New(clazz) =>
        val reifiedClass = reify(clazz)
        reflect.New(reifiedClass)
      case Typed(t, _) =>
        reify(t)
      case If(cond, thenp, elsep) =>
        reflect.If(reify(cond), reify(thenp), reify(elsep))
      case Assign(lhs, rhs) =>
        reflect.Assign(reify(lhs), reify(rhs))

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

      case cd @ ClassDef(mods, name, tparams, impl) =>
        if(!tparams.isEmpty)
          throw new TypeError("cannot handle polymorphic ClassDef ("+name+"): " + tparams)
        val rsym = reify(cd.symbol)
        val rimp = reify(impl)
        val rtpe = reify(impl.self.tpt.tpe) //todo: update
        reflect.ClassDef(rsym, rtpe, rimp.asInstanceOf[reflect.Template])

      case tmpl @ Template(parents, self, body) =>
        val rparents = for (p <- parents) yield { reify(p.tpe) }
        //todo: add self to reified templates
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

    override def reify(sym: Symbol): reflect.Symbol =
      env.get(sym) match {
	case Some(rsym) =>
	  rsym
	case None =>
	  super.reify(sym)
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
      def treatProduct(c: Product) = {
        val name = objectName(c)
        if (name.length() != 0)
          gen.mkAttributedRef(definitions.getModule(name))
        else {
          val name = className(c)
          if (name.length() == 0) throw new Error("don't know how to inject " + value)
          val injectedArgs = new ListBuffer[Tree]
          for (i <- 0 until c.productArity)
            injectedArgs += inject(c.productElement(i))
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
  } // Injector

  def reify(tree: Tree): reflect.Tree =
    new Reifier(new ReifyEnvironment(), reflect.NoSymbol).reify(tree)

  def inject(code: reflect.Tree): Tree =
    new Injector(ListMap.empty, new FreshNameCreator.Default).inject(code)

  def codify (tree: Tree): Tree =
    New(TypeTree(appliedType(definitions.CodeClass.typeConstructor,
                             List(tree.tpe))),
        List(List(inject(reify(tree)))))

}

// case EmptyTree =>
// case LiftPoint(tree) =>
// case PackageDef(name, stats) =>
// case ClassDef(mods, name, tparams, impl) =>
// case ValDef(mods, name, tpt, rhs) =>
// case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
// case TypeDef(mods, name, tparams, rhs) =>
// case LabelDef(name, params, rhs) =>
// case Template(parents, self, body) =>
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
