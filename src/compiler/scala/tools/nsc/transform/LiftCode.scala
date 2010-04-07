/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Gilles Dubochet
 */
// $Id$

package scala.tools.nsc
package transform

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
abstract class LiftCode extends Transform with Reifiers {

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  val symbols: global.type = global

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
      case Nil                => "scala.collection.immutable.Nil"
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
          if (name.length() == 0) abort("don't know how to inject " + value)
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
          abort("don't know how to inject " + value)
      }
    }
  } // Injector


  def inject(code: reflect.Tree): Tree =
    new Injector(ListMap.empty, new FreshNameCreator.Default).inject(code)

  def codify (tree: Tree): Tree =
    New(TypeTree(appliedType(definitions.CodeClass.typeConstructor,
                             List(tree.tpe))),
        List(List(inject(reify(tree)))))

}

// case EmptyTree =>
// case LiftPoint(tree) =>
// case PackageDef(pid, stats) =>
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
// case Alternative(trees) =>
// case Star(elem) =>
// case Bind(name, body) =>
