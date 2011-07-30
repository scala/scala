/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.FreshNameCreator

/** Translate expressions of the form reflect.Code.lift(exp)
 *  to the lifted "reflect trees" representation of exp.
 *  Also: mutable variables that are accessed from a local function are wrapped in refs.
 *
 *  @author Gilles Dubochet
 *  @author Martin Odersky
 *  @version 2.10
 */
abstract class LiftCode extends Transform with TypingTransformers {

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  val symbols: global.type = global

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftcode"

  def newTransformer(unit: CompilationUnit): Transformer =
    new Lifter(unit)

  class Lifter(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transformUnit(unit: CompilationUnit) {
      freeMutableVars.clear()
        freeLocalsTraverser(unit.body)
      atPhase(phase.next) {
        super.transformUnit(unit)
      }
    }

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Apply(lift, List(tree)) if sym == Code_lift =>
          //printTypings = true //debug
          val result = transform(localTyper.typedPos(tree.pos)(codify(tree)))
          //printTypings = false //debug
          result
        case ValDef(mods, name, tpt, rhs) if (freeMutableVars(sym)) =>
          val tpt1 = TypeTree(sym.tpe) setPos tpt.pos
          /* Creating a constructor argument if one isn't present. */
          val constructorArg = rhs match {
            case EmptyTree => gen.mkZero(atPhase(phase.prev)(sym.tpe))
            case _ => transform(rhs)
          }
          val rhs1 = typer.typedPos(rhs.pos) {
            /*util.errtrace("lifted rhs for "+tree+" in "+unit)*/ (
            Apply(Select(New(TypeTree(sym.tpe)), nme.CONSTRUCTOR), List(constructorArg)))
          }
          sym resetFlag MUTABLE
          sym removeAnnotation VolatileAttr
          treeCopy.ValDef(tree, mods &~ MUTABLE, name, tpt1, rhs1)
        case Ident(name) if freeMutableVars(sym) =>
          localTyper.typedPos(tree.pos) {
            /*util.errtrace("lifting ")*/(Select(tree setType sym.tpe, nme.elem))
          }
        case _ =>
          super.transform(tree)
      }
    }
  }

  case class FreeValue(tree: Tree) extends Tree

  class Reifier() {
    import reflect.runtime.{Mirror => rm}

    private val boundVars: mutable.Set[Symbol] = mutable.Set()

    // todo replace className by caseName in CaseClass once we have switched to nsc.
    def className(value: AnyRef): String = value match {
      case _ :: _ => "scala.$colon$colon"
      case MethodType(_, _) => "scala.reflect.runtime.Mirror.MethodType"
      case x: Product => "scala.reflect.runtime.Mirror."+x.productPrefix
      case _ => ""
    }

    def objectName(value: Any): String = value match {
      case Nil        => "scala.collection.immutable.Nil"
      case EmptyTree  => "scala.reflect.runtime.Mirror.EmptyTree"
      case NoSymbol   => "scala.reflect.runtime.Mirror.NoSymbol"
      case definitions.RootClass => "scala.reflect.runtime.Mirror.definitions.RootClass"
      case NoPrefix   => "scala.reflect.runtime.Mirror.NoPrefix"
      case NoType     => "scala.reflect.runtime.Mirror.NoType"
      case _ => ""
    }

    def makeFree(tree: Tree): Tree =
      Apply(mkTerm("scala.reflect.runtime.Mirror.freeValue"), List(tree))

    def reify(value: Any): Tree = {
      //println("reifing "+value) //debug
      /*util.trace("reified "+value+" --> ")*/ {
      value match {
        case tree: DefTree =>
          boundVars += tree.symbol
          reify1(tree)
        case tree @ This(_) if !(boundVars contains tree.symbol) =>
          makeFree(tree)
        case tree @ Ident(_) if !(boundVars contains tree.symbol) =>
          makeFree(tree)
        case _ =>
          reify1(value)
      }}
    }

    def mkMember(name: String, mkName: String => Name): Tree = {
      val parts = name split "\\."
      val prefixParts = parts.init
      val lastName = mkName(parts.last)
      if (prefixParts.isEmpty) Ident(lastName)
      else {
        val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail) (Select(_, _))
        Select(prefixTree, lastName)
      }
    }

    def mkTerm(name: String) = mkMember(name, newTermName)
    def mkType(name: String) = mkMember(name, newTypeName)

    def reify1(value: Any): Tree = {
      def treatProduct(c: Product): Tree = {
        val name = objectName(c)
        if (!name.isEmpty)
          mkTerm(name)
        else {
          val name = className(c)
          if (name.isEmpty) abort("don't know how to inject " + value + " of class "+ value.getClass)
          val injectedArgs = new ListBuffer[Tree]
          for (i <- 0 until c.productArity)
            injectedArgs += reify(c.productElement(i))
          New(mkType(name), List(injectedArgs.toList))
        }
      }

      value match {
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
      case x: Map[_,_]  => Apply(mkTerm("scala.collection.immutable.Map.apply"), Nil)//!!! Maps come from Modifiers, should not be part of case class
      case x: TermName  => Apply(mkTerm("scala.reflect.runtime.Mirror.newTermName"), List(Literal(Constant(x.toString))))
      case x: TypeName  => Apply(mkTerm("scala.reflect.runtime.Mirror.newTypeName"), List(Literal(Constant(x.toString))))
      case c: Product   => treatProduct(c)
      case _ =>
        abort("don't know how to inject " + value+" of class "+ value.getClass)
      }
    }
  }

  def reify(tree: Tree): Tree =
    new Reifier().reify(tree)

  def codify (tree: Tree): Tree = {
    val targetType = definitions.CodeClass.primaryConstructor.info.paramTypes.head
    val arg = gen.mkAsInstanceOf(reify(tree), targetType, wrapInApply = false)
    New(TypeTree(appliedType(definitions.CodeClass.typeConstructor, List(tree.tpe))),
        List(List(arg)))
  }

  /** Set of mutable local variables that are free in some inner method. */
  private val freeMutableVars: mutable.Set[Symbol] = new mutable.HashSet

  /** PP: There is apparently some degree of overlap between the CAPTURED
   *  flag and the role being filled here.  I think this is how this was able
   *  to go for so long looking only at DefDef and Ident nodes, as bugs
   *  would only emerge under more complicated conditions such as #3855.
   *  I'll try to figure it all out, but if someone who already knows the
   *  whole story wants to fill it in, that too would be great.
   */
  private val freeLocalsTraverser = new Traverser {
    var currentMethod: Symbol = NoSymbol
    var maybeEscaping = false

    def withEscaping(body: => Unit) {
      val saved = maybeEscaping
      maybeEscaping = true
      try body
      finally maybeEscaping = saved
    }

    override def traverse(tree: Tree) = tree match {
      case DefDef(_, _, _, _, _, _) =>
        val lastMethod = currentMethod
        currentMethod = tree.symbol
        try super.traverse(tree)
        finally currentMethod = lastMethod
      /** A method call with a by-name parameter represents escape. */
      case Apply(fn, args) if fn.symbol.paramss.nonEmpty =>
        traverse(fn)
        (fn.symbol.paramss.head, args).zipped foreach { (param, arg) =>
          if (param.tpe != null && isByNameParamType(param.tpe))
            withEscaping(traverse(arg))
          else
            traverse(arg)
        }
      /** The rhs of a closure represents escape. */
      case Function(vparams, body) =>
        vparams foreach traverse
        withEscaping(traverse(body))

      /** The appearance of an ident outside the method where it was defined or
       *  anytime maybeEscaping is true implies escape.
       */
      case Ident(_) =>
        val sym = tree.symbol
        if (sym.isVariable && sym.owner.isMethod && (maybeEscaping || sym.owner != currentMethod)) {
          freeMutableVars += sym
          val symTpe = sym.tpe
          val symClass = symTpe.typeSymbol
          atPhase(phase.next) {
            def refType(valueRef: Map[Symbol, Symbol], objectRefClass: Symbol) =
              if (isValueClass(symClass)) valueRef(symClass).tpe
              else appliedType(objectRefClass.typeConstructor, List(symTpe))

            sym updateInfo (
              if (sym.hasAnnotation(VolatileAttr)) refType(volatileRefClass, VolatileObjectRefClass)
              else refType(refClass, ObjectRefClass))
          }
        }
      case _ =>
        super.traverse(tree)
    }
  }
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
