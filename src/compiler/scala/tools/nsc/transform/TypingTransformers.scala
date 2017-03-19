/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
trait TypingTransformers {

  val global: Global
  import global._

  abstract class TypingTransformer(unit: CompilationUnit) extends Transformer {
    var localTyper: analyzer.Typer =
      if (phase.erasedTypes)
        erasure.newTyper(erasure.rootContextPostTyper(unit, EmptyTree)).asInstanceOf[analyzer.Typer]
      else // TODO: AM: should some phases use a regular rootContext instead of a post-typer one??
        analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
    protected var curTree: Tree = _

    override def atOwner[A](owner: Symbol)(trans: => A): A = atOwner(curTree, owner)(trans)

    def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(tree, if (owner.isModuleNotMethod) owner.moduleClass else owner)
      val result = super.atOwner(owner)(trans)
      localTyper = savedLocalTyper
      result
    }

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Template(_, _, _) =>
          // enter template into context chain
          atOwner(currentOwner) { super.transform(tree) }
        case PackageDef(_, _) =>
          atOwner(tree.symbol) { super.transform(tree) }
        case _ =>
          super.transform(tree)
      }
    }
  }

  abstract class BaseTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    @inline override final def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner

      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(curTree, if (owner.isModuleNotMethod) owner.moduleClass else owner)

      val result = trans

      localTyper = savedLocalTyper
      currentOwner = prevOwner

      result
    }

    @inline override final def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner

      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(tree, if (owner.isModuleNotMethod) owner.moduleClass else owner)

      val result = trans

      localTyper = savedLocalTyper
      currentOwner = prevOwner

      result
    }
    // DUPLICATED from itransform
    override def transform(tree: Tree): Tree = {
      curTree = tree
      def mclass(sym: Symbol) = sym map (_.asModule.moduleClass)
      // begin itransform
      tree match {
        case Ident(name) =>
          treeCopy.Ident(tree, name)
        case Select(qualifier, selector) =>
          treeCopy.Select(tree, transform(qualifier), selector)
        case Apply(fun, args) =>
          treeCopy.Apply(tree, transform(fun), transformTrees(args))
        case TypeTree() =>
          treeCopy.TypeTree(tree)
        case Literal(value) =>
          treeCopy.Literal(tree, value)
        case This(qual) =>
          treeCopy.This(tree, qual)
        case ValDef(mods, name, tpt, rhs) =>
          atOwner(tree.symbol) {
            treeCopy.ValDef(tree, transformModifiers(mods),
              name, transform(tpt), transform(rhs))
          }
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          atOwner(tree.symbol) {
            treeCopy.DefDef(tree, transformModifiers(mods), name,
              transformTypeDefs(tparams), transformValDefss(vparamss),
              transform(tpt), transform(rhs))
          }
        case Block(stats, expr) =>
          treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
        case CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
        case TypeApply(fun, args) =>
          treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
        case AppliedTypeTree(tpt, args) =>
          treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
        case Bind(name, body) =>
          treeCopy.Bind(tree, name, transform(body))
        case Function(vparams, body) =>
          atOwner(tree.symbol) {
            treeCopy.Function(tree, transformValDefs(vparams), transform(body))
          }
        case Match(selector, cases) =>
          treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
        case New(tpt) =>
          treeCopy.New(tree, transform(tpt))
        case Assign(lhs, rhs) =>
          treeCopy.Assign(tree, transform(lhs), transform(rhs))
        case AssignOrNamedArg(lhs, rhs) =>
          treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
        case EmptyTree =>
          tree
        case Throw(expr) =>
          treeCopy.Throw(tree, transform(expr))
        case Super(qual, mix) =>
          treeCopy.Super(tree, transform(qual), mix)
        case TypeBoundsTree(lo, hi) =>
          treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi))
        case Typed(expr, tpt) =>
          treeCopy.Typed(tree, transform(expr), transform(tpt))
        case Import(expr, selectors) =>
          treeCopy.Import(tree, transform(expr), selectors)
        case Template(parents, self, body) =>
          // enter template into context chain
          atOwner(currentOwner) {
            treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
          }
        case ClassDef(mods, name, tparams, impl) =>
          atOwner(tree.symbol) {
            treeCopy.ClassDef(tree, transformModifiers(mods), name,
              transformTypeDefs(tparams), transformTemplate(impl))
          }
        case ModuleDef(mods, name, impl) =>
          atOwner(mclass(tree.symbol)) {
            treeCopy.ModuleDef(tree, transformModifiers(mods),
              name, transformTemplate(impl))
          }
        case TypeDef(mods, name, tparams, rhs) =>
          atOwner(tree.symbol) {
            treeCopy.TypeDef(tree, transformModifiers(mods), name,
              transformTypeDefs(tparams), transform(rhs))
          }
        case LabelDef(name, params, rhs) =>
          treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LambdaLifter.proxy'
        case PackageDef(pid, stats) =>
          atOwner(currentOwner) {
            treeCopy.PackageDef(
              tree, transform(pid).asInstanceOf[RefTree],
              atOwner(mclass(tree.symbol)) {
                transformStats(stats, currentOwner)
              }
            )
          }
        case Annotated(annot, arg) =>
          treeCopy.Annotated(tree, transform(annot), transform(arg))
        case SingletonTypeTree(ref) =>
          treeCopy.SingletonTypeTree(tree, transform(ref))
        case SelectFromTypeTree(qualifier, selector) =>
          treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
        case CompoundTypeTree(templ) =>
          treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
        case ExistentialTypeTree(tpt, whereClauses) =>
          treeCopy.ExistentialTypeTree(tree, transform(tpt), transformMemberDefs(whereClauses))
        case Return(expr) =>
          treeCopy.Return(tree, transform(expr))
        case Alternative(trees) =>
          treeCopy.Alternative(tree, transformTrees(trees))
        case Star(elem) =>
          treeCopy.Star(tree, transform(elem))
        case UnApply(fun, args) =>
          treeCopy.UnApply(tree, transform(fun), transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
        case ArrayValue(elemtpt, trees) =>
          treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
        case ApplyDynamic(qual, args) =>
          treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
        case ReferenceToBoxed(idt) =>
          treeCopy.ReferenceToBoxed(tree, transform(idt) match { case idt1: Ident => idt1 })
        case _ =>
          global.xtransform(this, tree)
      }
    }


  }
}

