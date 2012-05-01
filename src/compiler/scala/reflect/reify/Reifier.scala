package scala.reflect
package reify

import scala.tools.nsc.Global
import scala.reflect.makro.ReificationError
import scala.reflect.makro.UnexpectedReificationError

/** Given a tree or a type, generate a tree that when executed at runtime produces the original tree or type.
 *  See more info in the comments to ``reify'' in scala.reflect.api.Universe.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
abstract class Reifier extends Phases
                          with Errors {

  val mirror: Global
  import mirror._
  import definitions._
  import treeInfo._

  val typer: mirror.analyzer.Typer
  val prefix: Tree
  val reifee: Any
  val dontSpliceAtTopLevel: Boolean
  val concrete: Boolean

  /**
   *  For ``reifee'' and other reification parameters, generate a tree of the form
   *
   *    {
   *      val $mr = <[ prefix ]>
   *      $mr.Expr[T](rtree)       // if data is a Tree
   *      $mr.TypeTag[T](rtree)    // if data is a Type
   *    }
   *
   *  where
   *
   *    - `prefix` is the tree that represents the universe
   *       the result will be bound to
   *    - `rtree` is code that generates `reifee` at runtime.
   *    - `T` is the type that corresponds to `data`.
   *
   *  This is not a method, but a value to indicate the fact that Reifier instances are a one-off.
   */
  lazy val reified: Tree = {
    try {
      // [Eugene] conventional way of doing this?
      if (prefix exists (_.isErroneous)) CannotReifyErroneousPrefix(prefix)
      if (prefix.tpe == null) CannotReifyUntypedPrefix(prefix)

      val rtree = reifee match {
        case tree: Tree =>
          reifyTrace("reifying = ")(if (opt.showTrees) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString)
          reifyTrace("reifee is located at: ")(tree.pos)
          reifyTrace("prefix = ")(prefix)
          // [Eugene] conventional way of doing this?
          if (tree exists (_.isErroneous)) CannotReifyErroneousReifee(prefix)
          if (tree.tpe == null) CannotReifyUntypedReifee(tree)
          val pipeline = mkReificationPipeline
          val rtree = pipeline(tree)

          // consider the following code snippet
          //
          //   val x = reify { class C; new C }
          //
          // inferred type for x will be C
          // but C ceases to exist after reification so this type is clearly incorrect
          // however, reify is "just" a library function, so it cannot affect type inference
          //
          // hence we crash here even though the reification itself goes well
          // fortunately, all that it takes to fix the error is to cast "new C" to Object
          // so I'm not very much worried about introducing this restriction
          if (tree.tpe exists (sub => sub.typeSymbol.isLocalToReifee))
            CannotReifyReifeeThatHasTypeLocalToReifee(tree)

          val taggedType = typer.packedType(tree, NoSymbol)
          val tagModule = if (reificationIsConcrete) ConcreteTypeTagModule else TypeTagModule
          val tagCtor = TypeApply(Select(Ident(nme.MIRROR_SHORT), tagModule.name), List(TypeTree(taggedType)))
          val exprCtor = TypeApply(Select(Ident(nme.MIRROR_SHORT), ExprModule.name), List(TypeTree(taggedType)))
          val tagArgs = List(reify(taggedType), reifyErasure(mirror)(typer, taggedType, concrete = false))
          Apply(Apply(exprCtor, List(rtree)), List(Apply(tagCtor, tagArgs)))

        case tpe: Type =>
          reifyTrace("reifying = ")(tpe.toString)
          reifyTrace("prefix = ")(prefix)
          val rtree = reify(tpe)

          val taggedType = tpe
          val tagModule = if (reificationIsConcrete) ConcreteTypeTagModule else TypeTagModule
          val ctor = TypeApply(Select(Ident(nme.MIRROR_SHORT), tagModule.name), List(TypeTree(taggedType)))
          val args = List(rtree, reifyErasure(mirror)(typer, taggedType, concrete = false))
          Apply(ctor, args)

        case _ =>
          throw new Error("reifee %s of type %s is not supported".format(reifee, if (reifee == null) "null" else reifee.getClass.toString))
      }

      val mirrorAlias = ValDef(NoMods, nme.MIRROR_SHORT, SingletonTypeTree(prefix), prefix)
      val wrapped = Block(mirrorAlias :: symbolTable, rtree)

      // todo. why do we resetAllAttrs?
      //
      // typically we do some preprocessing before reification and
      // the code emitted/moved around during preprocessing is very hard to typecheck, so we leave it as it is
      // however this "as it is" sometimes doesn't make any sense
      //
      // ===example 1===
      // we move a freevar from a nested symbol table to a top-level symbol table,
      // and then the reference to mr$ becomes screwed up, because nested symbol tables are already typechecked,
      // so we have an mr$ symbol that points to the nested mr$ rather than to the top-level one.
      //
      // ===example 2===
      // we inline a freevar by replacing a reference to it, e.g. $mr.Apply($mr.Select($mr.Ident($mr.newTermName("$mr")), $mr.newTermName("Ident")), List($mr.Ident($mr.newTermName("free$x"))))
      // with its original binding (e.g. $mr.Ident("x"))
      // we'd love to typecheck the result, but we cannot do this easily, because $mr is external to this tree
      // what's even worse, sometimes $mr can point to the top-level symbol table's $mr, which doesn't have any symbol/type yet -
      // it's just a ValDef that will be emitted only after the reification is completed
      //
      // hence, the simplest solution is to erase all attrs so that invalid (as well as non-existent) bindings get rebound correctly
      // this is ugly, but it's the best we can do
      //
      // todo. this is a common problem with non-trivial macros in our current macro system
      // needs to be solved some day
      //
      // list of non-hygienic transformations:
      // 1) local freetype inlining in Nested
      // 2) external freevar moving in Nested
      // 3) local freeterm inlining in Metalevels
      // 4) trivial tree splice inlining in Reify (Trees.scala)
      // 5) trivial type splice inlining in Reify (Types.scala)
      val freevarBindings = symbolTable collect { case entry @ FreeDef(_, _, binding, _, _) => binding.symbol } toSet
      // [Eugene] yeah, ugly and extremely brittle, but we do need to do resetAttrs. will be fixed later
      var importantSymbols = Set[Symbol](PredefModule, ScalaRunTimeModule)
      importantSymbols ++= importantSymbols map (_.companionSymbol)
      importantSymbols ++= importantSymbols map (_.moduleClass)
      importantSymbols ++= importantSymbols map (_.linkedClassOfClass)
      def importantSymbol(sym: Symbol): Boolean = sym != null && sym != NoSymbol && importantSymbols(sym)
      val untyped = resetAllAttrs(wrapped, leaveAlone = {
        case ValDef(_, mr, _, _) if mr == nme.MIRROR_SHORT => true
        case tree if freevarBindings contains tree.symbol => true
        case tree if importantSymbol(tree.symbol) => true
        case _ => false
      })

      if (reifyCopypaste) {
        if (reifyDebug) println("=============================")
        println(reifiedNodeToString(prefix, untyped))
        if (reifyDebug) println("=============================")
      } else {
        reifyTrace("reified = ")(untyped)
      }

      untyped
    } catch {
      case ex: ReificationError =>
        throw ex
      case ex: UnexpectedReificationError =>
        throw ex
      case ex: Throwable =>
        throw new UnexpectedReificationError(defaultErrorPosition, "reification crashed", ex)
    }
  }
}