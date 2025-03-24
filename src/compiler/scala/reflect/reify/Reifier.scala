/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.reify

import scala.tools.nsc.Global
import scala.reflect.macros.ReificationException
import scala.reflect.macros.UnexpectedReificationException
import scala.reflect.reify.utils.Utils

/** Given a tree or a type, generate a tree that when executed at runtime produces the original tree or type.
 *  See more info in the comments to `reify` in scala.reflect.api.Universe.
 */
abstract class Reifier extends States
                          with Phases
                          with Errors
                          with Utils {

  val global: Global
  import global._
  import definitions._
  private val runDefinitions = currentRun.runDefinitions

  val typer: global.analyzer.Typer
  val universe: Tree
  val mirror: Tree
  val reifee: Any
  val concrete: Boolean

  // needed to seamlessly integrate with standalone utils
  override def getReifier: Reifier { val global: Reifier.this.global.type } =
    this.asInstanceOf[Reifier { val global: Reifier.this.global.type }]
  override def hasReifier = true

  /** For `reifee` and other reification parameters, generate a tree of the form
   *  {{{
   *    {
   *      val \$u: universe.type = <[ universe ]>
   *      val \$m: \$u.Mirror = <[ mirror ]>
   *      \$u.Expr[T](rtree)       // if data is a Tree
   *      \$u.TypeTag[T](rtree)    // if data is a Type
   *    }
   *  }}}
   *
   *  where
   *
   *    - `universe` is the tree that represents the universe the result will be bound to.
   *    - `mirror` is the tree that represents the mirror the result will be initially bound to.
   *    - `rtree` is code that generates `reifee` at runtime.
   *    - `T` is the type that corresponds to `data`.
   *
   *  This is not a method, but a value to indicate the fact that Reifier instances are a one-off.
   */
  lazy val reification: Tree = {
    try {
      if (universe exists (_.isErroneous)) CannotReifyErroneousPrefix(universe)
      if (universe.tpe == null) CannotReifyUntypedPrefix(universe)

      val result = reifee match {
        case tree: Tree =>
          reifyTrace("reifying = ")(if (settings.Xshowtrees.value || settings.XshowtreesCompact.value || settings.XshowtreesStringified.value) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString)
          reifyTrace("reifee is located at: ")(tree.pos)
          reifyTrace("universe = ")(universe)
          reifyTrace("mirror = ")(mirror)
          if (tree exists (_.isErroneous)) CannotReifyErroneousReifee(tree)
          if (tree.tpe == null) CannotReifyUntypedReifee(tree)
          val pipeline = mkReificationPipeline
          val rtree = pipeline(tree)

          val tpe = typer.packedType(tree, NoSymbol)
          val ReifiedType(_, _, tpeSymtab, _, rtpe, tpeReificationIsConcrete) = `package`.reifyType(global)(typer, universe, mirror, tpe, concrete = false): @unchecked
          state.reificationIsConcrete &= tpeReificationIsConcrete
          state.symtab ++= tpeSymtab
          ReifiedTree(universe, mirror, symtab, rtree, tpe, rtpe, reificationIsConcrete)

        case tpe: Type =>
          reifyTrace("reifying = ")(tpe.toString)
          reifyTrace("universe = ")(universe)
          reifyTrace("mirror = ")(mirror)
          val rtree = reify(tpe)
          ReifiedType(universe, mirror, symtab, tpe, rtree, reificationIsConcrete)

        case _ =>
          throw new Error("reifee %s of type %s is not supported".format(reifee, if (reifee == null) "null" else reifee.getClass.toString))
      }

      // todo. why do we reset attrs?
      //
      // typically we do some preprocessing before reification and
      // the code emitted/moved around during preprocessing is very hard to typecheck, so we leave it as it is
      // however this "as it is" sometimes doesn't make any sense
      //
      // ===example 1===
      // we move a freevar from a nested symbol table to a top-level symbol table,
      // and then the reference to $u becomes screwed up, because nested symbol tables are already typechecked,
      // so we have an $u symbol that points to the nested $u rather than to the top-level one.
      //
      // ===example 2===
      // we inline a freevar by replacing a reference to it, e.g. $u.Apply($u.Select($u.Ident($u.newTermName("$u")), $u.newTermName("Ident")), List($u.Ident($u.newTermName("free$x"))))
      // with its original binding (e.g. $u.Ident("x"))
      // we'd love to typecheck the result, but we cannot do this easily, because $u is external to this tree
      // what's even worse, sometimes $u can point to the top-level symbol table's $u, which doesn't have any symbol/type yet -
      // it's just a ValDef that will be emitted only after the reification is completed
      //
      // hence, the simplest solution is to erase all attrs so that invalid (as well as non-existent) bindings get rebound correctly
      // this is ugly, but it's the best we can do
      //
      // todo. this is a common problem with non-trivial macros in our current macro system
      // needs to be solved some day
      // upd. a new hope: https://groups.google.com/forum/#!topic/scala-internals/TtCTPlj_qcQ
      var importantSymbols = Set[Symbol](
        NothingClass, AnyClass, SingletonClass, PredefModule, ScalaRunTimeModule, TypeCreatorClass, TreeCreatorClass, MirrorClass,
        ApiUniverseClass, JavaUniverseClass, ReflectRuntimePackage, runDefinitions.ReflectRuntimeCurrentMirror)
      importantSymbols ++= importantSymbols map (_.companionSymbol)
      importantSymbols ++= importantSymbols map (_.moduleClass)
      importantSymbols ++= importantSymbols map (_.linkedClassOfClass)
      def isImportantSymbol(sym: Symbol): Boolean = sym != null && sym != NoSymbol && importantSymbols(sym)
      val untyped = brutallyResetAttrs(result, leaveAlone = {
        case ValDef(_, u, _, _) if u == nme.UNIVERSE_SHORT => true
        case ValDef(_, m, _, _) if m == nme.MIRROR_SHORT => true
        case tree if symtab.syms contains tree.symbol => true
        case tree if isImportantSymbol(tree.symbol) => true
        case _ => false
      })

      if (reifyCopypaste) {
        if (reifyDebug) println("=============================")
        println(reifiedNodeToString(untyped))
        if (reifyDebug) println("=============================")
      } else {
        reifyTrace("reification = ")(untyped)
      }

      untyped
    } catch {
      case ex: ReificationException =>
        throw ex
      case ex: UnexpectedReificationException =>
        throw ex
      case ex: Throwable =>
        throw UnexpectedReificationException(defaultErrorPosition, "reification crashed", ex)
    }
  }
}
