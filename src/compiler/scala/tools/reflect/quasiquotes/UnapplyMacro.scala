package scala.tools.reflect

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable
import scala.collection.SortedMap

trait UnapplyMacro { self: Quasiquotes =>
  import c.universe._
  import c.universe.Flag._

  def unapplyQ: Tree = unapply(new QParser)

  def unapplyTq: Tree = unapply(new TQParser)

  def unapply(parser: QParser): Tree = {

    val (universe, parts) =
      c.macroApplication match {
        case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), _) =>
          val parts = parts0.map{
            case Literal(Constant(s: String)) => s
            case _ => throw new Exception("") // empty exception?
          }
          (universe, parts)
        case _ => throw new Exception("") // empty exception?
      }

    if (!(parts.length >= 1 && parts.length <= 23))
      c.abort(c.enclosingPosition, "Inappropriate amount of quasiquote params.")

    val unapplySelector = Ident(nme.SELECTOR_DUMMY)
    unapplySelector.setType(memberType(universe.tpe, tpnme.Tree))

    val (code, placeholders) = {
      val sb = new StringBuilder()
      var placeholders = SortedMap[String, (Tree, String)]()

      parts.init.foreach { p =>
        val (part, cardinality) =
          if (p.endsWith("..."))
            (p.stripSuffix("..."), "...")
          else if (p.endsWith(".."))
            (p.stripSuffix(".."), "..")
          else
            (p, "")
        val freshname = c.fresh(nme.QUASIQUOTE_PREFIX)
        sb.append(part)
        sb.append(freshname)
        placeholders += freshname -> (EmptyTree, cardinality)
      }
      sb.append(parts.last)

      (sb.toString, placeholders)
    }

    if (settings.Yquasiquotedebug.value) println(s"code to parse=\n$code\n")

    val tree = parser.parse(code, placeholders.keys.toSet)

    if (settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

    val reifier = new UnapplyReifier(universe, placeholders)
    val reifiedTree = reifier.reifyTree(tree)
    val correspondingTypes = reifier.correspondingTypes

    if (settings.Yquasiquotedebug.value) println(s"corresponding types=\n$correspondingTypes\n")
    if (settings.Yquasiquotedebug.value) println(s"reified tree\n=${reifiedTree}\n=${showRaw(reifiedTree)}\n")

    val caseBody: Tree =
      if (placeholders.size == 0)
        Literal(Constant(true))
      else if (placeholders.size == 1)
        Apply(Ident(nme.Some), List(Ident(TermName(placeholders.keys.head))))
      else
        Apply(
          Ident(nme.Some),
          List(Apply(
            Ident(nme.Tuple + placeholders.size.toString),
            placeholders.map(p => Ident(TermName(p._1))).toList)))

    val unapplyBody =
      Block(
        List(Import(Ident(nme.UNIVERSE_SHORT), List(ImportSelector(nme.WILDCARD, 0, null, 0)))),
        Match(Ident(nme.tree), List(
          CaseDef(reifiedTree, EmptyTree, caseBody),
          CaseDef(Ident(nme.WILDCARD), EmptyTree, Ident(nme.None)))))

    val localTreeType = Select(Ident(nme.UNIVERSE_SHORT), tpnme.Tree)

    val unapplyResultType: Tree =
      if (placeholders.size == 0) {
        Ident(tpnme.Boolean)
      } else if (placeholders.size == 1) {
        AppliedTypeTree(Ident(tpnme.Option), List(correspondingTypes(placeholders.keys.head)))
      } else {
        val tuple = Ident(TypeName("Tuple" + placeholders.size))
        val treetuple = AppliedTypeTree(tuple, placeholders.map(p => correspondingTypes(p._1)).toList)
        val optiontreetuple = AppliedTypeTree(Ident(tpnme.Option), List(treetuple))
        optiontreetuple
      }

    val apiUniverseType = Select(Select(Select(Ident(nme.scala_), nme.reflect), nme.api), tpnme.Universe)

    val unapplyMethod =
      DefDef(
        Modifiers(), nme.unapply, List(),
        List(
          List(ValDef(Modifiers(PARAM), nme.UNIVERSE_SHORT, apiUniverseType, EmptyTree)),
          List(ValDef(Modifiers(PARAM), nme.tree, localTreeType, EmptyTree))),
        unapplyResultType,
        unapplyBody)

    val moduleName = TermName(nme.QUASIQUOTE_MATCHER_NAME + randomUUID().toString.replace("-", ""))

    val moduleDef =
      ModuleDef(Modifiers(), moduleName, Template(
        List(Select(Ident(nme.scala_), tpnme.AnyRef)),
        emptyValDef,
        List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
          unapplyMethod)))

    if (settings.Yquasiquotedebug.value) println(s"moduledef\n=${showRaw(moduleDef, printTypes=true, printIds=true)}\n=$moduleDef\n")

    val modulePos = c.enclosingPosition.focus

    c.introduceTopLevel(nme.QUASIQUOTE_MATCHER_PACKAGE, atPos(modulePos)(moduleDef))

    val moduleRef = Select(Ident(nme.QUASIQUOTE_MATCHER_PACKAGE), moduleName)

    val result = Apply(Apply(Select(moduleRef, nme.unapply), List(universe)), List(unapplySelector))

    result
  }
}
