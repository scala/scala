/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.annotation.nowarn
import scala.collection.mutable
import scala.reflect.internal.Chars.{CR, LF, isLineBreakChar, isWhitespace}
import scala.reflect.internal.util.CodeAction
import scala.tools.nsc.Reporting.WarningCategory

/** Track import clauses and usages for -Wunused:imports reporting.
 */
trait ImportTracking { self: Analyzer =>
  import global._

  // Associate info with info at import keyword, plus owner for warning filtering. `import a.x, b.x` -> `(b, a, owner)`
  private type TrackedInfo = (ImportInfo, ImportInfo, Symbol)

  private val usedSelectors = mutable.Map.empty[ImportInfo, Set[ImportSelector]].withDefaultValue(Set.empty)
  private val importInfos   = mutable.Map.empty[CompilationUnit, List[TrackedInfo]].withDefaultValue(Nil)

  def recordImportUsage(info: ImportInfo, sel: ImportSelector): Unit = usedSelectors(info) += sel

  def recordImportContext(ctx: Context): Unit = ctx.firstImport.foreach { info =>
    val keyword =
      if (info.pos.start != info.pos.point) info
      else ctx.imports.find(p => p.pos.isDefined && p.pos.start != p.pos.point).getOrElse(info)
    importInfos(ctx.unit) ::= (info, keyword, ctx.owner) : @nowarn
  }

  def warnUnusedImports(unit: CompilationUnit): Unit = if (!unit.isJava) {
    def checkDeprecatedElementInPath(selector: ImportSelector, info: ImportInfo): String = {
      def msg(sym: Symbol) = sym.deprecationMessage.map(": " + _).getOrElse("")
      def badName(name: Name) =
        info.qual.tpe.member(name) match {
          case m if m.isDeprecated => Some(s" of deprecated $m${msg(m)}")
          case _ => None
        }
      val badSelected =
        if (!selector.isMask && selector.isSpecific) badName(selector.name).orElse(badName(selector.name.toTypeName))
        else None
      def badFrom = {
        val sym = info.qual.symbol
        if (sym.isDeprecated) Some(s" from deprecated $sym${msg(sym)}") else None
      }
      badSelected.orElse(badFrom).getOrElse("")
    }
    def warnUnusedSelections(infos: List[TrackedInfo]): Unit = {
      type Culled = (ImportSelector, TrackedInfo)
      def keyInfoOfTracked(info: TrackedInfo): ImportInfo = info._2
      def keyInfoOfCulled(culled: Culled): ImportInfo = keyInfoOfTracked(culled._2)
      def infoOfCulled(culled: Culled): ImportInfo = culled._2._1
      val unused: List[Culled] = {
        var res = List.empty[Culled]
        def cull(infos: List[TrackedInfo]): Unit =
          infos match {
            case (tracked @ (info, _, _)) :: infos =>
              val used = usedSelectors.remove(info).getOrElse(Set.empty)
              def checkSelectors(selectors: List[ImportSelector]): Unit =
                selectors match {
                  case selector :: selectors =>
                    checkSelectors(selectors)
                    if (!selector.isMask && !used(selector))
                      res ::= selector -> tracked
                  case _ =>
                }
              checkSelectors(info.tree.selectors)
              cull(infos)
            case _ =>
          }
        cull(infos)
        res
      }
      def emit(culled: Culled, actions: List[CodeAction]): Unit = culled match {
        case (selector, (info, _, owner)) =>
          val pos = info.posOf(selector)
          val origin = info.fullSelectorString(selector)
          val addendum = checkDeprecatedElementInPath(selector, info)
          runReporting.warning(pos, s"Unused import$addendum", WarningCategory.UnusedImports, owner, origin, actions)
      }
      // Widen pos to include leading and trailing white space on the line, including a trailing newline if result line is blank.
      // Include trailing comma if commaBias > 0. (Or preceding comma if commaBias < 0, for last element in a list.)
      def expandToLine(pos: Position, commaBias: Int): Position = {
        val content = pos.source.content
        val prev = content.lastIndexWhere(c => !isWhitespace(c), end = pos.start-1)
        val emptyLeft = prev < 0 || isLineBreakChar(content(prev))
        val next = content.indexWhere(c => !isWhitespace(c) && !(commaBias > 0 && c == ','), from = pos.end)
        val emptyRight = next < 0 || isLineBreakChar(content(next))

        if (emptyLeft && emptyRight) { // blank result line with trailing line separator, trim it
          val end =
            if (next >= 0)
              if (content(next) == CR && next+1 < content.length && content(next+1) == LF) next + 2
              else next + 1
            else content.length
          val start =
            if (commaBias < 0) {
              val probe = content.lastIndexWhere(c => !isWhitespace(c) && !isLineBreakChar(c), end = prev-1)
              if (probe >= 0 && content(probe) == ',') probe
              else if (prev >= 0) prev + 1
              else 0
            }
            else if (prev >= 0) prev + 1
            else 0
          pos.withStart(start).withEnd(end)
        }
        else if (commaBias < 0) {
          val start =
            if (prev >= 0 && content(prev) == ',') prev
            else {
              val probe = content.lastIndexWhere(c => !isWhitespace(c) && !isLineBreakChar(c), end = prev-1)
              if (probe >= 0 && content(probe) == ',') probe else if (prev >= 0) prev + 1 else 0
            }
          pos.withStart(start)
        }
        else if (commaBias > 0) {
          val next = content.indexWhere(c => !isWhitespace(c), from = pos.end)
          if (next >= 0 && content(next) == ',') pos.withEnd(next + 1)
          else pos
        }
        else pos
      }
      // if quickfixing, emit (possibly "merged") edit(s) per import statement
      if (settings.quickfix.isSetByUser && !settings.quickFixSilent) {
        def edit(pos: Position, replacement: String, commaBias: Int = 0) =
          runReporting.codeAction("unused import", expandToLine(pos, commaBias), newText = replacement, desc = "remove import")
        def delete(pos: Position, commaBias: Int) = edit(pos, replacement = "", commaBias)
        val statements = infos.groupBy(keyInfoOfTracked) // keyInfo -> tracked infos in statement
        unused.groupBy(keyInfoOfCulled).foreach {
        case (keyInfo, culled :: Nil) if statements(keyInfo).size == 1 && statements(keyInfo).head._1.tree.selectors.size == 1 => // import a.x
          emit(culled, actions = delete(keyInfo.pos, commaBias = 0)) // just one warning and edit
        case (keyInfo, culleds) => // import a.x, b.{y, z}
          val tracking = culleds.groupBy(infoOfCulled) // info -> Culled selectors
          val deleting = tracking.view.mapValues(_.map(_._1)).toMap // info -> selectors to remove: b.{y, z} -> y
          val existing = statements(keyInfo).map(_._1).sortBy(_.tree.pos.start) // infos for a, b
          val (editing, keeping) = existing.partition(deleting.contains(_))
          val (removing, updating) = editing.partition(info => info.tree.selectors.forall(deleting(info).contains(_)))
          if (keeping.isEmpty && updating.isEmpty) { // all are removed
            existing.init.foreach { info =>
              tracking(info).sortBy(_._1.namePos).foreach(emit(_, actions = Nil))
            }
            val info = existing.last
            val toEmit = tracking(info).sortBy(_._1.namePos)
            toEmit.init.foreach(emit(_, actions = Nil))
            val imports = existing.map(_.tree)
            val editPos = wrappingPos(imports.head.pos, imports)
            emit(toEmit.last, actions = delete(editPos, commaBias = 0)) // single delete of statement at last selector of last import info
          }
          else
            foreachWithIndex(existing) { (info, i) =>
              if (removing.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                toEmit.init.foreach(emit(_, actions = Nil))
                val commaBias = { // including comma to right, unless at end and comma to left was not deleted
                  val n = existing.size
                  if (n > 1 && i == n - 1 && !deleting.contains(existing(i - 1))) -1 else 1
                }
                val editPos = {
                  // exclude the keyword, i.e., do not edit it out
                  val p0 = info.tree.pos.withStart(info.tree.pos.point)
                  val prev = p0.source.content.lastIndexWhere(c => !isWhitespace(c), end = p0.start-1)
                  p0.withStart(prev + 1)
                }
                emit(toEmit.last, delete(editPos, commaBias = commaBias)) // emit edit with last warning for last selector
              }
              else if (updating.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                val remaining = info.tree.selectors.filter(!deleting(info).contains(_))
                if (remaining.size == 1) {
                  toEmit.init.foreach(emit(_, actions = Nil))
                  val editPos = info.tree.pos.withStart(info.tree.pos.point) // exclude the keyword, i.e., do not edit it out
                  val revised = info.tree.copy(selectors = remaining)
                  emit(toEmit.last, edit(editPos, replacement = revised.toString.stripPrefix("import "))) // exclude the keyword on output
                }
                else {
                  // instead of replacing the import, emit an edit for each change to preserve formatting.
                  // there are multiple selectors, comma-separated in braces {x, y => w, z}.
                  // for the prefix of deleted selectors, also delete the next comma if there is one before a brace.
                  // thereafter, also delete the preceding comma.
                  //val first = info.tree.selectors.head
                  val last = info.tree.selectors.last
                  toEmit.foreach { case culled @ (selector, (_, _, _)) =>
                    val start = selector.namePos
                    @annotation.unused val x = '{'
                    val end = info.tree.pos.source.content.indexWhere(c => c == ',' || c == '}', from = start)
                    val commaBias =
                      if (selector == last && info.tree.selectors.length > 1 && !toEmit.contains(info.tree.selectors.dropRight(1).last)) -1
                      else 1
                    val editPos = info.tree.pos.withStart(start).withEnd(end)
                    emit(culled, delete(editPos, commaBias = commaBias))
                  }
                }
              }
            }
        }
      }
      else unused.foreach(emit(_, actions = Nil))
    }
    importInfos.remove(unit).foreach(warnUnusedSelections)
  }
}
