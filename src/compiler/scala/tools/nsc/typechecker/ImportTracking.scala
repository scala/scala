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

package scala.tools.nsc
package typechecker

import scala.annotation.nowarn
import scala.collection.mutable
import scala.reflect.internal.Chars.{isLineBreakChar, isWhitespace}
import scala.reflect.internal.util.CodeAction
import scala.tools.nsc.Reporting.WarningCategory

/** Track import clauses and usages for -Wunused:imports reporting.
 */
trait ImportTracking { self: Analyzer =>
  import global._

  // Associate info with info at import keyword, plus owner for warning filtering. `import a.x, b.x` -> `(b, a, owner)`
  private type TrackedInfo = (ImportInfo, ImportInfo, Symbol)

  private val usedSelectors = mutable.Map.empty[ImportInfo, mutable.Set[ImportSelector]]
  private val importInfos   = mutable.Map.empty[CompilationUnit, List[TrackedInfo]].withDefaultValue(Nil)

  def recordImportUsage(info: ImportInfo, sel: ImportSelector): Unit = usedSelectors.get(info) match {
    case Some(sels) => sels.addOne(sel)
    case None => usedSelectors.put(info, mutable.Set(sel))
  }

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
      val unused: List[Culled] =
        infos.flatMap {
          case (tracked @ (info, _, _)) =>
            val used = usedSelectors.remove(info).getOrElse(mutable.Set.empty)
            info.tree.selectors.collect {
              case selector if !selector.isMask && !used(selector) => selector -> tracked
            }
        }.sortBy { case (_, (info, _, _)) => info.pos.start } // stable sort on info.pos preserves order of selectors
      def emit(culled: Culled, actions: List[CodeAction]): Unit = culled match {
        case (selector, (info, _, owner)) =>
          val pos = info.posOf(selector)
          val origin = info.fullSelectorString(selector)
          val addendum = checkDeprecatedElementInPath(selector, info)
          runReporting.warning(pos, s"Unused import$addendum", WarningCategory.UnusedImports, owner, origin, actions)
      }
      // If the rest of the line is blank, include it in the final edit position. (Delete trailing whitespace.)
      // If replacement is empty, and the prefix of the line is also blank, then include that, too. (Del blank line.)
      def editPosAt(pos: Position, replacement: String): Position = {
        val content = pos.source.content
        val prev = content.lastIndexWhere(c => !isWhitespace(c), end = pos.start - 1)
        val emptyLeft = prev < 0 || isLineBreakChar(content(prev))
        val next = content.indexWhere(c => !isWhitespace(c), from = pos.end)
        val emptyRight = next < 0 || isLineBreakChar(content(next))
        val deleteLine = emptyLeft && emptyRight && replacement.isEmpty
        val bump = if (deleteLine) 1 else 0
        val p1 = if (next >= 0 && emptyRight) pos.withEnd(next + bump) else pos
        val p2 = if (deleteLine) p1.withStart(prev + 1) else p1
        p2
      }
      def isSingleSelector(infos: List[TrackedInfo]): Boolean = infos match {
        case (info, _, _) :: Nil => info.tree.selectors.size == 1
        case _ => false
      }
      def emitEdits(): Unit = {
        def edit(pos: Position, replacement: String) =
          runReporting.codeAction("unused import", editPosAt(pos, replacement), replacement, desc = "remove import")
        def delete(pos: Position) = edit(pos, replacement = "")

        val statements = infos.groupBy(keyInfoOfTracked) // keyInfo -> tracked infos in statement

        unused.groupBy(keyInfoOfCulled).foreach {        // keyInfo -> culled selectors in statement
        case (keyInfo, culled :: Nil) if isSingleSelector(statements(keyInfo)) => // import a.x
          emit(culled, actions = delete(keyInfo.pos)) // just one warning with delete
        case (keyInfo, culleds) => // import a.x, b.{y, z}
          val tracking = culleds.groupBy(infoOfCulled) // info -> Culled selectors (group by import clause)
          val deleting = tracking.view.mapValues(_.map(_._1)).toMap // info -> selectors to remove: b.{y, z} -> y
          val existing = statements(keyInfo).map(_._1).sortBy(_.tree.pos.start) // infos for a, b
          val (editing, keeping) = existing.partition(deleting.contains(_)) // deleting = info has a selector to del
          val (removing, updating) = editing.partition(info => info.tree.selectors.length == deleting(info).size)
          if (keeping.isEmpty && updating.isEmpty) { // all clauses are removed in the current statement
            // existing.flatMap(tracking)
            val ordered = culleds.sortBy(_._1.namePos)
            ordered.init.foreach(emit(_, actions = Nil)) // emit warnings for N-1 selectors
            val imports = existing.map(_.tree)
            val editPos = wrappingPos(imports.head.pos, imports) // reconstitute range of import statement
            emit(ordered.last, actions = delete(editPos)) // at Nth selector, delete the statement
          }
          else
            foreachWithIndex(existing) { (info, i) =>
              if (removing.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                toEmit.init.foreach(emit(_, actions = Nil)) // emit warnings for N-1 selectors for clause
                // normally, delete from start of this clause to start of next clause: a.x, b.{y, z} from a to b
                // but if this is the last clause, then also delete the comma following the last undeleted clause.
                // also if this is the first clause, start includes the keyword, so advance it to the name (point)
                val n = existing.size
                val editPos = {
                  val p0 = info.tree.pos.withStart(info.tree.pos.point)
                  if (i == n - 1) p0
                  else p0.withEnd(existing(i + 1).tree.pos.start)
                }
                val actions =
                  if (n > 1 && i == n - 1) {
                    val prev = existing.lastIndexWhere(!deleting.contains(_))
                    val prevPos = existing(prev).tree.pos
                    val commaPos = prevPos.copyRange(start = prevPos.end, end = existing(prev + 1).tree.pos.start)
                    delete(commaPos) ++ delete(editPos)
                  }
                  else delete(editPos)
                emit(toEmit.last, actions) // at Nth selector, delete the clause (and maybe a comma)
              }
              else if (updating.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                val remaining = info.tree.selectors.filter(!deleting(info).contains(_))
                if (remaining.size == 1) { // reformat without braces if remaining selector a.x
                  toEmit.init.foreach(emit(_, actions = Nil))
                  val editPos = info.tree.pos.withStart(info.tree.pos.point) // exclude import keyword if i == 0
                  val revised = info.tree.copy(selectors = remaining)
                  emit(toEmit.last, edit(editPos, revised.toString.stripPrefix("import "))) // exclude the keyword
                }
                else {
                  // emit an edit at each change to preserve formatting.
                  // there are multiple selectors, comma-separated in braces {x, y => w, z}.
                  // delete from start of name to start of next name,
                  // except at last selector, where it's necessary to delete a preceding comma.
                  // find the previous selector that is not deleted, and delete from its comma to start of next name.
                  val selectors = info.tree.selectors
                  val infoPos = info.tree.pos
                  val last = selectors.last
                  val content = infoPos.source.content
                  toEmit.foreach { case culled @ (selector, (_, _, _)) =>
                    if (selector != last) {
                      val index = selectors.indexWhere(_ == selector)
                      val editPos = infoPos.copyRange(start = selector.namePos, end = selectors(index + 1).namePos)
                      emit(culled, delete(editPos))
                    }
                    else {
                      // info.tree.pos.end is one char after rbrace
                      val prev = selectors.lastIndexWhere(remaining.contains(_))
                      val comma = content.indexWhere(_ == ',', from = selectors(prev).namePos)
                      val commaPos = infoPos.copyRange(start = comma, end = selectors(prev + 1).namePos)
                      val editPos = infoPos.copyRange(start = selector.namePos, end = info.tree.pos.end - 1)
                      emit(culled, delete(commaPos) ++ delete(editPos))
                    }
                  }
                }
              }
            }
        }
      }
      if (settings.quickfix.isSetByUser && !settings.quickFixSilent) emitEdits()
      else unused.foreach(emit(_, actions = Nil))
    }
    importInfos.remove(unit).foreach(warnUnusedSelections)
  }
}
