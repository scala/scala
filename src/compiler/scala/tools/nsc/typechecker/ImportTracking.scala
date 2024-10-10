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
    val keyword = if (info.pos.start != info.pos.point) info else ctx.imports.find(p => p.pos.isDefined && p.pos.start != p.pos.point).getOrElse(info)
    importInfos(ctx.unit) ::= (info, keyword, ctx.owner) : @nowarn
  }

  def warnUnusedImports(unit: CompilationUnit): Unit = if (!unit.isJava) {
    def msg(sym: Symbol) = sym.deprecationMessage.map(": " + _).getOrElse("")
    def checkDeprecatedElementInPath(selector: ImportSelector, info: ImportInfo): String = {
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
      var unused = List.empty[Culled]
      def cull(infos: List[TrackedInfo]): Unit =
        infos match {
          case (tracked @ (info, _, _)) :: infos =>
            val used = usedSelectors.remove(info).getOrElse(Set.empty)
            def checkSelectors(selectors: List[ImportSelector]): Unit =
              selectors match {
                case selector :: selectors =>
                  checkSelectors(selectors)
                  if (!selector.isMask && !used(selector))
                    unused ::= selector -> tracked
                case _ =>
              }
            checkSelectors(info.tree.selectors)
            cull(infos)
          case _ =>
        }
      cull(infos)
      def emit(culled: Culled, actions: List[CodeAction]): Unit = culled match {
        case (selector, (info, _, owner)) =>
          val pos = info.posOf(selector)
          val origin = info.fullSelectorString(selector)
          val addendum = checkDeprecatedElementInPath(selector, info)
          runReporting.warning(pos, s"Unused import$addendum", WarningCategory.UnusedImports, owner, origin, actions)
      }
      // include leading and trailing white space on the line, including a trailing newline if result line is blank
      // if commaBias is non-zero, consume the adjacent comma (left if bias < 0) (and assume no leading comma)
      // if commaBias is > 0, don't consume left whitespace (to preserve import keyword) but do consume next comma.
      def expandToLine(pos: Position, commaBias: Int): Position = {
        var i = pos.start
        val content = pos.source.content
        if (commaBias <= 0)
          while (i > 0 && isWhitespace(content(i-1)) && !isLineBreakChar(content(i-1))) i -= 1
        val sawNL = i > 0 && isLineBreakChar(content(i-1))
        val emptyLeft = sawNL || i == 0 // import may be at start of content
        if (commaBias < 0)
          while (i > 0 && (isWhitespace(content(i-1)) || isLineBreakChar(content(i-1)) || content(i-1) == ',')) i -= 1
        var j = pos.end
        val max = content.length
        while (j < max && (isWhitespace(content(j)) || commaBias > 0 && content(j) == ',') && !isLineBreakChar(content(j))) j += 1
        val emptyResultLine = emptyLeft && j < max && isLineBreakChar(content(j))
        if (emptyResultLine) { // blank result line with trailing line separator, trim it
          if (content(j) == CR && j+1 < max && content(j+1) == LF) j += 2
          else j += 1
        }
        // omit this unnecessary complication
        //else if (sawNL && j == max) { // rare stray import at EOF (actually SourceFile always appends NL to content)
        //  if (content(i-1) == LF && i > 1 && content(i-2) == CR) i -= 2
        //  else i -= 1
        //}
        if (i != pos.start || j != pos.end) pos.withStart(i).withEnd(j) else pos
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
            val edits = delete(editPos, commaBias = 0) // single delete of statement at last selector of last import info
            emit(toEmit.last, edits)
          }
          else
            foreachWithIndex(existing) { (info, i) =>
              if (removing.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                toEmit.init.foreach(emit(_, actions = Nil))
                val editPos = info.tree.pos.withStart(info.tree.pos.point) // exclude the keyword, i.e., do not edit it out
                emit(toEmit.last, delete(editPos, commaBias = if (i == 0) 1 else -1)) // emit edit with last warning for last selector
              }
              else if (updating.contains(info)) {
                val toEmit = tracking(info).sortBy(_._1.namePos)
                val remaining = info.tree.selectors.filter(!deleting(info).contains(_))
                if (remaining.size == 1) {
                  toEmit.init.foreach(emit(_, actions = Nil))
                  val editPos = info.tree.pos.withStart(info.tree.pos.point) // exclude the keyword, i.e., do not edit it out
                  val revised = info.tree.copy(selectors = remaining)
                  emit(toEmit.last, edit(editPos, replacement = revised.toString.stripPrefix("import"))) // exclude the keyword on output
                }
                else {
                  // instead of replacing the import, emit an edit for each change to preserve formatting.
                  // first edit may overlap with second, so trim it.
                  // range of selector is from namePos to a comma or brace
                  var editss = toEmit.map { case culled @ (selector, (_, _, _)) =>
                    val j = info.tree.selectors.indexOf(selector)
                    //val end = if (j < info.tree.selectors.size - 1) info.tree.selectors(j + 1).namePos else info.tree.pos.end - 1
                    val end = info.tree.pos.source.content.indexWhere(c => c == ',' || c == '}', from = selector.namePos)
                    val editPos = info.tree.pos.withStart(selector.namePos).withEnd(end)
                    delete(editPos, commaBias = if (j == 0) 1 else -1)
                  }
                  // correct first two edits fighting over shared comma
                  if (editss.length > 1) {
                    val first = editss(0).head.edits.head
                    val second = editss(1).head.edits.head
                    if (first.position.overlaps(second.position))
                      editss = (editss(0).head.copy(edits = first.copy(position = first.position.withEnd(second.position.start)) :: Nil) :: Nil) :: editss.tail
                  }
                  foreach2(toEmit, editss)((culled, edits) => emit(culled, edits))
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
