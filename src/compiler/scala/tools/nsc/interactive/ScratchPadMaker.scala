package scala.tools.nsc
package interactive

import scala.reflect.internal.util.{SourceFile, BatchSourceFile, RangePosition}
import collection.mutable.ArrayBuffer
import reflect.internal.Chars.{isLineBreakChar, isWhitespace}

trait ScratchPadMaker { self: Global =>

  import definitions._

  private case class Patch(offset: Int, text: String)

  private class Patcher(contents: Array[Char], endOffset: Int) extends Traverser {
    var objectName: String = ""

    private val patches = new ArrayBuffer[Patch]
    private val toPrint = new ArrayBuffer[String]
    private var skipped = 0
    private var resNum: Int = -1

    private def nextRes(): String = {
      resNum += 1
      "res$"+resNum
    }

    private def nameType(name: String, tpe: Type): String = name+": "+tpe

    private def nameType(sym: Symbol): String = nameType(sym.name.toString, sym.tpe)

    private def literal(str: String) = "\"\"\""+str+"\"\"\""

    private val prologue = "import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{"

    private val epilogue = "}"

    private def applyPendingPatches(offset: Int) = {
      if (skipped == 0) patches += Patch(offset, prologue)
      for (msg <- toPrint) patches += Patch(offset, ";System.out.println("+msg+")")
      toPrint.clear()
    }

    /** The position where to insert an instrumentation statement in front of giuven statement.
     *  This is at the latest `stat.pos.start`. But in order not to mess with column numbers
     *  in position we try to insert it at the end of the preceding line instead.
     *  To be safe, this can be done only if there's only whitespace between that position and
     *  statement's start position.
     */
    private def instrumentPos(stat: Tree): Int = {
      var start = stat.pos.start
      while (start > 0 && isWhitespace(contents(start - 1))) start -= 1
      if (start > 0 && isLineBreakChar(contents(start - 1))) start -= 1
      start
    }

    private def addSkip(stat: Tree): Unit = {
      val ipos = instrumentPos(stat)
      if (stat.pos.start > skipped) applyPendingPatches(ipos)
      if (stat.pos.start >= endOffset)
        patches += Patch(ipos, ";$stop()")
      var end = stat.pos.end
      if (end > skipped) {
        while (end < contents.length && !isLineBreakChar(contents(end))) end += 1
        patches += Patch(ipos, ";$skip("+(end-skipped)+"); ")
        skipped = end
      }
    }

    private def addSandbox(expr: Tree) = {}
//      patches += (Patch(expr.pos.start, "sandbox("), Patch(expr.pos.end, ")"))

    private def resultString(prefix: String, expr: String) =
      literal(prefix + " = ") + " + $show(" + expr + ")"

    private def traverseStat(stat: Tree) =
      if (stat.pos.isInstanceOf[RangePosition]) {
        stat match {
          case ValDef(_, _, _, rhs) =>
            addSkip(stat)
            if (stat.symbol.isLazy)
              toPrint += literal(nameType(stat.symbol) + " = <lazy>")
            else if (!stat.symbol.isSynthetic) {
              addSandbox(rhs)
              toPrint += resultString(nameType(stat.symbol), stat.symbol.name.toString)
            }
          case DefDef(_, _, _, _, _, _) =>
            addSkip(stat)
            toPrint += literal(nameType(stat.symbol))
          case Annotated(_, arg) =>
            traverse(arg)
          case DocDef(_, defn) =>
            traverse(defn)
          case _ =>
            if (stat.isTerm) {
              addSkip(stat)
              if (stat.tpe.typeSymbol == UnitClass) {
                addSandbox(stat)
              } else {
                val resName = nextRes()
                val dispResName = resName filter ('$' != _)
                patches += Patch(stat.pos.start, "val " + resName + " = ")
                addSandbox(stat)
                toPrint += resultString(nameType(dispResName, stat.tpe), resName)
              }
            }
        }
      }

    override def traverse(tree: Tree): Unit = tree match {
      case PackageDef(_, _) =>
        super.traverse(tree)
      case ModuleDef(_, name, Template(_, _, body)) =>
        val topLevel = objectName.isEmpty
        if (topLevel) objectName = tree.symbol.fullName
        body foreach traverseStat
        if (skipped != 0) { // don't issue prologue and epilogue if there are no instrumented statements
          applyPendingPatches(skipped)
          if (topLevel)
            patches += Patch(skipped, epilogue)
        }
      case _ =>
    }

    /** The patched text.
     *  @require  traverse is run first
     */
    def result: Array[Char] = {
      val reslen = contents.length + (patches map (_.text.length)).sum
      val res = Array.ofDim[Char](reslen)
      var lastOffset = 0
      var from = 0
      var to = 0
      for (Patch(offset, text) <- patches) {
        val delta = offset - lastOffset
        assert(delta >= 0)
        Array.copy(contents, from, res, to, delta)
        from += delta
        to += delta
        lastOffset = offset
        text.copyToArray(res, to)
        to += text.length
      }
      assert(contents.length - from == reslen - to)
      Array.copy(contents, from, res, to, contents.length - from)
      res
    }
  }

  /** Compute an instrumented version of a sourcefile.
   *  @param source  The given sourcefile.
   *  @param line    The line up to which results should be printed, -1 = whole document.
   *  @return        A pair consisting of
   *                  - the fully qualified name of the first top-level object definition in the file.
   *                    or "" if there are no object definitions.
   *                  - the text of the instrumented program which, when run,
   *                    prints its output and all defined values in a comment column.
   */
  protected def instrument(source: SourceFile, line: Int): (String, Array[Char]) = {
    val tree = typedTree(source, true)
    val endOffset = if (line < 0) source.length else source.lineToOffset(line + 1)
    val patcher = new Patcher(source.content, endOffset)
    patcher.traverse(tree)
    (patcher.objectName, patcher.result)
  }
}
