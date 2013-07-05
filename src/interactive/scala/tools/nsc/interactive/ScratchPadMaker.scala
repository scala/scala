package scala
package tools.nsc
package interactive

import scala.reflect.internal.util.{SourceFile, BatchSourceFile, RangePosition}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.Chars.{isLineBreakChar, isWhitespace}
import ast.parser.Tokens._

@deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
trait ScratchPadMaker { self: Global =>

  import definitions._

  private case class Patch(offset: Int, text: String)

  private class Patcher(contents: Array[Char], lex: LexicalStructure, endOffset: Int) extends Traverser {
    var objectName: String = ""

    private val patches = new ArrayBuffer[Patch]
    private val toPrint = new ArrayBuffer[String]
    private var skipped = 0
    private var resNum: Int = -1

    private def nextRes(): String = {
      resNum += 1
      "res$"+resNum
    }

    private def nameType(name: String, tpe: Type): String = {
      // if name ends in symbol character, add a space to separate it from the following ':'
      val pad = if (Character.isLetter(name.last) || Character.isDigit(name.last)) "" else " "
      name+pad+": "+tpe
    }

    private def nameType(sym: Symbol): String = nameType(sym.name.decoded, sym.tpe)

    private def literal(str: String) = "\"\"\""+str+"\"\"\""

    private val prologue = ";import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{"

    private val epilogue = "}"

    private def applyPendingPatches(offset: Int) = {
      if (skipped == 0) patches += Patch(offset, prologue)
      for (msg <- toPrint) patches += Patch(offset, ";System.out.println("+msg+")")
      toPrint.clear()
    }

    /** The position where to insert an instrumentation statement in front of given statement.
     *  This is at the latest `stat.pos.start`. But in order not to mess with column numbers
     *  in position we try to insert it at the end of the previous token instead.
     *  Furthermore, `(' tokens have to be skipped because they do not show up
     *  in statement range positions.
     */
    private def instrumentPos(start: Int): Int = {
      val (prevToken, prevStart, prevEnd) = lex.locate(start - 1)
      if (prevStart >= start) start
      else if (prevToken == LPAREN) instrumentPos(prevStart)
      else prevEnd
    }

    private def addSkip(stat: Tree): Unit = {
      val ipos = instrumentPos(stat.pos.start)
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
                val offset = instrumentPos(stat.pos.start)
                patches += Patch(offset, "val " + resName + " = ")
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
        if (topLevel) {
          objectName = tree.symbol.fullName
          body foreach traverseStat
          if (skipped != 0) { // don't issue prologue and epilogue if there are no instrumented statements
            applyPendingPatches(skipped)
            patches += Patch(skipped, epilogue)
          }
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

  class LexicalStructure(source: SourceFile) {
    val token = new ArrayBuffer[Int]
    val startOffset = new ArrayBuffer[Int]
    val endOffset = new ArrayBuffer[Int]
    private val scanner = new syntaxAnalyzer.UnitScanner(new CompilationUnit(source))
    scanner.init()
    while (scanner.token != EOF) {
      startOffset += scanner.offset
      token += scanner.token
      scanner.nextToken()
      endOffset += scanner.lastOffset
    }

    /** @return token that starts before or at offset, its startOffset, its endOffset
     */
    def locate(offset: Int): (Int, Int, Int) = {
      var lo = 0
      var hi = token.length - 1
      while (lo < hi) {
        val mid = (lo + hi + 1) / 2
        if (startOffset(mid) <= offset) lo = mid
        else hi = mid - 1
      }
      (token(lo), startOffset(lo), endOffset(lo))
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
    val tree = typedTree(source, forceReload = true)
    val endOffset = if (line < 0) source.length else source.lineToOffset(line + 1)
    val patcher = new Patcher(source.content, new LexicalStructure(source), endOffset)
    patcher.traverse(tree)
    (patcher.objectName, patcher.result)
  }
}
