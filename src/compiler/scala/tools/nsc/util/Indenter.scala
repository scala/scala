package scala.tools.nsc
package util

import java.io.PrintStream

class Indenter(var stringFn: Any => String) {
  def this() = this("" + _)
  def out: PrintStream = System.out

  var indentSpaces = 2
  var isSorted     = false
  var openString   = ""
  var closeString  = ""

  def braces: this.type = {
    openString = " {"
    closeString = "}"
    this
  }
  def sorted: this.type = { isSorted = true ; this }
  def stringify(fn: Any => String): this.type = {
    stringFn = fn
    this
  }

  def atStartOfLine = startOfLine
  private var indentLevel = 0
  private var startOfLine = true
  def indent: this.type = { indentLevel += 1 ; this }
  def undent: this.type = { indentLevel -= 1 ; this }
  def currentIndent = " " * indentLevel * indentSpaces
  def printIndent() = {
    out.print(currentIndent)
    startOfLine = true
  }

  // Execute the given body indented one level further.
  def >>[T](body: => T): T = {
    indentLevel += 1
    try body
    finally indentLevel -= 1
  }

  def openIndent(token: Any) {
    print(token + "\n")
    indent
    printIndent()
  }
  def closeIndent(token: Any) {
    print("\n")
    undent
    printIndent()
    print(token)
  }
  def finishLine(token: Any) {
    print(token)
    printIndent()
  }
  def nextIndent(endOfLine: Any) = finishLine(endOfLine)

  def block(label: String)(body: => Unit) {
    if (label != "" || openString != "")
      pp(label + openString)

    this >> body

    if (closeString != "")
      pp(closeString)
  }
  def print(x: Any) = {
    out print stringFn(x)
    out.flush()
    startOfLine = false
  }
  def pps(xs: TraversableOnce[Any]) {
    if (isSorted) xs.toSeq.sortBy("" + _) foreach pp
    else xs foreach pp
  }
  def pp(x: Any) {
    printIndent()
    out println stringFn(x)
    out.flush()
    startOfLine = false
  }
}
