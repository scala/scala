/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.util.Position

trait Printers { self: ICodes =>
  import global._
  import global.icodes.opcodes._
  import global.icodes._

  class TextPrinter(writer: PrintWriter, lin: Linearizer) {
    private var margin = 0
    private var out = writer

    final val TAB = 2

    def setWriter(w: PrintWriter) { out = w }

    def indent() { margin += TAB }
    def undent() { margin -= TAB }

    def print(s: String) { out.print(s) }
    def print(o: Any) { print(o.toString()) }

    def println(s: String) {
      print(s);
      println
    }

    def println() {
      out.println()
      var i = 0
      while (i < margin) {
        print(" ");
        i += 1
      }
    }

    def printList[A](l: List[A], sep: String): Unit = l match {
      case Nil =>
      case x :: Nil => print(x)
      case x :: xs  => print(x); print(sep); printList(xs, sep)
    }

    def printList[A](pr: A => Unit)(l: List[A], sep: String): Unit = l match {
      case Nil =>
      case x :: Nil => pr(x)
      case x :: xs  => pr(x); print(sep); printList(pr)(xs, sep)
    }

    def printClass(cls: IClass) {
      print(cls.symbol.toString()); print(" extends ");
      printList(cls.symbol.info.parents, ", ");
      indent; println(" {");
      println("// fields:");
      cls.fields.foreach(printField); println;
      println("// methods");
      cls.methods.foreach(printMethod);
      undent; println;
      println("}")
    }

    def printField(f: IField) {
      print(f.symbol.keyString); print(" ");
      print(f.symbol.nameString); print(": ");
      println(f.symbol.info.toString());
    }

    def printMethod(m: IMethod) {
      print("def "); print(m.symbol.name);
      print("("); printList(printParam)(m.params, ", "); print(")");
      print(": "); print(m.symbol.info.resultType)

      if (!m.isAbstractMethod) {
        println(" {")
        println("locals: " + m.locals.mkString("", ", ", ""))
        println("startBlock: " + m.code.startBlock)
        println("blocks: " + m.code.blocks.mkString("[", ",", "]"))
        println
        lin.linearize(m) foreach printBlock
        println("}")

        indent; println("Exception handlers: ")
        m.exh foreach printExceptionHandler

        undent; println
      } else
        println
    }

    def printParam(p: Local) {
      print(p.sym.name); print(": "); print(p.sym.info);
      print(" ("); print(p.kind); print(")")
    }

    def printExceptionHandler(e: ExceptionHandler) {
      indent;
      println("catch (" + e.cls.simpleName + ") in " + e.covered.toSeq.sortBy(_.label) + " starting at: " + e.startBlock);
      println("consisting of blocks: " + e.blocks);
      undent;
      println("with finalizer: " + e.finalizer);
//      linearizer.linearize(e.startBlock) foreach printBlock;
    }

    def printBlock(bb: BasicBlock) {
      print(bb.label)
      if (bb.loopHeader) print("[loop header]")
      print(": ");
      if (settings.debug.value) print("pred: " + bb.predecessors + " succs: " + bb.successors + " flags: " + bb.flagsString)
      indent; println
      bb.toList foreach printInstruction
      undent; println
    }

    def printInstruction(i: Instruction) {
//      if (settings.Xdce.value)
//        print(if (i.useful) "   " else " * ");
      if (i.pos.isDefined) print(i.pos.line.toString + "\t") else print("?\t")
      println(i.toString())
    }
  }
}
