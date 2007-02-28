/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import java.io.PrintWriter;

import scala.tools.nsc.util.Position;
import scala.tools.nsc.symtab.Flags;

trait Printers requires ICodes {
//  val global: Global;
  import global._;
  import global.icodes.opcodes._;
  import global.icodes._;

  class TextPrinter(writer: PrintWriter, lin: Linearizer) {
    var margin = 0;
    var out = writer;

    final val TAB = 2;

    def setWriter(w: PrintWriter) = (out = w);

    def indent = margin = margin + TAB;
    def undent = margin = margin - TAB;

    def print(s: String) = out.print(s);
    def print(o: Any): Unit = print(o.toString());

    def println(s: String): Unit = {
      print(s);
      println
    }

    def println = {
      out.println();
      var i = 0;
      while (i < margin) {
        print(" ");
        i = i + 1;
      }
    }

    def printList[a](l: List[a], sep: String): Unit = l match {
      case Nil => ();
      case x :: Nil => print(x);
      case x :: xs  => print(x); print(sep); printList(xs, sep);
    }

    def printList[a](pr: a => Unit)(l: List[a], sep: String): Unit = l match {
      case Nil => ();
      case x :: Nil => pr(x);
      case x :: xs  => pr(x); print(sep); printList(pr)(xs, sep);
    }


    private var clazz : IClass = _;
    def printClass(cls: IClass): Unit = {
      this.clazz = cls;
      print(cls.symbol.toString()); print(" extends ");
      printList(cls.symbol.info.parents, ", ");
      indent; println(" {");
      println("// fields:");
      cls.fields.foreach(printField); println;
      println("// methods");
      cls.methods.foreach(printMethod);
      undent; println;
      println("}");
    }

    def printField(f: IField): Unit = {
      print(f.symbol.keyString); print(" ");
      print(f.symbol.nameString); print(": ");
      println(f.symbol.info.toString());
    }

    def printMethod(m: IMethod): Unit = {
      print("def "); print(m.symbol.name);
      print("("); printList(printParam)(m.params.reverse, ", "); print(")");
      print(": "); print(m.symbol.info.resultType);

      if (!m.isDeferred) {
        println(" {");
        println("locals: " + m.locals.mkString("", ", ", ""));
        println("startBlock: " + m.code.startBlock);
        println("blocks: " + m.code.blocks.mkString("[", ",", "]"));
        println;
        lin.linearize(m) foreach printBlock;
        println("}");

        indent;println("Exception handlers: ");
        m.exh foreach printExceptionHandler;

        undent;println;
      } else
        println;
    }

    def printParam(p: Local): Unit = {
      print(p.sym.name); print(": "); print(p.sym.info);
      print(" ("); print(p.kind); print(")");
    }

    def printExceptionHandler(e: ExceptionHandler) = {
      indent;
      println("catch (" + e.cls.simpleName + ") in " + e.covered + " starting at: " + e.startBlock);
      println("consisting of blocks: " + e.blocks);
      undent;
      println("with finalizer: " + e.finalizer);
//      linearizer.linearize(e.startBlock) foreach printBlock;
    }

    def printBlock(bb: BasicBlock): Unit = {
      print(bb.label);
      if (bb.loopHeader) print("[loop header]");
      print(": "); indent; println;
      bb.toList foreach printInstruction;
      undent; println;
    }

    def printInstruction(i: Instruction): Unit = {
//      if (settings.debug.value)
//        print("/* " + Position.line(clazz.cunit.source, i.pos) + " */ ");
      println(i.toString());
    }
  }
}
