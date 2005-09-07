/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import java.io.PrintWriter;

abstract class Printers {
  val global: Global;
  import global._;
  import global.icodes.opcodes._;
  import global.icodes._;

  class TextPrinter(out: PrintWriter) {
    var margin = 0;

    final val TAB = 2;

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


    def printClass(cls: IClass): Unit = {
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
      print(": "); print(m.symbol.info.resultType); println(" {");
      printCode(m.code);
      println("}");
    }

    def printParam(p: Symbol): Unit = {
      print(p.name); print(": "); print(p.info);
    }

    def printCode(code: Code): Unit = {
      code traverse printBlock;
    }

    def printBlock(bb: BasicBlock): Unit = {
      print(bb.label); print(": "); indent; println;
      bb traverse printInstruction;
      undent; println;
    }

    def printInstruction(i: Instruction): Unit = {
      println(i.toString());
    }
  }
}
