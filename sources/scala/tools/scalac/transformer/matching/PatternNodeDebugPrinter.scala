/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */


import scala.tools.util.Position;
import scalac._;
import scalac.ast._;
import scalac.atree.AConstant;
import scalac.util._;
import scalac.symtab._;
//import scalac.transformer.matching.CodeFactory;
//import scalac.transformer.matching.PatternNodeCreator;
//import scalac.transformer.matching.PatternNode;
//import scalac.transformer.matching.CaseEnv;
//import scalac.transformer.matching.PatternTool;
//import PatternNode._;
import Tree._;

package scala.tools.scalac.transformer.matching {

  class PatternNodeDebugPrinter() {

    def print(patNode: PatternNode, indent: String): Unit = {

      def newIndent(s: String) = {
        val removeBar: Boolean = (null == patNode.or);
        val sb = new StringBuffer();
        sb.append(indent);
        if (removeBar)
          sb.setCharAt(indent.length() - 1, ' ');
        var i = 0; while (i < s.length()) {
          sb.append(' ');
          i = i + 1
        }
        sb.toString()
      }

      if (patNode == null)
        System.out.println(indent + "NULL");
      else
        patNode match {

          case _h: Header =>
            val selector = _h.selector;
            val next = _h.next;
            Console.println(indent + "HEADER(" + patNode.getTpe() +
                            ", " + selector + ")");
            print(patNode.or, indent + "|");
            if (next != null)
              print(next, indent);

          case ConstrPat(casted) =>
            val s = "-- " + patNode.getTpe().symbol().name +
            "(" + patNode.getTpe() + ", " + casted + ") -> ";
            val nindent = newIndent(s);
            Console.println(nindent + s);
            print(patNode.and, nindent);
            if (patNode.or != null)
              print(patNode.or, indent);

          case SequencePat( casted, plen ) =>
            val s = "-- " + patNode.getTpe().symbol().name + "(" +
                    patNode.getTpe() +
                    ", " + casted + ", " + plen + ") -> ";
            val nindent = newIndent(s);
            Console.println(indent + s);
            print(patNode.and, nindent);
            if (patNode.or != null)
              print(patNode.or, indent);

          case DefaultPat() =>
            Console.println(indent + "-- _ -> ");
            print(patNode.and, indent.substring(0, indent.length() - 1) +
                "         ");
            if (patNode.or != null)
              print(patNode.or, indent);

          case ConstantPat(value) =>
            val s = "-- CONST(" + value + ") -> ";
            val nindent = newIndent(s);
            Console.println(indent + s);
            print(patNode.and, nindent);
            if (patNode.or != null)
              print(patNode.or, indent);

          case VariablePat(tree) =>
            val s = "-- STABLEID(" + tree + ": " + patNode.getTpe() + ") -> ";
            val nindent = newIndent(s);
            Console.println(indent + s);
            print(patNode.and, nindent);
            if (patNode.or != null)
              print(patNode.or, indent);

          /*
          case AltPat(header) =>
            Console.println(indent + "-- ALTERNATIVES:");
            print(header, indent + "   * ");
            print(patNode.and, indent + "   * -> ");
            if (patNode.or != null)
              print(patNode.or, indent);
*/
          case _b:Body =>
            if ((_b.guard.length == 0) && (_b.body.length == 0))
              Console.println(indent + "true");
            else
              Console.println(indent + "BODY(" + _b.body.length + ")");

        }
    } // print

  } // class PatternNodeDebugPrinter

}
