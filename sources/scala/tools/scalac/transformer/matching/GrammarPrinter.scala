package scala.tools.scalac.transformer.matching ;

import scala.runtime.matching.Grammar ;

object GrammarPrinter {
  def toString( gram:Grammar ) = {
    "Grammar("+gram.treeTransitions+",\n"+gram.hedgeTransitions+",\n"+{
      var k = 1;
      val sb = new java.lang.StringBuffer();
      for( val y <- Iterator.fromArray( gram.vars ) ) {
        sb.append("case "+k+": max var ="+y);
        k = k + 1;
      }
      sb.toString()
    }+")\n";
  }
}
