package scala.tools.scalac.transformer.matching ;

import scala.runtime.matching.Grammar ;

//import scalac.ast.{ Tree, TreeGen };
//import scalac.util.Name;

object GrammarTool {

  def toString(gram: Grammar) = {
    "new Grammar("+gram.treeTransitions+",\n"+gram.hedgeTransitions+",\n"+{
      var k = 1;
      val sb = new java.lang.StringBuffer();
      for( val y <- Iterator.fromArray( gram.vars ) ) {
        sb.append("case "+k+": max var ="+y);
        k = k + 1;
      }
      sb.toString()
    }+")\n";
  }

  /*
  private val _Grammar  = Name.fromString("Grammar");
  private val _runtime  = Name.fromString("runtime");
  private val _matching = Name.fromString("matching");

 // convenience methods
  private def _scala(pos: int, name: Name) =
    make.Select( pos, make.Ident( pos, Names.scala ), name);

  private def _scala_runtime(pos: int, name: Name) =
    make.Select( pos, _scala( pos, _xml ), name );

  private def _scala_runtime_matching( pos: int ) = {
    make.Select( pos, _scala_xml( pos, _runtime ), name );

  private def _scala_runtime_matching_Grammar( pos: int ) =
    make.Select( pos, _scala_xml_matching( pos, _Grammar ), name );


  def toTree(gram: Grammar) = {

      gen.New(
        gen.mkApplyTV(
          gen.mkPrimaryConstructorGlobalRef(
            pos,
            defs.TUPLE_CLASS[2]),
          new Type[] { left.getType(), right.getType() },
          new Tree[] { left, right }
        )
      );

    }
    make.New(pos, init);
  }
  */
}
