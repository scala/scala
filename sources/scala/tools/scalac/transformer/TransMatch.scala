/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import java.io._;
import java.util._;
import scalac.{Global => scalac_Global};
import scalac._;
import scalac.ast._;
import scalac.symtab._;
import scalac.util._;       // Names
import Tree._;

import scalac.transformer.{ OwnerTransformer => scalac_transformer_OwnerTransformer };

import scalac.transformer.matching.PatternMatcher ;
import scalac.transformer.matching.TestRegTraverser ;
import scalac.transformer.matching.AlgebraicMatcher ;
import scalac.transformer.{ TransMatch$Matcher => scalac_transformer_TransMatch_Matcher }
/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger, Burak Emir
 *  @version    1.1
 */
package scalac.tools.transformer {

class TransMatch( global:scalac_Global )
  extends scalac_transformer_OwnerTransformer( global ) {

  /** container. classes AlgebraicMatcher and SequenceMatcher get input and store their results in here.
  *  resembles the 'Memento' design pattern, could also be named 'Liaison'
  */
  class Matcher( theOwner:Symbol, theRoot:Tree, theResultType:Type )
    extends scalac_transformer_TransMatch_Matcher( theOwner:Symbol, theRoot:Tree, theResultType:Type ) ;
    /*{

    // owner of the code we create (input)

    var owner: Symbol =  {
      if( theOwner == Symbol.NONE ) error("owner none");
      owner
    };

    // the selector value (input)

    var selector: Tree =  {
      if(( theRoot == null )&&( theRoot.getType() == null )) error("root[.type] null");
      theRoot
    };

    // type of the result of the whole match (input)

    var resultType: Type = {
      if( theResultType == Type.NoType ) error("result type?");
      theResultType
    };

    // tree representing the matcher (output)

    var tree: Tree ;

    var pos: int = theRoot.pos;

    var varMap:java.util.HashMap ;    // needed in LeftTracerInScala
    var stms:Array[Tree] ;      // needed in LeftTracerInScala

  }
    */
  var cunit:Unit = null;

  override def apply( cunit:Unit ):unit = {
    this.cunit = cunit;
    super.apply( cunit );
  }

  protected def  transform( root:Tree, cases:Array[Tree$CaseDef], restpe:Type ):Tree = {
    var containsReg = false;
    var i = 0;
    while (i < cases.length) {
      containsReg = TestRegTraverser.apply(cases( i )) || containsReg;
      var nilvars:Set  = TestRegTraverser.getNilVariables();
      if( !nilvars.isEmpty() ) {
        //System.err.println("nilvars present");
        val newBody = new Array[Tree]( nilvars.size() + 1 );
        var j=0;
        var it:Iterator = nilvars.iterator();
        while( it.hasNext() ) {
          val v:Symbol = it.next().asInstanceOf[ Symbol ];
          val n = gen.mkNil(cases(i).pos);
          newBody.update( {j = j + 1; j} , gen.ValDef(v, n));
        }
        newBody.update( newBody.length - 1, cases(i).body);
        cases(i).body = gen.mkBlock( newBody );
      }
      i = i+1;
    }
    if( containsReg ) {
      val am = new AlgebraicMatcher( cunit );
      val matcher = new Matcher( currentOwner, root, restpe );
      am.construct( matcher, cases.asInstanceOf[ Array[Tree] ] );
      matcher.tree
    } else {
      val pm = new PatternMatcher( cunit, root, currentOwner, restpe );
      pm.construct( cases.asInstanceOf[ Array[Tree] ] );
      if (global.log()) {
        global.log("internal pattern matching structure");
        pm.print();
      }
      pm.toTree();
    }
  }

   override def transform( tree:Tree ):Tree = {
     if (tree == null)
       null;
     else
       tree match {
         case Tree$Apply(Tree$Select( receiver, Names.match ), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Tree$Visitor( cases ) =>
                 transform(transform(receiver), cases, tree.getType());
             }
           tree;
         case Tree$Apply(Tree$TypeApply(Tree$Select( receiver, Names.match ), targs), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Tree$Visitor( cases ) =>
                 transform(transform(receiver), cases, tree.getType());
             };
           tree;
         case _ =>
           super.transform(tree);
       }
   }
}

}
