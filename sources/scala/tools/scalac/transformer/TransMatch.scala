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

import scalac.transformer.matching.PartialMatcher ;
import scalac.transformer.matching.PatternMatcher ;
import scalac.transformer.matching.TestRegTraverser ;
import scalac.transformer.matching.AlgebraicMatcher ;
/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger, Burak Emir
 *  @version    1.1
 */
package scala.tools.scalac.transformer {

class TransMatch( global:scalac_Global )
  extends scalac_transformer_OwnerTransformer( global ) {

  var cunit:Unit = null;

  override def apply( cunit:Unit ):unit = {
    this.cunit = cunit;
    super.apply( cunit );
  }

  def  transform( root:Tree, cases:Array[Tree$CaseDef], restpe:Type ):Tree = {
    if( global.newMatch ) {
      throw new ApplicationError("not implemented");
    };
    var containsReg = false;
    var i = 0;
    while (i < cases.length) {
      containsReg = TestRegTraverser.apply(cases( i )) || containsReg;
      var nilvars:Set  = TestRegTraverser.getNilVariables();
      if( !nilvars.isEmpty() ) {
        val newBody = new Array[Tree]( nilvars.size() );
        var j=0;
        var it:Iterator = nilvars.iterator();
        while( it.hasNext() ) {
          val v:Symbol = it.next().asInstanceOf[ Symbol ];
          val n = gen.mkNil(cases(i).pos);
          newBody( j ) = gen.ValDef(v, n);
          j = j + 1;
        }
        cases(i).body = gen.mkBlock( newBody, cases(i).body );
      }
      i = i+1;
    }
    if( containsReg ) {
      val am = new AlgebraicMatcher( cunit );
      val matcher = new PartialMatcher( currentOwner, root, restpe );
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
    /** evil hack. OwnerTransformer should have this function */
    def transform1(ts:Array[Tree$CaseDef]):Array[Tree$CaseDef] =  {
      var i = 0;
      while( i < ts.length ) {
        val t = transform(ts( i ));
        if (t != ts( i )) {
          val res = new Array[Tree$CaseDef](ts.length);
          System.arraycopy(ts, 0, res, 0, i);
          res( i ) = t.asInstanceOf[Tree$CaseDef];
          i = i + 1;
          while(i < ts.length) {
            res( i ) = transform(ts( i )).asInstanceOf[Tree$CaseDef];
            i = i + 1
          };
          return res;
        };
        i = i + 1;
      }
      return ts;
    }

   override def transform( tree:Tree ):Tree = {
     if (tree == null)
       return null;
     else
       tree match {
         case Tree$Apply(Tree$Select( receiver, Names.match ), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Tree$Visitor( cases ) =>
                 return transform(transform(receiver), transform1(cases), tree.getType());
             }
           return tree;

         case Tree$Apply(Tree$TypeApply(Tree$Select( receiver, Names.match ), targs), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Tree$Visitor( cases ) =>
                 return transform(transform(receiver), transform1(cases), tree.getType());
             }
           return tree;
         case _ =>
           super.transform(tree);
       }
   }
}

}
