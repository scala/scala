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

import scalac.transformer.{ OwnerTransformer => scalac_transformer_OwnerTransformer };

import scalac.transformer.matching.PartialMatcher ;
import scalac.transformer.matching.PatternMatcher ;
import scalac.transformer.matching.AlgebraicMatcher ;

/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger, Burak Emir
 *  @version    1.1
 */
package scala.tools.scalac.transformer {


import matching.FullRegularTranslator ;
import matching.GrammarTool ; //DEBUG

class TransMatch( global:scalac_Global )
  extends scalac_transformer_OwnerTransformer( global ) {

  var cunit:CompilationUnit = null;

  override def apply( cunit:CompilationUnit ):unit = {
    this.cunit = cunit;
    super.apply( cunit );
  }

    def isRegular(ps:Array[Tree]):Boolean = {
      var res = false;
      val pats = scala.Iterator.fromArray( ps );
      while (!res && pats.hasNext )
        res = isRegular( pats.next );
      res
    }

    def isRegular(pat:Tree):Boolean = pat match {
      case Tree$Alternative(_)          =>  true
      case Tree$Bind( n, pat1 )              =>
        TreeInfo.isNameOfStarPattern( n )
      || TreeInfo.isEmptySequence( pat1 )
      || isRegular( pat1 )
      case Tree$Ident(n)                =>  false
      case Tree$Sequence( trees )       =>
        ( trees.length == 0 ) || isRegular( trees );
      case Tree$Apply( fn, trees )      =>
        isRegular( trees ) &&
      !((trees.length == 1) && TreeInfo.isEmptySequence( trees( 0 )))
      case Tree$Literal(_)              => false;
      case Tree$Select(_,_)             => false;
      case Tree$Typed(_,_)              => false;
      case _ => error("in TransMatch.isRegular phase: unknown node"+pat.getClass());
    }

    def nilVariables( pat:Tree ) = {
      var res:scala.List[Symbol] = Nil;
      def getNilVars1( ps:Array[Tree] ):scala.Unit = {
        val z:Seq[Tree] = ps; z.elements.foreach( x => getNilVars( x ));
      }
      def getNilVars( p:Tree ):scala.Unit = p match {
        case Tree$Alternative( _ )  => /* no bind allowed! */
        case Tree$Bind( _, pat )     =>
	  if( TreeInfo.isEmptySequence( pat ) )
	    res = p.symbol() :: res;
	  getNilVars(pat);
	case Tree$Ident(_)          =>
	case Tree$Sequence( trees ) => getNilVars1( trees )
        case Tree$Apply( _,  args ) => getNilVars1( args )
        case Tree$Literal(_)        =>
        case Tree$Select(_,_)       =>
        case Tree$Typed(_,_)        =>
        case _ => error("in TransMatch.nilVariables: unknown node"+pat.getClass());
      }
      getNilVars( pat );
      res
    }


    //val bsf = new scala.util.automaton.BerrySethi[ matching.PatternTest ]( pe );

  def  transform( root:Tree, cases:Array[Tree$CaseDef], restpe:Type ):Tree = {

    if( global.newMatch ) {
      val fm = new FullRegularTranslator( global );
      val gram = fm.MakeGrammar( scala.Iterator.fromArray( cases ) );
      Console.println("writing out the grammar to /tmp/encodedGrammar.bin");
      //val f = new FileOutputStream(new File("/tmp/encodedGrammar.bin"));
      //f.write( gram.encode );
      //f.close();
      // val gram = Predef.decode( Predef.Array[] );
      Console.println( GrammarTool.encode( gram ));

      throw new ApplicationError("not impl.");
    };

    var containsReg = false;
    var i = 0;
    while (i < cases.length) {
      containsReg = isRegular(cases( i ).pat) || containsReg;

      var nilvars = nilVariables(cases( i ).pat);
      if( !nilvars.isEmpty ) {
        val newBody = new Array[Tree]( nilvars.length );
        var j=0;
        for( val v <- nilvars.elements ) {
          val n = gen.mkNil( cases( i ).pos );
          newBody( j ) = gen.ValDef( v, n );
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
      try{
      pm.construct( cases.asInstanceOf[ Array[Tree] ] );
      } catch {
        case e:Throwable =>
          Console.println("failed on pats"+scala.Iterator.fromArray(cases).toList.mkString("","\n","")+", message\n"+e.getMessage())
      }
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
         case Tree$Apply(Tree$Select( receiver, Names._match ), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Tree$Visitor( cases ) =>
                 return transform(transform(receiver), transform1(cases), tree.getType());
             }
           return tree;

         case Tree$Apply(Tree$TypeApply(Tree$Select( receiver, Names._match ), targs), args) =>
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
