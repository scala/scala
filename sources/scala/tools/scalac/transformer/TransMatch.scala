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


//import scalac.transformer.matching.PatternMatcher ;
//import scalac.transformer.matching.AlgebraicMatcher ;

/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger, Burak Emir
 *  @version    1.1
 */
package scala.tools.scalac.transformer {

  import matching.PatternMatcher ;
  import matching.PartialMatcher ;
//import matching.FullRegularTranslator ;

class TransMatch( global:scalac_Global )
  extends scalac_transformer_OwnerTransformer( global ) {

    import Tree._ ;

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
      case Alternative(_)          =>  true
      case Bind( n, pat1 )              =>
        TreeInfo.isNameOfStarPattern( n )
      || TreeInfo.isEmptySequence( pat1 )
      || isRegular( pat1 )
      case Ident(n)                =>  false
      case Sequence( trees )       =>
        ( trees.length == 0 ) || isRegular( trees );
      case Apply( fn, trees )      =>
        isRegular( trees ) &&
      !((trees.length == 1) && TreeInfo.isEmptySequence( trees( 0 )))
      case Literal(_)              => false;
      case Select(_,_)             => false;
      case Typed(_,_)              => false;
      case _ => error("in TransMatch.isRegular phase: unknown node"+pat.getClass());
    }

    def nilVariables( pat:Tree ) = {
      var res:scala.List[Symbol] = Nil;
      def getNilVars1( ps:Array[Tree] ):scala.Unit = {
        val z:Seq[Tree] = ps; z.elements.foreach( x => getNilVars( x ));
      }
      def getNilVars( p:Tree ):scala.Unit = p match {
        case Alternative( _ )  =>  /* no bind allowed! */
        case Bind( _, pat )    =>
	  getNilVars(pat);
	  if( TreeInfo.isEmptySequence( pat ) )
	    res = p.symbol() :: res;
	case Ident(_)          =>
	case Sequence( trees ) => getNilVars1( trees )
        case Apply( _,  args ) => getNilVars1( args )
        case Literal(_)        =>
        case Select(_,_)       =>
        case Typed(_,_)        =>
        case _ => error("in TransMatch.nilVariables: unknown node"+pat.getClass());
      }
      getNilVars( pat );
      res
    }

    // 2do: remove binds from pattern
    def handleNilVariables( cse: CaseDef ): Unit = {
      val nilvars = nilVariables(cse.pat);
      if( !nilvars.isEmpty ) {
        val newBody = new Array[Tree]( nilvars.length );
        var j=0;
        for( val v <- nilvars.elements ) {
          val n = gen.mkNil( cse.pos );
          newBody( j ) = gen.ValDef( v, n );
          j = j + 1;
        }
        cse.body = gen.mkBlock( newBody, cse.body );
      }
    }

    //val bsf = new scala.util.automaton.BerrySethi[ matching.PatternTest ]( pe );

  def  transform( root:Tree, cases:Array[CaseDef], restpe:Type ):Tree = {

    if( global.newMatch ) {
      //val fm = new FullRegularTranslator( global );
      //val gram = fm.MakeGrammar( scala.Iterator.fromArray( cases ) );
      //Console.println("writing out the grammar to /tmp/encodedGrammar.bin");
      ////val f = new FileOutputStream(new File("/tmp/encodedGrammar.bin"));
      ////f.write( gram.encode );
      ////f.close();
      //// val gram = Predef.decode( Predef.Array[] );
      //Console.println( gram.encode);

      throw new ApplicationError("not impl.");
    };

    var containsReg = false;
    var i = 0;
    while (i < cases.length) {
      if(isRegular(cases( i ).pat)) {
        containsReg = true;
        handleNilVariables(cases( i ));
      }
      i = i+1;
    }
    if( containsReg ) {
      /*
      val pe = new matching.PatternExp(global.definitions); // TEST
      var j = 0;
      val pat = new Array[pe.RegExp](cases.length);
      while( j < cases.length) {
        pat(j) = pe.fromTree(cases(j).pat);
        j = j + 1;
      } // TEST
      */
      val am = new matching.AlgebraicMatcher( cunit );
      val matcher = new PartialMatcher( currentOwner, root, restpe );
      am.construct( matcher, cases.asInstanceOf[ Array[Tree] ] );
      matcher.tree
    } else {
      val pm = new matching.PatternMatcher( cunit );
      pm.initialize(root, currentOwner, restpe, true );
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
    def transform1(ts:Array[CaseDef]):Array[CaseDef] =  {
      var i = 0;
      while( i < ts.length ) {
        val t = transform(ts( i ));
        if (t != ts( i )) {
          val res = new Array[CaseDef](ts.length);
          System.arraycopy(ts, 0, res, 0, i);
          res( i ) = t.asInstanceOf[CaseDef];
          i = i + 1;
          while(i < ts.length) {
            res( i ) = transform(ts( i )).asInstanceOf[CaseDef];
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
         case Apply(Select( receiver, Names._match ), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Visitor( cases ) =>
                 return transform(transform(receiver), transform1(cases), tree.getType());
             }
           return tree;

         case Apply(TypeApply(Select( receiver, Names._match ), targs), args) =>
           if ((args != null) && (args.length == 1))
             args( 0 ) match {
               case Visitor( cases ) =>
                 return transform(transform(receiver), transform1(cases), tree.getType());
             }
           return tree;
         case _ =>
           super.transform(tree);
       }
   }
}

}
