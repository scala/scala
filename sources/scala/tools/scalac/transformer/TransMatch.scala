/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import java.io._;
import java.util.HashMap;
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


    //def debugLog(s:String) = {
      //if(currentOwner.toString().indexOf("fetchToken") != -1) {
     //Console.println(s);
      //}
    //}

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
      //case Alternative(_)          =>  true
      case Alternative(trees)          =>
        isRegular(trees)
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
      var res:List[Symbol] = Nil;
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


    /** removes alternative notes by unfolding them, adding casedefs,
     *  and duplication righthand sides
     *
     *  @pre cases are all nonregular
     */
    def removeAlterns(cases: Array[CaseDef]) = {

      //debugLog("removeAlterns called![ currentOwner = "+currentOwner+" [");
      //  var jjj = 0; while (jjj<cases.length) {
      //  debugLog(cases(jjj).toString());
      //  jjj = jjj + 1;
      //}
      //debugLog("]");

      def lst2arr(l:List[Tree]):Array[Tree] = {
        val res = new Array[Tree](l.length);
        val it = l.elements; var i = 0; while(it.hasNext) {
          res(i) = it.next;
          i = i + 1;
        }
        res
      }
      def select(j:int, expanded:Array[List[Tree]]): List[List[Tree]] = {
        if( j == expanded.length )
          List(scala.Nil);
        else {
          val rest:List[List[Tree]] = select(j+1, expanded);
          //expanded(j) map { x:Tree => rest map { xs:List[Tree] => x :: xs }}
          for(val t <- expanded(j);
              val zs <- rest)
            yield t :: zs;
        }
      }

      def newCloner() = {
        val sc = new SymbolCloner();
        sc.owners.put(currentOwner,currentOwner);

        new GenTreeCloner(global, Type.IdMap, sc) {

          override def getSymbolFor(tree:Tree): Symbol = {
            //Console.println("getSymbolFor "+tree.getClass());
            tree match {
              case Bind(_,_) =>
                val symbol = cloner.cloneSymbol(tree.symbol());

              //System.out.println("TreeCloner: Bind"+symbol);
              //System.out.println("TreeCloner: Bind - old owner "+tree.symbol().owner());
              //System.out.println("TreeCloner: Bind - new owner "+symbol.owner());
              //Console.println("in TreeCloner: type = "+symbol.getType());
              symbol.setType(transform(symbol.getType()));
              //Console.println("in TreeCloner: type (post) = "+symbol.getType());
              //System.out.println("done TreeCloner: Bind"+symbol);
              return symbol;

              case _ : ClassDef =>

                  throw new ApplicationError("sorry, cannot compile this correctly in this scala version. Move your classes, fundefs etc out of the pattern body, please, and wait for nsc.");
              case _ : DefDef =>
                throw new ApplicationError("sorry, cannot compile this correctly in this scala version. Move your classes, fundefs etc out of the pattern body, please, and wait for nsc.");
              //  tree.symbol();

              case Ident(_) if tree.symbol() == global.definitions.PATTERN_WILDCARD =>
                tree.symbol()

              case _ =>
                super.getSymbolFor(tree);
            }

          }
        }
      }

      def remove(pat:Tree): List[Tree] = {
        //Console.println("remove("+pat+"), currentOwner = "+currentOwner);
        val res = pat match {
        case Alternative( branches  )  =>  // no bind allowed!
          val z: Seq[Tree] = branches;
          List.flatten(z.toList.map( remove ));
        case b @ Bind( n, pat ) =>
            //Console.println("remove-Bind sym-owner: "+b.symbol().owner());
          remove(pat) match {
            case List(r) if (r eq pat) =>
              List(b);
            case zs =>
              //Console.println("remove-Bind sym: "+b.symbol());
              //Console.println("remove-Bind sym-type: "+b.symbol().getType());
              //Console.println("rb-(case zs)"+zs);
              zs map { x => //Console.println(b.symbol());
                      val r = new ExtBind(b.symbol(),x);
                      r.setType(b.getType());
                      //Console.println("rrrrr ="+ r.getType());
                      r
                    };
          }
	case Sequence( trees ) =>
          val expanded = new Array[List[Tree]](trees.length);
          var i = 0; while( i < trees.length) {
            expanded(i) = remove(trees(i));
            i = i + 1;
          }
          select(0, expanded) map { x:List[Tree] =>
            //Console.println("remove:Sequence, type = "+pat.getType());
            val res = Sequence(lst2arr(x)).setType(pat.getType());
                                   //Console.println("remove:Sequence RETURNS "+res);
                                   res
                                 }

        case Apply( fn,  trees ) =>
          val expanded = new Array[List[Tree]](trees.length);
          var i = 0; while( i < trees.length) {
            expanded(i) = remove(trees(i));
            i = i + 1;
          }

          select(0, expanded) map { x => new Apply(fn, lst2arr(x))
                                   .setType(pat.getType()) }

        case Ident(_)          => List(pat);
        case Literal(_)        => List(pat);
        case Select(_,_)       => List(pat);
        case Typed(_,_)        =>
          List(pat);
        case _ => error("in TransMatch.nilVariables: unknown node"+pat.getClass());


        }
      //Console.println("end remove("+pat+")");
      res
      }
      val zs:Seq[CaseDef] = cases;
      val ncases: List[List[Tree]] = zs.toList map {
        x => x match {
        case CaseDef(pat,guard,body) =>

          //debugLog("removeAlterns - ("+x+")");
          remove(pat) match {
            case List(p) => List(x);
            case pats =>
              //debugLog("removal yields patterns (before trans) in "+cunit);
            //debugLog( pats.mkString("","\n","") );
            pats map {
              npat =>
                val tc = newCloner();
                //Console.println("start cloning! casedef in "+cunit);
                val res = tc.transform(new CaseDef(npat,
                                                   guard.duplicate(),
                                                   body.duplicate()));
              /*Console.println(sc.owners);*/res}
          }
        }
      }

      lst2arr(List.flatten(ncases));
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
    //Console.println("TransMatch: ");
    //for(val ci <- cases) Console.println(ci);
    if( containsReg ) {
      //Console.println("TransMatch: isRegular!");
      /*
      val pe = new matching.PatternExp(global.definitions); // TEST
      var j = 0;
      val pat = new Array[pe.RegExp](cases.length);
      while( j < cases.length) {
        pat(j) = pe.fromTree(cases(j).pat);
        j = j + 1;
      } // TEST
      */
      //debugLog("containsReg!");
      val am = new matching.AlgebraicMatcher( cunit );
      val matcher = new PartialMatcher( currentOwner, root, restpe );
      am.construct( matcher, cases.asInstanceOf[ Array[Tree] ] );
      matcher.tree
    } else {
      //Console.println("TransMatch: NOT regular");
      val pm = new matching.PatternMatcher( cunit );
      pm.initialize(root, currentOwner, restpe, true );
      try{

        val ncases = removeAlterns(cases);
        //if(ncases.length > cases.length) {
        //  debugLog("did some removal!");
        //  var kk = 0; while (kk<ncases.length) {
        //    debugLog(ncases(kk).toString());
        //    kk = kk + 1;
        //  }
        //} else
          //else debugLog("did NOT do removal!");

        pm.construct( ncases.asInstanceOf[Array[Tree]] );
      } catch {
        case e:Throwable =>
          e.printStackTrace();
          Console.print("failed on pats "+scala.Iterator.fromArray(cases).toList.mkString("","\n","")+", message\n"+e.getMessage());
          Console.print("  unit "+cunit);
          Console.println(" with exception:"+e.getMessage());
          //e.printStackTrace();
          exit(-1); //Debug.abort()
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

   override def transform( tree: Tree ):Tree = {
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
