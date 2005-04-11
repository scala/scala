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

import scala.tools.scalac.util.NewArray;
import scalac.transformer.{ OwnerTransformer => scalac_transformer_OwnerTransformer };

//import scalac.transformer.matching.PatternNode ;
//import scalac.transformer.matching.SequenceMatcher ;
//import PatternNode._ ;

package scala.tools.scalac.transformer.matching {

  /** Matthias' algebraic pattern matcher, with some things factored out,
   *  then translated to scala
   *  @author Matthias Zenger, Burak Emir
   */
  class AlgebraicMatcher(unit: CompilationUnit) extends PatternMatcher(unit) {

/*
import scalac.*;
import scalac.ast.*;
import scalac.atree.AConstant;
import scalac.symtab.*;

import Tree.*;

import scalac.util.Name ;
import scalac.util.Names ;

import java.util.Vector ;
import java.util.Iterator ;
*/

      var _m:PartialMatcher = _;

    this.delegateSequenceMatching = true;
    this.optimize = false;

    /** constructs an algebraic pattern matcher from cases
     */
    def construct(m: PartialMatcher, cases:Array[Tree]): Unit = {
      construct(m, cases, true);
    }

    /** constructs an algebraic pattern matcher from cases
     */
    def construct(m: PartialMatcher, cases: Array[Tree], doBinding: Boolean): Unit = {
      this._m = m;
      super.initialize( _m.selector, _m.owner, _m.resultType, doBinding );
      var i = 0; while (i < cases.length) {
        enter( cases( i ) ); //(CaseDef) cases[i], i);
        i = i + 1;
      }
      if (unit.global.log()) {
        unit.global.log("internal pattern matching structure");
        print();
      }
      _m.tree = toTree();
    }


    /** initializes this AlgebraicMatcher, see Matcher.initialize
     void initialize() {}
     */
    /*
    def isStarApply(tree: Tree.Apply): Boolean = {
      val params:Array[Symbol] = tree.fun.getType().valueParams();
      //System.err.println( tree.fun.type.resultType().symbol() );
      (tree.args.length == 1)
      && (tree.getType().symbol().flags & Modifiers.CASE) != 0
      && params.length > 0
      && (params(params.length-1).flags & Modifiers.REPEATED) != 0;
    }
*/
//////////// generator methods

    override def toTree(): Tree = {
      val ts = NewArray.Tree (
        gen.ValDef(root.symbol(), _m.selector ),
        gen.ValDef(resultVar,
                   gen.mkDefaultValue(_m.pos, resultVar.info()) ));
      val res = gen.If( super.toTree(root.and),
                       gen.Ident( _m.pos, resultVar ),
                       cf.ThrowMatchError( _m.pos, _m.resultType ));
      /*
            gen.If(
                _m.pos,
                toTree(root.and),
                gen.Ident( _m.pos, resultVar ),
                cf.ThrowMatchError( _m.resultType ));
        */
      gen.mkBlock(_m.pos, ts, res);
    }

    protected override def toTree(node: PatternNode, selector: Tree): Tree = {
	//System.err.println("AM.toTree called"+node);
      if (node == null)
        gen.mkBooleanLit(_m.pos, false);
      else node.match {
        case SeqContainerPat( _, _ ) =>
	  callSequenceMatcher( node,
			      selector );
        case _ =>
	  super.toTree( node, selector );
      }
    }

    /** collects all sequence patterns and returns the default
     */
    def collectSeqPats(node1: PatternNode, seqPatNodes: Vector, bodies: Vector ): PatternNode = {
      var node = node1;
      var defaultNode: PatternNode = null;
      var exit = false;
      do {
        if( node == null )
          exit=true;//defaultNode = node; // break
        else
          node.match {
            case SeqContainerPat( _, _ ) =>
              seqPatNodes.add( node );
              bodies.add( super.toTree( node.and ) );
              node = node.or;
              exit//defaultNode = node; //               break;

            case _ =>
              defaultNode = node;
          }
      } while (!exit && (null == defaultNode)) ;

      defaultNode;
    }

    def callSequenceMatcher(node: PatternNode, selector: Tree):  Tree = {

      //Console.println("calling sequent matcher for"+node);
      /*    ???????????????????????? necessary to test whether is a Seq?
       gen.If(selector.pos, maybe cf.And( cf.Is(selector, seqpat.type()) )???)
       */

      // translate the _.and subtree of this SeqContainerPat

      val seqPatNodes = new Vector();
      val bodies      = new Vector();

      var defaultNode = collectSeqPats( node, seqPatNodes, bodies );

      val defaultCase = toTree( defaultNode, selector );

      val wordRec = new SequenceMatcher(unit);

      val m = new PartialMatcher( _m.owner, selector, defs.boolean_TYPE() );

      val pats = new Array[Tree]( seqPatNodes.size() );
      val body = new Array[Tree]( seqPatNodes.size() );

      val tmp = bodies.toArray();
      var j = 0;
      val it = seqPatNodes.iterator();
      while(it.hasNext()) {
	pats( j ) = it.next().asInstanceOf[SeqContainerPat].seqpat;
	body( j ) = tmp(j).asInstanceOf[Tree];
	j = j + 1;
      }
      //Tree defaultTree = toTree( node.or, selector ); // cdef.body ;

      wordRec.construct( m, pats, body, defaultCase, doBinding );

      //_m.defs.addAll( m.defs  );

      m.tree;
    }

  }

}
