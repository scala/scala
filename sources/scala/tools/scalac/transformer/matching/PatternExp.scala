import scalac.ast._ ;
import scalac.atree.AConstant ;
import scalac.symtab.{ Definitions, Modifiers, Symbol, Type} ;
import scalac.util.Name ;

package scala.tools.scalac.transformer.matching {

  import scala.util.regexp.{ PointedHedgeExp, WildcardBase };

  class PatternExp(defs: Definitions)
  extends PointedHedgeExp[PatternTest] with WildcardBase {
    type regexp = RegExp ;

    case class Binding(vble:Symbol, re:regexp) extends Meta( re ) ;

    def fromTrees( ts:Array[Tree] ):Seq[RegExp] = {
      var nts: List[RegExp] = Nil;
      val it = Iterator.fromArray( ts );
      while( it.hasNext )
        nts = fromTree( it.next ) :: nts;
      nts.reverse
    }

    def fromTree( t:Tree ):RegExp = {
      import Tree._ ;
      t match {
        case Alternative(choices:Array[Tree]) =>
          Alt( fromTrees(choices):_* )

        case Apply(s:Ident, args) if (s.symbol() == defs.PATTERN_WILDCARD) =>
          Node(WildcardTest, Sequ(fromTrees( args ):_*))

        case x @ Apply(_, args) =>
          if (isSeqApply( x ))          // List(1,2,3)
            Node(TypeTest( x.getType() ), Sequ(fromTrees( args ):_*))

          else if (isObjectRef( x ))  // uncurry: foo.bar => foo.bar()
            Node(EqualsValue( t ), Eps);

           else                        // case class constructor
             Node(Constructor( t.getType() ), Sequ(fromTrees( args ):_*));

        case Bind(n: Name, t:Tree) =>
          if( TreeInfo.isNameOfStarPattern( n ) )
            Star(fromTree( t ))
          else
            Binding( t.symbol(), fromTree( t ))

        case Literal( c ) =>
            Node(EqualsValue( t ), Star(Wildcard))

        case Ident( n:Name ) =>
          if( t.symbol() == defs.PATTERN_WILDCARD )
            Wildcard
          else if( TreeInfo.isNameOfStarPattern( n ) )
            Eps;
          else Node(EqualsValue( t ), Star(Wildcard))

        case Select(_,_) =>
          Node(EqualsValue( t ), Star(Wildcard))

        case Sequence( trees ) =>
          Sequ(fromTrees( trees ):_*);

        case Typed(_,_) =>
          Node(TypeTest( t.getType() ), Eps)

      }
    }

    /** these are treated like constant value */
    final def isObjectRef( t:Tree.Apply ):Boolean = {
      val sym = t.fun.symbol();
      (sym != null)
      && sym.isStable()
      && !(sym.isModule()
           && ((sym.flags & Modifiers.CASE) != 0));
    }

    final def isSeqApply( tree:Tree.Apply  ):Boolean = {
      ((tree.getType().symbol().flags & Modifiers.CASE) == 0)
      && ( tree.args.length == 1 )
      && tree.args(0).isInstanceOf[Tree.Sequence]
    }

    final def isSequenceValued( p:RegExp ):Boolean = p match {
      case x:Alt =>
        val it = x.rs.elements;
        while (it.hasNext)
          if (isSequenceValued( it.next ))
            return true;
        return false;
      case Binding(_,r)   => isSequenceValued( r )
      case Eps            => true;
      case _:Node         => false;
      case _:Sequ         => true;
      case TopIter(l, r)  => isSequenceValued( l )||isSequenceValued( r );
      case Point          => false;
      case Wildcard       => false;
    }

    /** returns width */
    private def getLowerBoundWidth( it:Iterator[RegExp] ):Int = {
      if( !it.hasNext ) 0 else {
        var wid = minimalWidth( it.next );
        for( val h <- it ) {
          val hw = minimalWidth( h );
          if( hw < wid ) {
            wid = hw
          }
        }
        wid
      }
    }

    /** @todo translate this from ems pattern
    */
    def minimalWidth( pat:RegExp ):Int = pat match {
      case x:Alt            => getLowerBoundWidth( x.rs.elements );
      case _:Node           => 1
      case x:Sequ           =>
        x.rs.elements.foldLeft (0) {
          (s:int,x:RegExp) => s + minimalWidth( x )
        }
      case Binding( _, r )  => minimalWidth( r );
      case _                => 0

    }
  }
}
