import scalac.ast._ ;
import scalac.atree.AConstant ;
import scalac.symtab.{ Definitions, Modifiers, Symbol, Type} ;
import scalac.util.Name ;

package scala.tools.scalac.transformer.matching {

  import scala.util.regexp.PointedHedgeExp;

  class PatternExp(defs: Definitions) extends PointedHedgeExp[PatternTest] {
    type regexp = RegExp ;

    case class Binding(vble:Symbol, re:regexp) extends Meta( re ) ;
    case object WildcardNode                  extends RegExp {
      final val isNullable = false;
    }

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
        case Alternative(choices:Array[Tree] ) =>
          Alt( fromTrees(choices):_* )

        case x @ Apply(_, args) =>
          if(isSeqApply( x ))          // List(1,2,3)
            Node(TypeTest( x.getType() ), Sequ(fromTrees( args ):_*))

          else if( isObjectRef( x ) )  // uncurry: foo.bar => foo.bar()
            Node(EqualsValue( t ), Eps);

           else                        // case class constructor
             Node(Constructor( t.getType() ), Sequ(fromTrees( args ):_*));

        case Bind(n: Name, t:Tree) =>
          if( TreeInfo.isNameOfStarPattern( n ) )
            Star(fromTree( t ))
          else
            Binding( t.symbol(), fromTree( t ))

        case Ident( n:Name ) =>
          if( t.symbol() == defs.PATTERN_WILDCARD )
            WildcardNode
          else if( TreeInfo.isNameOfStarPattern( n ) )
            Eps;
          else Node(EqualsValue( t ), Eps)

        case Select(_,_) =>
          Node(EqualsValue( t ), Eps)

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
  }
}
