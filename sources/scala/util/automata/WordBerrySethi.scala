package scala.util.automata ;

import scala.util.alphabet.Alphabet ;
import scala.util.regexp.WordExp ;

import scala.collection.{ mutable, Map } ;
import scala.collection.immutable ;

/** this turns a regexp over A into a NondetWordAutom over A using the
 *  celebrated position automata construction (also called Berry-Sethi or
 *  Glushkov)
 */
abstract class WordBerrySethi[ A <: Alphabet ] extends BaseBerrySethi {

  override val lang: WordExp[ A ];
  import lang.{Alt,Eps,Letter,Meta,RegExp,Sequ,Star} ;


  protected var labels:mutable.HashSet[A] = _ ;
  // don't let this fool you, only labelAt is a real, surjective mapping
  protected var labelAt: immutable.TreeMap[int, A] = _; // new alphabet "gamma"

  protected var deltaq: Array[mutable.HashMap[A,List[Int]]] = _;    // delta

  protected var defaultq: Array[List[Int]] = _;  // default transitions

  protected var initials:immutable.Set[Int] = _ ;
  //NondetWordAutom revNfa ;

  /** computes first( r ) where the word regexp r */
  protected override def compFirst(r: RegExp): immutable.Set[Int] = r match {
    case x:Letter => emptySet + posMap(x);  // singleton set
    case _ => super.compFirst(r);
  }

  /** computes last( r ) where the word regexp r */
  protected override def compLast(r: RegExp): immutable.Set[Int] = r match {
    case x:Letter => emptySet + posMap(x) // singleton set
    case _ => super.compLast(r)
  }

  /** returns the first set of an expression, setting the follow set along
   *  the way
   */
  protected override def compFollow1( fol1:immutable.Set[Int], r:RegExp ): immutable.Set[Int] =
    r match {

      case x:Letter =>
        val i = posMap( x );
        this.follow.update( i, fol1 );
        emptySet + i;

      case _ => super.compFollow1(fol1, r)

    }

  /** returns "Sethi-length" of a pattern, creating the set of position
   *  along the way
   */


  /** called at the leaves of the regexp */
  protected def  seenLabel( r:RegExp, i:Int, label: A ): Unit = {
    this.posMap.update( r, i );
    this.labelAt = this.labelAt.update( i, label );
    //@ifdef if( label != Wildcard ) {
      this.labels += label ;
    //@ifdef }
  }

  // overriden in BindingBerrySethi
  protected def seenLabel( r: RegExp, label: A ): Unit = {
    pos = pos + 1;
    seenLabel( r, pos, label );
  }


  // todo: replace global variable pos with acc
  override def traverse(r: RegExp): Unit = r match {
      case Letter( label ) => seenLabel( r, label ) ;
      case _               => super.traverse(r)
  }


  protected def makeTransition(src: Int, dest:Int, label: A ):Unit = {
    //@ifdef compiler if( label == Wildcard )
    //@ifdef compiler   defaultq.update(src, dest::defaultq( src ))
    //@ifdef compiler else
    val q = deltaq( src );
    q.update(label, dest::(q.get(label) match {
      case Some(x) => x
      case _       => Nil
    }));
  }

  protected def initialize(subexpr: Seq[RegExp]): Unit = {
    this.posMap = new mutable.HashMap[RegExp,Int]();
    this.labelAt = new immutable.TreeMap[Int,A]();
    this.follow = new mutable.HashMap[Int,immutable.Set[Int]]();
    this.labels = new mutable.HashSet[A]();

    this.pos = 0;

    // determine "Sethi-length" of the regexp
    //activeBinders = new Vector();
    var it = subexpr.elements;
    while( it.hasNext )
      traverse( it.next );

    //assert ( activeBinders.isEmpty() );
    this.initials = emptySet + 0;
  }

  protected def initializeAutom(): Unit = {

    finals   = immutable.TreeMap.Empty[Int,Int];        // final states
    deltaq   = new Array[mutable.HashMap[A,List[Int]]]( pos );   // delta
    defaultq = new Array[List[Int]]( pos );    // default transitions

    var j = 0;
    while( j < pos ) {
      deltaq( j ) = new mutable.HashMap[A,List[Int]]();
      defaultq( j ) = Nil;
      j = j + 1
    }
  }

  protected def collectTransitions(): Unit = {  // make transitions
    var j = 0;
    while( j < pos ) {
      val fol = this.follow( j );
      val it = fol.elements;
      while( it.hasNext ) {
        val k = it.next;
        if( pos == k )
          finals.update( k, finalTag )
        else
          makeTransition( j, k, labelAt( k ))
      }
      j = j + 1;
    }
  }

  def automatonFrom(pat: RegExp, finalTag: Int): NondetWordAutom[ A ] = {
    this.finalTag = finalTag;

    pat match {
      case x:Sequ =>
        // (1,2) compute follow + first
        initialize( x.rs );
      pos = pos + 1;
      globalFirst = compFollow( x.rs );

      //System.out.print("someFirst:");debugPrint(someFirst);
      // (3) make automaton from follow sets
      initializeAutom();
      collectTransitions();

      if( x.isNullable ) // initial state is final
	finals = finals.update( 0, finalTag );

      var delta1: immutable.TreeMap[Int,Map[A,List[Int]]] =
        new immutable.TreeMap[Int,Map[A,List[Int]]];

      var i = 0;
      while( i < deltaq.length ) {
        delta1 = delta1.update( i, deltaq( i ));
        i = i + 1;
      }

      new NondetWordAutom[ A ] {
        val nstates  = pos;
        val labels   = WordBerrySethi.this.labels;
        val initials = WordBerrySethi.this.initials;
        val finals   = WordBerrySethi.this.finals;
        val delta    = delta1;
        val default  = defaultq;
      }
      case _ => error("expected Sequ");
    }
  }

  /*
   void print1() {
   System.out.println("after sethi-style processing");
   System.out.println("#positions:" + pos);
   System.out.println("posMap:");

   for( Iterator it = this.posMap.keySet().iterator();
   it.hasNext(); ) {
   Tree t = (Tree) it.next();
                  switch(t) {
                  case Literal( _ ):
                        System.out.print( "(" + t.toString() + " -> ");
                        String s2 = ((Integer) posMap.get(t)).toString();
                        System.out.print( s2 +") ");
                  }
            }
            System.out.println("\nfollow: ");
            for( int j = 1; j < pos; j++ ) {
                  TreeSet fol = (TreeSet) this.follow.get(new Integer(j));
                  System.out.print("("+j+" -> "+fol.toString()+") ");
                  //debugPrint( fol );
                  System.out.println();
            }

      }
    */
}
