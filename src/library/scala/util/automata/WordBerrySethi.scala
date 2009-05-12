/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.automata

import scala.collection.{immutable, mutable, Map}
import scala.util.regexp.WordExp


/** This class turns a regexp into a NondetWordAutom using the
 *  celebrated position automata construction (also called Berry-Sethi or
 *  Glushkov)
 *
 *  @author Burak Emir
 *  @version 1.0
 */
abstract class WordBerrySethi extends BaseBerrySethi {

  override val lang: WordExp

  type _labelT = this.lang._labelT

  import lang.{Alt, Eps, Letter, Meta, RegExp, Sequ, Star}


  protected var labels:mutable.HashSet[_labelT] = _
  // don't let this fool you, only labelAt is a real, surjective mapping
  protected var labelAt: immutable.Map[Int, _labelT] = _ // new alphabet "gamma"

  protected var deltaq: Array[mutable.HashMap[_labelT,List[Int]]] = _ // delta

  protected var defaultq: Array[List[Int]] = _ // default transitions

  protected var initials:immutable.Set[Int] = _
  //NondetWordAutom revNfa

  // maps a letter to an Integer ( the position )
  // is not *really* needed (preorder determines position!)
  //protected var posMap: mutable.HashMap[RegExp, Int] = _;

  /** Computes <code>first(r)</code> where the word regexp <code>r</code>.
   *
   *  @param r the regular expression
   *  @return  the computed set <code>first(r)</code>
   */
  protected override def compFirst(r: RegExp): immutable.Set[Int] = r match {
    case x:Letter => emptySet + x.pos //posMap(x);  // singleton set
    case Eps      => emptySet /*ignore*/
    case _        => super.compFirst(r)
  }

  /** Computes <code>last(r)</code> where the word regexp <code>r</code>.
   *
   *  @param r the regular expression
   *  @return  the computed set <code>last(r)</code>
   */
  protected override def compLast(r: RegExp): immutable.Set[Int] = r match {
    case x:Letter => emptySet + x.pos //posMap(x) // singleton set
    case Eps      => emptySet /*ignore*/
    case _        => super.compLast(r)
  }

  /** Returns the first set of an expression, setting the follow set along
   *  the way.
   *
   *  @param fol1 ...
   *  @param r    the regular expression
   *  @return     the computed set
   */
  protected override def compFollow1(fol1: immutable.Set[Int], r: RegExp): immutable.Set[Int] =
    r match {
      case x:Letter =>
        //val i = posMap(x)
        val i = x.pos
        this.follow.update(i, fol1)
        emptySet + i
      case Eps =>
        emptySet /*ignore*/
      case _ =>
        super.compFollow1(fol1, r)
    }

  /** returns "Sethi-length" of a pattern, creating the set of position
   *  along the way
   */


  /** called at the leaves of the regexp */
  protected def seenLabel(r: RegExp, i: Int, label: _labelT) {
    //Console.println("seenLabel (1)");
    //this.posMap.add(r, i)
    this.labelAt = this.labelAt.updated(i, label)
    //@ifdef if( label != Wildcard ) {
      this.labels += label
    //@ifdef }
  }

  // overriden in BindingBerrySethi
  protected def seenLabel(r: RegExp, label: _labelT): Int = {
    //Console.println("seenLabel (2)");
    pos = pos + 1
    seenLabel(r, pos, label)
    pos
  }

  // todo: replace global variable pos with acc
  override def traverse(r: RegExp): Unit = r match {
    case a @ Letter(label) => a.pos = seenLabel(r, label)
    case Eps               => /*ignore*/
    case _                 => super.traverse(r)
  }


  protected def makeTransition(src: Int, dest: Int, label: _labelT ) {
    //@ifdef compiler if( label == Wildcard )
    //@ifdef compiler   defaultq.add(src, dest::defaultq( src ))
    //@ifdef compiler else
    val q = deltaq(src)
    q.update(label, dest::(q.get(label) match {
      case Some(x) => x
      case _       => Nil
    }))
  }

  protected def initialize(subexpr: Seq[RegExp]): Unit = {
    //this.posMap = new mutable.HashMap[RegExp,Int]()
    this.labelAt = immutable.Map[Int, _labelT]()
    this.follow = new mutable.HashMap[Int, immutable.Set[Int]]()
    this.labels = new mutable.HashSet[_labelT]()

    this.pos = 0

    // determine "Sethi-length" of the regexp
    //activeBinders = new Vector()
    var it = subexpr.elements
    while (it.hasNext)
      traverse(it.next)

    //assert(activeBinders.isEmpty())
    this.initials = emptySet + 0
  }

  protected def initializeAutom() {
    finals   = immutable.Map.empty[Int, Int] // final states
    deltaq   = new Array[mutable.HashMap[_labelT, List[Int]]](pos) // delta
    defaultq = new Array[List[Int]](pos) // default transitions

    var j = 0
    while (j < pos) {
      deltaq(j) = new mutable.HashMap[_labelT,List[Int]]()
      defaultq(j) = Nil
      j += 1
    }
  }

  protected def collectTransitions(): Unit = {  // make transitions
    //Console.println("WBS.collectTrans, this.follow.keys = "+this.follow.keys)
    //Console.println("WBS.collectTrans, pos = "+this.follow.keys)
    var j = 0; while (j < pos) {
      //Console.println("WBS.collectTrans, j = "+j)
      val fol = this.follow(j)
      val it = fol.elements
      while (it.hasNext) {
        val k = it.next
        if (pos == k)
          finals = finals.updated(j, finalTag)
        else
          makeTransition( j, k, labelAt(k))
      }
      j += 1
    }
  }

  def automatonFrom(pat: RegExp, finalTag: Int): NondetWordAutom[_labelT] = {
    this.finalTag = finalTag

    pat match {
      case x:Sequ =>
        // (1,2) compute follow + first
        initialize(x.rs)
        pos = pos + 1
        globalFirst = compFollow(x.rs)

        //System.out.print("someFirst:");debugPrint(someFirst);
        // (3) make automaton from follow sets
        initializeAutom()
        collectTransitions()

        if (x.isNullable) // initial state is final
          finals = finals.updated(0, finalTag)

        var delta1: immutable.Map[Int, Map[_labelT, List[Int]]] =
          immutable.Map[Int, Map[_labelT, List[Int]]]()

        var i = 0
        while (i < deltaq.length) {
          delta1 = delta1.updated(i, deltaq(i))
          i += 1
        }
        val finalsArr = new Array[Int](pos)

        {
          var k = 0; while (k < pos) {
            finalsArr(k) = finals.get(k) match {
              case Some(z) => z
              case None => 0 // 0 == not final
            };
            k += 1
          }
        }

        val initialsArr = new Array[Int](initials.size)
        val it = initials.elements

        {
          var k = 0; while (k < initials.size) {
            initialsArr(k) = it.next
            k += 1
          }
        }

        val deltaArr = new Array[Map[_labelT, immutable.BitSet]](pos)

        {
          var k = 0; while(k < pos) {
            val labels = delta1(k).keys
            val hmap =
              new mutable.HashMap[_labelT, immutable.BitSet]
            for (lab <- labels) {
              val trans = delta1(k)
              val x = new mutable.BitSet(pos)
              for (q <- trans(lab))
                x += q
              hmap.update(lab, x.toImmutable)
            }
            deltaArr(k) = hmap
            k += 1
          }
        }
        val defaultArr = new Array[immutable.BitSet](pos)

        {
          var k = 0; while(k < pos) {
            val x = new mutable.BitSet(pos)
            for (q <- defaultq(k))
              x += q
            defaultArr(k) = x.toImmutable
            k += 1
          }
        }

        new NondetWordAutom[_labelT] {
          type _labelT = WordBerrySethi.this._labelT
          val nstates  = pos
          val labels   = WordBerrySethi.this.labels.toList
          val initials = initialsArr
          val finals   = finalsArr
          val delta    = deltaArr
          val default  = defaultArr
        }
      case z =>
       val z1 = z.asInstanceOf[this.lang._regexpT]
       automatonFrom(Sequ(z1), finalTag)
    }
  }

  /*
   void print1() {
   System.out.println("after sethi-style processing");
   System.out.println("#positions:" + pos);
   System.out.println("posMap:");

   for (Iterator it = this.posMap.keySet().iterator();
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
            for (int j = 1; j < pos; j++ ) {
                  TreeSet fol = (TreeSet) this.follow.get(new Integer(j));
                  System.out.print("("+j+" -> "+fol.toString()+") ");
                  //debugPrint( fol );
                  System.out.println();
            }

      }
    */
}
