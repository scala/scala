package scala.tools.nsc.matching ;
import java.util._ ;

trait NondetWordAutoms {
/** a nondeterministic word automaton.
 *  states are represented (implicitly) as Integer objects.
 */
class NondetWordAutom  {
  // BEGIN stuff from FiniteAutom

  //final static Integer FINTAG = new Integer(0);

  /** number of states */
  var nstates: int =_;

  /** the 'alphabet' */
  var labels:  HashSet = _;

  /** the set of final states, here as a TreeMap */
  var finals: TreeMap = _;

  /** dfa: HashMap trans: Object -> Integer
   *  nfa: HashMap trans: Object -> Vector [ Integer ]
   *
   *  nfa: Integer  ->(Object -> Vector [ Int ])
   *       [q]     |->( a |-> { q' | (q,a,q') in \deltaright } )
   *
   *  dfa: Integer  ->(Object -> Int)
   *       [q]     |->( a |-> q' | \deltaright(q,a) = q' } )
   */

  var _deltaq:Array[HashMap] = _;

  var _defaultq:Array[Vector] = _; // this gives the default transitions

  //public HashMap deltaq[];

  // --- accessor methods

  /** returns number of states
   def nstates():  int = {
   return nstates;
   }
   */

  /** returns the labels
   def labels():  HashSet = {
   return _labels;
   }
   */

  /** returns the transitions
   */
  def deltaq(  state: int ):HashMap = {
    return _deltaq( state );
  }

  /** returns the transitions
   */
  def deltaq(  state: Integer ): HashMap = {
    return _deltaq( state.intValue() );
  }

  /** returns the transitions
   */
  def defaultq( state: int  ): Vector = {
    return _defaultq( state );
  }

  /** returns the transitions
   */
   def defaultq( state:Integer  ): Vector = {
     return _defaultq( state.intValue() );
   }


  /** returns true if the state is final
   */
  def isFinal( state:int  ): boolean = {
    return ((finals != null)
            && (finals.get( new Integer( state )) != null));
  }

  /** returns true if the state is final
   */
  def isFinal( state:Integer  ): boolean = {
    return ((finals != null) && finals.containsKey( state ));
  }

  /** returns true if the state is final
   */
  def finalTag(  state: Integer ):     Integer = {
    return finals.get( state ).asInstanceOf[Integer];
  }


  def finalTag( state:int  ): Integer = {
    return finals.get( new Integer (state )).asInstanceOf[Integer];
  }

  /** returns true if the set of states contains at least one final state
   */
  def containsFinal( Q:TreeSet  ): boolean = {
    var  it = Q.iterator();
    while(it.hasNext()) {
      if( isFinal( it.next().asInstanceOf[Integer]))
        return true;
    }
    return false;
  }


  /** returns true if there are no finite states
   */
  def isEmpty(): boolean = finals.isEmpty();

  // END stuff from FiniteAutom


  // inherited from FiniteAutom

  // set of *distinct* objects that may label transitions
  // see Object.hashCode() to see why this works

  //HashSet labels;
  //TreeMap finals;

  var initials: TreeSet = _; // ? need this ?
  // ---

  // Object deltaq -->
  // HashMap deltaq[]; // this gives the transitions of q;

  var leftTrans: boolean = _;
  var rightTrans:    boolean = _;

  /** if true, this automaton behaves as a special left transducer.
   *  if a run succeeds, the result is not "true" but the entire
   *  run of the automaton as a sequence of (label,state) - pairs.
   *  used for binding variables.
   */
   def producesRun(): boolean = {
     return leftTrans;
   }

    def consumesRun():    boolean = {
      return rightTrans;
    }

  def initial( i: Integer  ):      boolean  = {
    return initials.contains( i );
  }
  var qbinders: Array[Vector] = _;

  /** returns all states accessible from Qsrc via label.
   *  used by class DetWordAutomaton.
   */
  def getSide ( Qsrc:TreeSet , label:Object  ): TreeSet = {
    //Console.println("NWA::getSide(Qsrc="+Qsrc);
    val Qdest = new TreeSet();
    var it = Qsrc.iterator();
    while(it.hasNext())  {// state
      val q = it.next().asInstanceOf[Integer].intValue();
      val ps = deltaq( q ).get( label ).asInstanceOf[Vector];
      //Console.println("deltaq(q) = "+deltaq(q));
      //Console.println("_deltaq(q) = "+_deltaq(q));
      //Console.println("ps = "+ps);
      if( null != ps ) {
	Qdest.addAll( ps );
      }
      //Console.println("defq = "+_defaultq( q ));
      Qdest.addAll( _defaultq( q ) );
    }
    //Console.println("DONE-NWA::getSide");
    return Qdest;
  }

  /** returns the transitions
   */
  def defaultq( P1: Set  ): Object  = {
      val defTarget = new TreeSet();
    var p1 = P1.iterator();
    while(p1.hasNext()) {
      val q1 = p1.next().asInstanceOf[Integer].intValue();
      //System.out.println("   q1:"+q1+ " defa: "+defaultq( q1 ));
      defTarget.addAll( _defaultq( q1 ) );
    }
    return defTarget;
  }


  /** first match policy: among several final states, the smallest one is
   *   chosen. used by class DetWordAutomaton
   */
  def finalTag( Q:Set  ): Integer = {

    var min = Integer.MAX_VALUE ;
    var it = Q.iterator();
    while(it.hasNext()) {
      val state = it.next().asInstanceOf[Integer];
      val tag = finals.get( state ).asInstanceOf[Integer];
      if( tag != null ) {
        if( tag.intValue() < min )
          min = tag.intValue();
      }
    }

    if ( min == Integer.MAX_VALUE )
      scala.Predef.error( "there should be a final state ");

    return new Integer( min );
  }

  /*
   void tmap(int offs, TreeMap t) = {
   TreeMap nt = new TreeMap();
   for(Iterator it = t.keySet().iterator(); it.hasNext(); ) = {
   Integer key = (Integer) it.next();
   Integer newkey = new Integer( key.intValue() + offs );
   Integer val = (Integer) t.get( key );
   Integer newval = new Integer( val.intValue() + offs );

   nt.put( newkey, newval );
   }
   return nt;
   }
   */
  // hashmaps, treemaps
  def mapmap(src:Map, offset:int , dest:Map , mapkeys:boolean , mapvals:boolean ): Map = {
    var it = src.keySet().iterator();
    while(it.hasNext()) {
      var key = it.next();
      var value = src.get( key );
      if( mapkeys ) key = new Integer( key.asInstanceOf[Integer].intValue()
                                      + offset );
      if( mapvals ) value = vmap( offset, value.asInstanceOf[Vector] ) ;
      /* new Integer( ((Integer)val).intValue()
       + offset );
       */
      dest.put( key, value );
    }
    return dest;
  }

  def vmap(offs:int , v:Vector  ):  Vector = {
    if( v == null )
      return null;
    var res = new Vector( v.size() );
    var it = v.iterator();
    while(it.hasNext()) {
      val item = it.next().asInstanceOf[Integer];
      res.add( new Integer( item.intValue() + offs ));
    }
    return res;

  }

  /*
   void relocate_defaultq( int offs, Vector   _defaultq[] ) = {
   for( int i = 0; i < this.nstates; i++ ) = {
   _defaultq[ i + offset ] = vmap( offset, ((Vector[])defaultq)[ i ]);
   }
   }
   */

  /** copies the values in the fields of this object into the
   *  arguments, possibly adding an offset.
   */
  def relocate( offset:int, _finals:TreeMap, _deltaq1:Array[HashMap], _defaultq1:Array[Vector], _qbinders1:Array[Vector] ): Unit = {

    mapmap( finals, offset, _finals, true, false);
    var i = 0;
    while(i < this.nstates) {

      _deltaq1  ( i + offset ) =
        mapmap( deltaq( i ), offset, new HashMap(), false, true).asInstanceOf[HashMap];

      _defaultq1( i + offset ) = vmap( offset, this.defaultq( i ) );
      i = i + 1;
    }
    if ((_qbinders1 != null) &&( qbinders != null )) {
      i = 0;
      while(i < this.nstates ) {
        //System.out.println("hallo"+qbinders);
        //System.out.println("qbinders[ i ] :"+qbinders[ i ]);
        //assert _qbinders != null;
        //System.out.println("_qbinders :"+_qbinders);

        _qbinders1( i + offset ) = qbinders( i );
        i = i + 1
      }
    }
  }


  /** if there is more than one initial state, a new initial state
   *  is created, with index 0
   */
  def normalize( initials:TreeSet , reloc:boolean  ): Unit = {
    //if( initials.size() == 1 )
    //      return;

    var idelta   = new HashMap();
    var idefault = new TreeSet();

    var q0 = new Integer( 0 );

    var it = initials.iterator();
    while(it.hasNext()) {

      val ostate = it.next().asInstanceOf[Integer];

      val finTag = finals.get( ostate ).asInstanceOf[Integer] ;
      if(( finTag != null ) && ( finals.get( q0 )  == null))
        finals.put( q0, finTag );


      var tmp = deltaq( ostate );

      if( reloc )
        tmp = mapmap( tmp, 1, new HashMap(), false, true ).asInstanceOf[HashMap];

      val labs = tmp.keySet().iterator();
      while(labs.hasNext()) {
        val lab     = labs.next();
        var itarget = idelta.get( lab ).asInstanceOf[Vector];
        if( null == itarget )
          idelta.put( lab, {itarget = new Vector(); itarget});
        val otarget = tmp.get( lab ).asInstanceOf[Vector];
        itarget.addAll( otarget );
      }
      //System.out.println( "normalize:defaultq[ "+ostate+" ] "+((Vector[]) defaultq) [ ostate.intValue() ]);
      if( defaultq( ostate ) != null )
        idefault.addAll( defaultq( ostate )  );
    }

    if( reloc ) {
      val m = 1 + this.nstates;
      val _finals    = new TreeMap();
      val _deltaq    = new Array[HashMap]( m );
      val _defaultq = new Array[Vector]( m );
      var _qbinders: Array[Vector] = null;

      if( qbinders != null )
        _qbinders = new Array[Vector]( m );

      relocate( 1, _finals, _deltaq, _defaultq, _qbinders );

      this.nstates  = m;
      this.finals   = _finals;
      this._deltaq   = _deltaq;
      this._defaultq = _defaultq;
      this.qbinders = _qbinders;
    }

    this._deltaq  ( 0 ) = idelta;
    //System.out.println("normalize:deltaq[ 0 ]"+ idelta );
    this._defaultq( 0 ) = new Vector( idefault );

    //System.out.println("normalize:defaultq[ 0 ]"+ idefault );

    this.initials = new TreeSet();
    this.initials.add( q0 );
  }


      /** called from Berry-Sethi construction.
       */

       def this(nstates:int, _labels:HashSet, initials: TreeSet, finals:TreeMap, deltaq:Array[HashMap], defaultq:Array[Vector], qbinders:Object ) = {
         this();
         //Console.println("NWA::this(. . . . )");
         this.nstates = nstates;
         this.labels = _labels;
         this.initials = initials;
         this.finals = finals;
         this._deltaq = deltaq;
         this._defaultq = defaultq;
         this.qbinders = qbinders.asInstanceOf[Array[Vector]];
         //print();
         //System.exit(0);
       }



  var offset:Array[int] = _; // only used if constructed from multiple

  def collectLabels( nfa:Array[NondetWordAutom ] ): Unit = {
    this.labels = new HashSet();
    var i = 0;
    while(i < nfa.length) {
      this.labels.addAll( nfa( i ).labels );
      i = i + 1
    }
  }

  def collectOffsets( nfa:Array[NondetWordAutom] ): Unit = {
    this.offset = new Array[int]( nfa.length + 1 );
    offset( 0 ) = 1; // we have a new initial state
    var i = 0;
    while(i < nfa.length ) {
      offset( i + 1 ) = nfa( i ).nstates + offset( i );
      i = i + 1
    }
  }

  /** collapses several normalized NondetWordAutom objects into one.
   */

  def this( nfa: Array[NondetWordAutom] ) = {
    this();
    //Console.println("NWA::this(.)");

    //this.m
    val m = nfa.length;
    //System.out.println("enter NondetWordSwitch, "
    //                   +"combining " + m + " automata");

    collectLabels( nfa );
    collectOffsets( nfa );

    //Console.println(" X 1");


    this.nstates = offset( nfa.length ); //m - 1 ] + nfa[ m - 1 ].nstates;


    this.finals = new TreeMap();

    this.qbinders = new Array[Vector]( nstates );

    // new initial state gets all transitions from all initial states

    this._deltaq        = new Array[HashMap]( nstates );
    this._defaultq      = new Array[Vector]( nstates );
    //Console.println(" X 2");

    //TreeSet defaultqSet = new TreeSet();
    _deltaq( 0 ) = new HashMap(); // 0 = our new initial state

    val initials = new TreeSet();

    var i = 0;
    while(i < m) {
      //System.out.println("i (current NFA):"+i);

      val n = nfa( i );

      val offs = offset( i );

      initials.add( new Integer( offs ));

      n.relocate( offs,
                 this.finals,
                 this._deltaq,
                 this._defaultq,
                 this.qbinders );
      i = i + 1;
    }

    normalize( initials, false );
    //Console.println("Leave-NWA::this(.)");
  }




  def print(): Unit = {

    Console.print("NFA on labels "+ this.labels);

    if( offset != null ) {
      Console.print("offset");
      var k = 0;
      while(k < offset.length) {
        if( k > 0)
          Console.print(", ");
        Console.print(offset(k));
        k = k + 1;
      }
    }
    Console.println;

    Console.print("max state number :" + (nstates - 1)  );

    Console.println("initials" + initials);

    Console.println("finals" + finals);

    var i = 0;
    while(i < nstates) {
      Console.print("state: " + i);
      if( finals.containsKey( new Integer( i )) ){
        Console.print("*"); //final
      }
      Console.print("  transitions: {");
      var arrows:HashMap  = deltaq( i );

      var it = arrows.keySet().iterator();
      while(it.hasNext()) {
        val label = it.next();
        val targets = arrows.get( label ).asInstanceOf[Vector];
        val jt = targets.iterator();
        while(jt.hasNext()) {
          val p = jt.next().asInstanceOf[Integer];
          Console.print("("+label+","+p+")");
        }
      }

      Console.print("} ");
      Console.print(" default transitions: "+_defaultq( i ));
      if( null != qbinders )
        Console.println(" binders "+qbinders( i ));
      Console.println;
      i = i + 1;
    }
  }


}

}
