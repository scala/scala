/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$
package scala.tools.nsc.matching ;

import java.util._ ;

trait DetWordAutoms requires TransMatcher  {

import global._;
class DetWordAutom {

    /** determinization -- standard algorithm considering only
     *                    reachable states
     */
    def this(nfa: NondetWordAutom) = {
      this();
      //Console.println("DWA:this(.)");
      //Console.println(nfa.nstates);
      //nfa.print();
      determinize( nfa );
      //Console.println(_nstates);
    }

  //final static Integer FINTAG = new Integer(0);

  /** number of states */
  var _nstates:int =0;

  /** the 'alphabet' */
  var _labels:HashSet = _;

  /** the set of final states, here as a TreeMap */
  /*protected*/ var finals:TreeMap = _;

  /** dfa: HashMap trans: Object -> Integer
   *  nfa: HashMap trans: Object -> Vector [ Integer ]
   *
   *  nfa: Integer  ->(Object -> Vector [ Int ])
   *       [q]     |->( a |-> { q' | (q,a,q') in \deltaright } )
   *
   *  dfa: Integer  ->(Object -> Int)
   *       [q]     |->( a |-> q' | \deltaright(q,a) = q' } )
   */

  var _deltaq: Array[HashMap] = _;

  var _defaultq: Array[Integer] = _; // this gives the default transitions

  //protected HashMap deltaq[];

  // --- accessor methods

  /** returns number of states
   */
  def nstates(): Int = _nstates;

  /** returns the labels
   */
  def labels():  HashSet = _labels;

  /** returns the transitions
   */
  def deltaq( state:int  ): HashMap =  _deltaq( state );

  /** returns the transitions
   */
  def deltaq( state:Integer  ): HashMap =  _deltaq( state.intValue() );

  /** for a set of nfa states (that must exist), returns its transitions
   */
  def deltaq(nset: TreeSet):  HashMap =
    deltaq( indexMap.get( nset ).asInstanceOf[Integer] );

  /** for a set of nfa states (that must exist), returns its transitions
   */
   def defaultq( nset:TreeSet  ):  Integer =
     defaultq( indexMap.get( nset ).asInstanceOf[Integer] );

  /** returns the transitions
   */
  def  defaultq( state: int  ): Integer =
    _defaultq( state );

  /** returns the transitions
   */
  def defaultq( state: Integer  ): Integer =
    _defaultq( state.intValue() );

  /** returns true if the state is final
   */
  def isFinal(state: int): boolean =
    ((finals != null)
     && (finals.get( new Integer( state )) != null));

  /** returns true if the state is final
   */
  def  isFinal(state: Integer):  boolean = {
    return ((finals != null) && finals.containsKey( state ));
  }

  /** returns true if the state is final
   */
  def finalTag( state:Integer  ):  Integer = {
    return finals.get( state ).asInstanceOf[Integer];
  }


  def  finalTag( state: int  ): Integer = {
    return finals.get( new Integer (state )).asInstanceOf[Integer];
  }

  /** returns true if the set of states contains at least one final state
   */
  def containsFinal( Q: TreeSet  ): boolean = {
    val it = Q.iterator();
    while(it.hasNext()) {
      if( isFinal(it.next().asInstanceOf[Integer]))
	return true;
    }
    return false;
  }


  /** returns true if there are no finite states
   */
  def isEmpty():  boolean = {
    return finals.isEmpty();
  }

  // END stuff from FiniteAutom

   final val FIRST: int  = 0;
   final val LAST: int   = FIRST + 1;

  //static final int WHICH_LONGEST_MATCH = FIRST ;
   final val WHICH_LONGEST_MATCH:int  = LAST ;

  // inherited from FiniteAutom:

  // int nstates;   // number of states
  // HashSet labels;// the alphabet
  // TreeMap finals;

  // HashMap deltaq[];
  //Integer defaultq[];


  // TEMPORARY VAR used only during determinization and debug printing
  // Q -> (Label -> Q )
  var  delta/*Map*/ : HashMap = _;
  // Q -> Integer;
  var indexMap: HashMap = _;

  // Integer -> Q
  var invIndexMap: HashMap = _;

    // only not null if this is a right-transducer
  var qbinders: Array[Vector] = _;

  final val NODEFAULT:  Integer = new Integer( -1 );

  def isSink( i:int ):  boolean = {
    return  ( _deltaq( i ).keySet().isEmpty()
	     && (_defaultq != null )
	     && (_defaultq( i ).intValue() == i) );
  }

  def hasDefault( i:int  ):  boolean = {
    return _defaultq( i ) != NODEFAULT;
  }

  def determinize( nfa: NondetWordAutom  ): Unit = {
    //Console.println("DetWordAutom:determinize");
    //System.out.println("nfa:");nfa.print();
    var states:TreeSet = null; // temp: Set[Set[Integer]]
    var deftrans:HashMap = null; // Set[Integer] -> Int

    var trans: HashMap = null; // always points to a mapping ( Label -> Q )
    var ix = 0;    // state index

    this._labels = nfa.labels;
    ////System.out.println("Labels: "+labels);
    this.delta/*Map*/ = new HashMap();
    //this.dead = -1;

    states = new TreeSet( new StateSetComparator() );
    deftrans = new HashMap();
    // temporarily: Map[Set[Integer]] later: Map[Integer]
    this.finals = new TreeMap( new StateSetComparator() );
    this.invIndexMap = new HashMap();
    this.indexMap = new HashMap();

    // new initial state (singleton set { q0 } by construction)
    val q0 = new TreeSet();
    q0.addAll( nfa.initials ); /*new Integer( 0 )); */
    states.add( q0 );

    val empty = new TreeSet();
    deftrans.put( q0, empty );
    states.add( empty );
    deftrans.put( empty, empty );

    val rest = new Stack();
    if( nfa.isFinal( 0 ) )
      this.finals.put( q0, nfa.finalTag( 0 ) );

    //Console.println("...beginning");

    rest.push( empty );
    rest.push( q0 );
    //Console.println("...beginning 2" );
    while( !rest.empty() ) {
      //Console.println("...beginning 3" );
      val P1 = rest.pop().asInstanceOf[TreeSet];

      //System.out.println("states:"+ states);
      //System.out.println("P1:"+ P1);

      invIndexMap.put( new Integer( ix ), P1 );
      indexMap.put( P1, new Integer( ix ));
      ix = ix + 1;
      delta/*Map*/.put( P1, {trans = new HashMap(); trans});

      //Console.println("...beginning 4" );
      // labelled transitions
      val  it = _labels.iterator();
      //Console.println("it = "+it );
      //Console.println(it.hasNext());

      while( it.hasNext() ) {
        //Console.println("...beginning 5" );
        //Console.flush;
	val label = it.next();
	//Console.print( "Label: " + label +" ");
	// Qdest will contain all states reachable via `label'
	// from some nfa state in P1;
	val Qdest = nfa.getSide( P1, label );
	//Console.println("Qdest:"+Qdest);
	if( !states.contains( Qdest ) ) {
	  states.add( Qdest );
	  ////System.out.print(" (added)" );
	  rest.push( Qdest );
	  ////System.out.print(" (pushed)");

        //Console.println("nfa.containsFinal("+Qdest+") =="+nfa.containsFinal( Qdest ));

	  if( nfa.containsFinal( Qdest ) )
	    this.finals.put( Qdest, nfa.finalTag( Qdest ));
	  ////System.out.print(" (added final)");

	}
	////System.out.println(".Qdest");

	trans.put( label, Qdest );
	// //System.out.println( "Qdest: " + Qdest);
        //Console.println("Y marks");
      }

      // default transitions

      val defTarget: TreeSet = nfa.defaultq( P1 ).asInstanceOf[TreeSet];
      //System.out.println("defTarget:"+defTarget);
      deftrans.put( P1, defTarget );

      //Console.println("X marks 0");

      if( !states.contains( defTarget ) ) {
        //Console.println("X marks 1");

	states.add( defTarget );
	rest.push( defTarget );
        //Console.println("nfa.containsFinal("+defTarget+")"+nfa.containsFinal( defTarget ));
	if( nfa.containsFinal( defTarget ) )
	  this.finals.put( defTarget, nfa.finalTag( defTarget ));
      }

      //Console.println("X marks");
    }

    //Console.println("...continuing");

    // <DEBUG>
    //printBefore( states, deftrans );

    // </DEBUG> do not call printBefore after this point
    // //System.out.println("indexMap: "+indexMap);

    this._nstates = states.size();
    _deltaq = new Array[HashMap]( _nstates );
    _defaultq = new Array[Integer]( _nstates );

    // we replace Set[Set[Integer]] by its index and clean up

    val jt = states.iterator();
    while(jt.hasNext()) {
      val state   = jt.next().asInstanceOf[TreeSet];
      val state_x = indexMap.get( state ).asInstanceOf[Integer];

      val defTarget  = deftrans.get( state ).asInstanceOf[TreeSet];
      var defTarget_x: Integer  = null;
      if(  null != defTarget) {
	defTarget_x = indexMap.get( defTarget ).asInstanceOf[Integer];
	////System.out.println("deftarget" + defTarget);
      } else
	defTarget_x = NODEFAULT;

      ////System.out.print(state.toString() + " --> " + state_x);
      //System.out.println(" deftarget " + defTarget + " --> "+defTarget_x);

      trans = delta/*Map*/.get( state ).asInstanceOf[HashMap];
      val newTrans = new HashMap();
      val labs = _labels.iterator();
      while(labs.hasNext()) {
	val label = labs.next();
	val target   = trans.get( label ).asInstanceOf[TreeSet];
	var target_x: Integer = null;
	if( null != target  ) {
	  // //System.out.println("target :"+target);
	  target_x = indexMap.get( target ).asInstanceOf[Integer];

	  if( target_x.intValue() != defTarget_x.intValue() ) {
	    // replace target by target_x
	    // (use type-unawareness)
	    newTrans.put( label, target_x );
	  }
	  trans.remove( label );
	}

      }
      _deltaq( state_x.intValue() ) = newTrans;
      _defaultq( state_x.intValue() ) = defTarget_x;

      delta/*Map*/.remove( state );
      deftrans.remove( state );

    }
    //Console.println("determinize::: finals"+finals);
    val oldfin: TreeMap = finals;
    this.finals = new TreeMap();
    var kt = oldfin.keySet().iterator();
    while(kt.hasNext()) {
      val state = kt.next().asInstanceOf[TreeSet];
      val state_x = indexMap.get( state ).asInstanceOf[Integer];;
      this.finals.put( state_x, oldfin.get( state ) ); // conserve tags
    }

    // clean up, delete temporary stuff
    /*
     // we cannot clean up, indexmap is needed later
     for( Iterator it = states.iterator(); it.hasNext(); ) {
     ((TreeSet) it.next()).clear();
     }
     */
    states.clear();

      //Console.println("...done");
    //minimize();
  }



  def isDead(state: Int): Boolean = {
    return state == _nstates - 1; // by construction
  }

  def isDead(state: Integer): Boolean = {
    return state.intValue() == _nstates - 1; // by construction
  }


  /** returns target of the transition from state i with label label.
   *  null if no such transition exists.
   */
  def delta(i: Int, label: Label): Integer = {
    var target:Integer = null;
    label match {
    case DefaultLabel() =>
      if (! hasDefault(i))
        return null;
        return _defaultq( i ).asInstanceOf[Integer] ;
    case SimpleLabel( _ ) | TreeLabel( _ ) =>
      return _deltaq( i ).get( label ).asInstanceOf[Integer] ;
    /*case LPair( Integer state, Label lab ):
      return state;
     */
    case _ =>
      scala.Predef.error("whut's this: label="+label+", class "+label.getClass());
    }
  }

  def delta(i: Integer, label: Label): Integer =
    delta(i.intValue(), label);

  /** should maybe in nfa, not here
   */
  /*static  */
  protected def smallestFinal( nfa: NondetWordAutom, states:TreeSet  ): Integer = {

    var min = Integer.MAX_VALUE ;
    val it = states.iterator();
    while (it.hasNext()) {
      val state = it.next().asInstanceOf[Integer];
      if( nfa.isFinal( state ) && (state.intValue() < min ))
	min = state.intValue();
    }
    if (min == Integer.MAX_VALUE)
      scala.Predef.error("I expected a final set of states");
    return new Integer( min );
  }

  protected def allSetsThatContain( ndstate: Integer  ): Vector = {
    val v = new Vector();
    val it = indexMap.keySet().iterator();
    while(it.hasNext()) {
      val ndstateSet = it.next().asInstanceOf[TreeSet];
      if( ndstateSet.contains( ndstate ))
	v.add( ndstateSet );
    }
    return v;
  }


  protected def filterItOutQuoi( dLeft: DetWordAutom, npTarget: Npair,lab:LPair , nsrc:TreeMap ):Unit = {
    val theLabel  = lab.lab;
    val ntarget = lab.state;

    // e.g.[2,(3),4] --> 7
    val dstate = dLeft.indexMap.get( npTarget.nset ).asInstanceOf[Integer];

    // eg. 3 -> [3] [2,3]
    val targets:Vector = dLeft.allSetsThatContain( ntarget );

    ////System.out.println( targets+", of these " ) ;

    // filter out those source states which arrive here...
    val su = targets.iterator();
    while(su.hasNext()) {
      val nset   = su.next().asInstanceOf[TreeSet];
      val ddelta = dLeft.deltaq( nset ).asInstanceOf[HashMap];

      // ...  at THIS dstate
      if(ddelta.get( theLabel ).asInstanceOf[Integer] == dstate ) {

	val np1 = new Npair( ntarget, nset );

	////System.out.print( np1.toString( dLeft.indexMap ));

	if( WHICH_LONGEST_MATCH == FIRST )
	  addTransitionFLM( nsrc, np1 );
	else
	  addTransitionLLM( nsrc, np1 );
      }

    }
  }

  /** all default transitions from sets that contain nq to npTarget
   */
  protected def filterItOutQuoiDefault( dLeft: DetWordAutom ,npTarget:Npair , nq:Integer , nsrc:TreeMap  ): Unit = {


    ////System.out.println( "npTarget = " + npTarget ) ;

    val allSources = dLeft.allSetsThatContain( npTarget.nstate );
    val it = allSources.iterator();
    while(it.hasNext()) {

      // e.g.[2,(3),4] --> 7
      //Integer dstate = (Integer) dLeft.indexMap.get( npTarget.nset );

      val dstate = dLeft.indexMap.get( it.next() ).asInstanceOf[Integer];

      //System.out.println( "dstate = " + dstate ) ;

      //assert dstate != null;

      // eg. 3 -> [3] [2,3]
      val targets = dLeft.allSetsThatContain( nq );

      //System.out.println( "targets: " + targets ) ;

      // filter out those source states which arrive here...
      val su = targets.iterator();
      while(su.hasNext()) {
	val nset = su.next().asInstanceOf[TreeSet];
	val ddef = dLeft.defaultq( nset );

	//System.out.println( "ddef ="+ddef );

	// ...  at THIS dstate
	if( ddef == dstate ) {

	  val np1 = new Npair( nq, nset );

	  // print target
	  //System.out.print( np1.toString( dLeft.indexMap ));

	  if( WHICH_LONGEST_MATCH == FIRST )
	    addTransitionFLM( nsrc, np1 );
	  else
	    addTransitionLLM( nsrc, np1 );

	}

      }
    }
  }

  /** this implements the first longest match policy
   */
  protected def addTransitionFLM( nsrc:TreeMap , np:Npair  ): Unit= {
    val np2 = nsrc.get( np.nset ).asInstanceOf[Npair ];

    // (policy) first longest match
    if(( np2 == null )
       ||( np2.nstate.intValue() > np.nstate.intValue())) {
	 nsrc.put( np.nset, np  );
       }

  }

  /** this implements the last longest match policy (!)
   */
  protected def addTransitionLLM(nsrc: TreeMap,  np: Npair ): Unit = {
    val np2 = nsrc.get( np.nset ).asInstanceOf[Npair];

    // (policy) first longest match
    if(( np2 == null )
       ||( np2.nstate.intValue() < np.nstate.intValue())) {
	 nsrc.put( np.nset, np  );
       }

  }


  /** build a deterministic right to left transducer from the args
   */
  def this(right: NondetWordAutom, left:NondetWordAutom, dLeft: DetWordAutom ) = {
    this();

    /* System.out.println("DetWordAutom.<init>(nfa,nfa,dfa)");
     System.out.println("nfa-left:");left.print();
     System.out.println("nfa-right:");right.print();
     System.out.println("dLeft:"+dLeft.print());
     System.out.println("dLeft.finals"+dLeft.finals);
     */
    this.indexMap = dLeft.indexMap;
    this.invIndexMap = dLeft.invIndexMap;
    // fix indexMap
    /* // unnecessary
     TreeSet q0 = new TreeSet();
     q0.add( new Integer( 0 ));
     indexMap.put( q0, new Integer( 0 ));
     //System.out.println("check out the indexMap!" + indexMap);
     */

    val visited_n = new TreeSet( new NpairComparator() );
    val rest    = new Stack();

    // right is "nearly deterministic"
    // we can follow reverse traces paths by using dLeft.indexMap

    // start with right.initials, left.final, dLeft.final
    val it = dLeft.finals.keySet().iterator();
    while(it.hasNext()) {
      val fstate = it.next().asInstanceOf[Integer];
      val nfstate = invIndexMap.get( fstate ).asInstanceOf[TreeSet];
      //System.out.print( "final state:"+fstate);
      //System.out.print( " correspond to set of states:"+ nfstate );

      val min_ndstate: Integer = smallestFinal( left, nfstate );

      val npair:Npair = new Npair( min_ndstate, nfstate );

      //System.out.println( "  smallest final of these: "+ min_ndstate );


      //System.out.println( "push final nfa state "+npair.toString( dLeft.indexMap ));

      if( !visited_n.contains( npair )) {
	visited_n.add( npair );
	rest.push( npair );
      }
    }

    val ratLab     = new HashMap(); // maps nset to label,HashMap
    val ratDelta   = new HashMap(); // maps nset to Vector[ NP ]targets

    val ratDefault = new HashMap(); // maps nset to NP (one target)

    var ix = 1;
    val ix_initial = rest.clone().asInstanceOf[Stack];
    var ix_final = new TreeSet( new NpairComparator() );;

    val newIndexMap = new TreeMap( new NpairComparator() );

    while( !rest.isEmpty() ) {

      val npair = rest.pop().asInstanceOf[Npair];
      newIndexMap.put( npair, new Integer(ix));

      ratDelta.put( npair, new Vector() );

      if( npair.nset.contains( new Integer( 0 )) ) {
	ix_final.add( npair );
      }
      ix = ix + 1;

      //System.out.println(" popped "+npair.toString( dLeft.indexMap ));

      ////System.out.print(" binders: ");
      ////System.out.print( right.qbinders[ npair.nstate.intValue() ] );

      val delta = right.deltaq( npair.nstate );

      ////System.out.print(" we could have arrived : ");
      //search the delta for target invIndexMap

      val labelToNset = new HashMap();
      val labelToFrom = new HashMap();

      // maps nsets to the active nstates
      var nsrc = new TreeMap( new StateSetComparator() );

      // berry-sethi construction assures that
      //   there is only one label for outgoing transitions
      var theLabel:Label = null;

      // collect all transition possible in the DFA
      val jt = delta.keySet().iterator();
      while(jt.hasNext()) {
	val lab = jt.next().asInstanceOf[LPair];

	// lab.state is the target in the NFA

	if( null == theLabel ) {
	  ratLab.put( npair, lab.lab );
	  ////System.out.print(" with \""+lab.lab+"\" ");
	}
	theLabel = lab.lab ;

	////System.out.print("\nfrom n" + lab.state +"  ... ");

	// these are too many, filter out those that exist in DFA

	filterItOutQuoi( dLeft, npair, lab, nsrc );

      }


      ////System.out.println( "---" );

      ////System.out.println("all sources: ");

      // !!  first longest match
      val ut = nsrc.keySet().iterator();
      while(ut.hasNext()) {
	val nset  = ut.next().asInstanceOf[TreeSet];
	val np2: Npair = nsrc.get( nset ).asInstanceOf[Npair] ;

	//assert( np2 != null );
	////System.out.println("target: n"+npair.nstate+" via: "+theLabel+" from "+ np2.toString( dLeft.indexMap ));// nset:"+nset+ " namely state n"+ dest);

	val v = ratDelta.get( npair ).asInstanceOf[Vector];

	v.add( np2 );

	if( !visited_n.contains( np2 ) ) {

	  visited_n.add( np2 );
	  rest.push( np2 );
	}

      }

      //System.out.println("default sources: ");

      // maps nsets to the active nstates
      nsrc = new TreeMap( new StateSetComparator() );

      // now for all default transitions that arrive at this nfa state
      val defqs = right.defaultq( npair.nstate );
      val kt = defqs.iterator();
      while( kt.hasNext() ) {
	val nq = kt.next().asInstanceOf[Integer];
	//System.out.println("checking nq="+nq);
	filterItOutQuoiDefault( dLeft, npair, nq, nsrc );
	//System.out.println( "nsrc after "+nq+" is "+nsrc );
      }

      //System.out.println( "defqs :"+defqs );
      //System.out.println( "nsrc :"+nsrc );
      var nut = nsrc.keySet().iterator();
      while(nut.hasNext()) {

	val np2 = nsrc.get( nut.next() ).asInstanceOf[Npair];

	var v = ratDefault.get( npair ).asInstanceOf[Vector] ;
	if( v == null )
	  ratDefault.put( npair, {v = new Vector(); v} );
	v.add( np2 );

	if( !visited_n.contains( np2 ) ) {

	  visited_n.add( np2 );
	  rest.push( np2 );
	}

      }

      ////System.out.println("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");

    }

    // Renumbering

    ////System.out.println( "output: a dfa with "+ix+"states");

    // FIX: empty regular expression (as in "List()") is valid
    //assert ( !ix_final.isEmpty() ) : "no final states found";

    ////System.out.println( "final state:"+ix_final);

    //System.out.println( "indexMap: " +indexMap);
      //System.out.println( "newIndexMap: " +newIndexMap);
    this.finals = new TreeMap();
    this._nstates = ix;
    val dratDelta = new Array[HashMap]( ix );
    qbinders  = new Array[Vector]( ix );
    _labels = new HashSet();
    val kit = ratDelta.keySet().iterator();
    while(kit.hasNext()) {
      val np = kit.next().asInstanceOf[Npair];

      //System.out.print( "\nstate: "+np);
      val ndset = np.nset;
      val dstate = newIndexMap.get( np ).asInstanceOf[Integer];
      //assert dstate != null : "no dstate for "+np.toString(dLeft.indexMap);

      //System.out.print(" binders:");

      qbinders( dstate.intValue() ) = left.qbinders( np.nstate.intValue() );

      //System.out.print( qbinders[dstate.intValue() ]);

      //System.out.println(" transitions:");
      if( ix_final.contains( np ) ) {
	val fin_ix = newIndexMap.get( np ).asInstanceOf[Integer];
	finals.put( fin_ix, new Integer( 0 ));
      }

      val lab = ratLab.get( np ).asInstanceOf[Label];
      val v   = ratDelta.get( np ).asInstanceOf[Vector];

      val ddelta = new HashMap();

      // v might be null if there are only default transitions
      if( v != null ) {
        val it2 = v.iterator();
        while(it2.hasNext()) {

	  val np2= it2.next().asInstanceOf[Npair];
	  //System.out.print( "("+lab+","+np2+") " );
	  val ddestR = newIndexMap.get( np2 ).asInstanceOf[Integer];
	  val ddest = indexMap.get( np2.nset ).asInstanceOf[Integer];
	  //assert ddest != null :
	  //"no ddest for "
	  //+np2.toString(dLeft.indexMap);

	  val newLab = new LPair(ddest, lab);
	  ddelta.put( newLab, ddestR );
	  _labels.add( newLab );

	}
	dratDelta( dstate.intValue() ) = ddelta;

      }
    }
    var itt = ratDefault.keySet().iterator();
    while(itt.hasNext()) {
      val np  = itt.next().asInstanceOf[Npair];
      val dstate = newIndexMap.get( np ).asInstanceOf[Integer];

      //System.out.print("\nstate: "+np+" default trans: ");

      val v = ratDefault.get( np ).asInstanceOf[Vector];
      val ut = v.iterator();
      while(ut.hasNext()) {
	val np2  = ut.next().asInstanceOf[Npair];
	val targetL      = indexMap.get( np2.nset ).asInstanceOf[Integer];;
	val targetR      = newIndexMap.get( np2 ).asInstanceOf[Integer];;

	val defLab = new LPair( targetL, DefaultLabel() );

	_labels.add( defLab );
	//System.out.print( "("+defLab+","+np2+") " );

	var d = dratDelta( dstate.intValue() );
	if( d == null )
	  dratDelta( dstate.intValue() ) = {d = new HashMap(); d};

	d.put( defLab, targetR );
      }
    }

    _deltaq = dratDelta;

    val hmap = new HashMap();

    // final states of left are initial states of right
    // problem: still need to choose the one

    while( !ix_initial.isEmpty() ) {
      val np = ix_initial.pop().asInstanceOf[Npair];

      val i          = newIndexMap.get( np ).asInstanceOf[Integer]; //R-state
      val dtarget    = indexMap.get( np.nset ).asInstanceOf[Integer];// left-d-state

      hmap.put( dtarget, i );
    }
    _deltaq( 0 )  = hmap; // careful, this maps Int to Int

    qbinders( 0 ) = new Vector();
    //((Vector[])defaultq)[ 0 ] = new Vector(); is null
    //printBeforeRAT( dratDelta );

  }

  def printBeforeRAT1(str: String): Unit = {
    val tmp = new StringBuffer( str );
    var j = tmp.length();
    while(j < 20) {
      tmp.append(" ");
      j = j + 1;
    }
    Console.println( tmp.toString() );
  }

  def printBeforeRAT( dratDelta: Array[HashMap] ): Unit = {
    //System.out.println();
    printBeforeRAT1( "dratDelta" );
    printBeforeRAT1( "[index]" );
    //System.out.println();
    var  i = 0;
    while(i < dratDelta.length) {
      if( isFinal( i ))
	printBeforeRAT1( "*"+i );
      else
	printBeforeRAT1( " "+i );

      //System.out.println( dratDelta[ i ] );
    i = i + 1
    }
  }

  /** you may only call this before the set[set[...]] representation
   *  gets flattened.
   */
  def printBefore(states: TreeSet, deftrans: HashMap): Unit = {
    var  trans: HashMap = null;
    Console.println(states);
    val  it = states.iterator();
    while (it.hasNext()) {
      val state = it.next().asInstanceOf[TreeSet];
      Console.print("state:" + state.toString() + " transitions ");
      trans = delta/*Map*/.get( state ).asInstanceOf[HashMap];
      val labs = _labels.iterator();
      while(labs.hasNext()) {
	val label = labs.next();
	val target = trans.get( label ).asInstanceOf[TreeSet];
	Console.print( "  (" + label.toString()
		      + "," + target.toString()+")");
      }
      Console.print("default trans"+deftrans.get(state));
      Console.println;
    }
    Console.println("final states:" + finals);
  }
}

}
