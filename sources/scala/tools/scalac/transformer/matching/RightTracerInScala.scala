/**
 *  $Id$
 */

import scalac._;
import scalac.ast._;
import scalac.symtab._;
import Tree._;

import java.util._ ;
import Scope.SymbolIterator;

import scalac.util.Name ;
import scalac.util.Names ;

import scala.tools.util.Position;

package scala.tools.scalac.transformer.matching {

  /** translate right tracer to code
     * @param dfa determinized left tracer
     * @param left nondeterm. left tracer (only needed for variables!)
     * @param cf   ...
     * @param pat  ?
     * @param elementType ...
     */
class RightTracerInScala(dfa: DetWordAutom, seqVars: Set, owner: Symbol, cf: CodeFactory, pat:Tree, elementType: Type)
extends TracerInScala( dfa, elementType, owner, cf )  {

  final def defs = cf.defs;

  val allVars: Set = collectVars( pat );

  var varsToExport: Set = new HashSet(); // @todo HANDLE seqVars THESE GLOBALLY INSTEAD OF LOCALLY

  varsToExport.addAll( allVars );
  varsToExport.removeAll( seqVars );

  var targetSym:Symbol = _;

  var helpMap  = new HashMap();
  var helpMap2 = new HashMap();
  var helpVarDefs = new TreeList();

  val it = seqVars.iterator();
  while(it.hasNext()) {
    makeHelpVar( it.next().asInstanceOf[Symbol] );
  }

  val jt = allVars.iterator();
  while(jt.hasNext()) {
    val varSym = jt.next().asInstanceOf[Symbol];
    if(( varSym.name.toString().indexOf("$") == -1 )
       && ( !seqVars.contains( varSym ))) {
         makeHelpVar( varSym, true );
       }
  }

  //System.out.println("allVars: "+allVars);
  //System.out.println("seqVars: "+seqVars);
  //System.out.println("helpVarDefs now: "+helpVarDefs);

  initializeSyms();

  def makeHelpVar(realVar: Symbol): Unit = {
    makeHelpVar( realVar, false );
  }

  /** makes a helpvar and puts mapping into helpMap, ValDef into helpVarDefs
   */

  def makeHelpVar(realVar: Symbol, keepType: Boolean): Unit = {
    val helpVar = owner.newVariable( cf.pos,
                                    0,
                                    cf.fresh.newName( realVar.name
                                                     .toString()+"RTIS" ));
    var rhs: Tree = _;

    //System.out.println("RTiS making helpvar : "+realVar+" -> "+helpVar);

    if( keepType ) {
      helpVar.setType( realVar.getType() );
      rhs = gen.mkDefaultValue( cf.pos, realVar.getType() );
    } else {
      helpVar.setType( defs.LIST_TYPE(elementType) );
      rhs = gen.mkNil( cf.pos );
    }

    helpMap.put( realVar, helpVar );
    helpVar.flags = helpVar.flags | Modifiers.MUTABLE;
    val varDef = gen.ValDef( helpVar, rhs );
    //((ValDef) varDef).kind = Kinds.VAR;
    helpVarDefs.append( varDef );

  }

  def prependToHelpVar(realVar: Symbol, elem:Tree): Tree = {
    val hv = refHelpVar( realVar );
    gen.Assign( hv, gen.mkNewCons( cf.pos, elementType, elem, hv ));
    /*
     return cf.Block(pos,
     new Tree [] {
          cf.debugPrintRuntime( "ASSIGN" ),
          gen.Assign( hv, cf.newSeqCons( elem, hv ))
          }, defs.UNIT_TYPE());
        */
  }

  protected def initializeSyms(): Unit = {

    this.funSym = owner.newLabel( pos,
                                 cf.fresh.newName( "right" ));

    this.iterSym = owner.newVariable( pos,
                                     Modifiers.MUTABLE,
                                     cf.fresh.newName("iter"))
    .setType( cf.SeqTraceType( elementType ));

    this.stateSym = owner.newVariable ( pos,
                                       Modifiers.MUTABLE,
                                       cf.fresh.newName("q"))
    .setType( defs.int_TYPE() ) ;

    this.curSym = owner.newVariable( pos,
                                    Modifiers.MUTABLE,
                                    cf.fresh.newName("cur"))
    .setType( elementType ) ;

    this.targetSym = owner.newVariable( pos,
                                       Modifiers.MUTABLE,
                                       cf.fresh.newName("p"))
    .setType( defs.int_TYPE() ) ;

    funSym.setType( new Type.MethodType( Predef.Array[Symbol] (  // dummy symbol MethodType
      funSym.newVParam( pos, 0, cf.fresh.newName("iter"), cf.SeqTraceType( elementType )),
      funSym.newVParam( pos, 0, cf.fresh.newName( "q" ), defs.int_TYPE()) ),
                                        defs.void_TYPE() )); // result

  }

  // load current elem and trace
  override def loadCurrentElem(body: Tree): Tree = {
    gen.If( cf.isEmpty( _iter() ),
           run_finished( 0 ),            // we are done
           gen.mkBlock( Predef.Array[Tree] (
             gen.ValDef( this.targetSym,
                        cf.SeqTrace_headState( gen.Ident( pos, iterSym))),
             gen.ValDef( this.curSym,
                        cf.SeqTrace_headElem( gen.Ident( pos, iterSym )))),
                       body )
         );
  }

  /** see code_state0_NEW
   */
  def code_state0(elseBody: Tree) = { // careful, map Int to Int

    gen.If( cf.Equals( _state(), gen.mkIntLit( cf.pos, 0 )),
           code_state0_NEW(),
           elseBody );

  }

    /** this one is special, we check the first element of the trace
     *  and choose the next state depending only on the state part
     */
  def code_state0_NEW(): Tree =  { // careful, map Int to Int

    val hmap = dfa.deltaq( 0 ); // all the initial states

    var i = 0;
    val n = hmap.keySet().size(); // all transitions defined

    val tmapTag = new TreeMap();
    val  tmapBody = new TreeMap();
    var  it = hmap.keySet().iterator();
    while(it.hasNext()) {
      val  targetL = it.next().asInstanceOf[Integer];
      val targetR  = hmap.get( targetL ).asInstanceOf[Integer];

      val I = new Integer( i );
      tmapTag.put( targetL, I );
      tmapBody.put( I, callFun( Predef.Array[Tree] (
        cf.SeqTrace_tail( _iter() ),
        gen.mkIntLit( cf.pos, targetR.intValue() ) )));
      i = i + 1;
    }
    i = 0;
    val tags    = new Array[Int]( n );
    val targets = new Array[Tree]( n );
    var jt = tmapTag.keySet().iterator();
    while(jt.hasNext()) {
      val tagI = jt.next().asInstanceOf[Integer];
      tags( i )    = tagI.intValue();
      val I    = tmapTag.get( tagI ).asInstanceOf[Integer];
      targets( i ) = tmapBody.get( I ).asInstanceOf[Tree];;
      i = i + 1
    }
    gen.Switch( gen.Ident( pos, targetSym ),
               tags,
               targets,
               code_error()/*cannot happen*/ );

  }

  override def currentMatches(label: Label): Tree = {
    label.match {
      case LPair( target, theLab ) =>
        cf.Equals( gen.mkIntLit( cf.pos, target.intValue() ),
                         current() );
      case _ => throw new ApplicationError("expected Pair label");
    }
  }


  override def code_state_NEW(i: Int): Tree = { // precondition i != 0
    var stateBody = code_delta( i, DefaultLabel() );
    if( stateBody == null ) {
      stateBody = code_error();
    }
    val trans = dfa.deltaq( i );
    val tmapTag = new TreeMap();
    val tmapBody = new TreeMap();
    var  j = 0;
    var  labs = dfa.labels().iterator();
    while(labs.hasNext())  {
      val label  = labs.next();
      val next   = trans.get( label ).asInstanceOf[Integer];
      val action = code_delta( i, label.asInstanceOf[Label] );

      if( action != null ) {
        val J = new Integer( j );
        tmapTag.put( label.asInstanceOf[LPair].state, J );
        tmapBody.put( J, action );

        stateBody = gen.If( currentMatches( label.asInstanceOf[Label] ),
                           action,
                           stateBody);
      }
      j = j + 1;
    }
    val n = tmapTag.keySet().size();
    j = 0;
    val tags    = new Array[int]( n );
    val targets = new Array[Tree]( n );
    val it = tmapTag.keySet().iterator();
    while(it.hasNext()) {
      val tagI     = it.next().asInstanceOf[Integer];
      tags( j )    = tagI.intValue();
      val J        = tmapTag.get( tagI ).asInstanceOf[Integer];
      targets( j ) = tmapBody.get( J ).asInstanceOf[Tree];
      j = j + 1;
    }
    if( n > 0 )
      gen.Switch( gen.Ident( pos, targetSym ), tags, targets, code_error() );
    else
      code_error();
  }

    // calling the AlgebraicMatcher here
  override def _cur_match(pat1: Tree): Tree = {
    var pat = pat1;
    //System.out.println("RTiS._cur_match("+pat.toString()+")");
    //System.out.println("calling algebraic matcher on type:"+pat.type);

    //System.err.println( "curT"+currentElem().type().widen() );
    val m = new PartialMatcher( owner, //funSym,//this.funSym,
                               currentElem(),
                               defs.boolean_TYPE() );

    val freshenMap = new HashMap(); // sym2exp -> new sym
    val helpMap3 = new HashMap();         // new sym -> original sym

    // "freshening": never use the same symbol more than once
        // (in later invocations of _cur_match)

    var it = varsToExport.iterator();
    while(it.hasNext() ) {
      val key = it.next().asInstanceOf[Symbol];
      if( key.name.toString().indexOf("$") == -1 ) {
        this.helpMap2.put( key, helpMap.get( key ));
        // "freshening" by appending string ( a bit dangerous )
        val newSym = key.cloneSymbol().setOwner( owner /*funSym*/ );
        newSym.name = Name.fromString( key.name.toString() + "%" );
        freshenMap.put( key, newSym );
        helpMap3.put( newSym, helpMap.get( key ));
        //System.out.println( "key: "+ key + " key.owner:"+key.owner());
        //System.out.println( "newsym owner:"+newSym.owner());
      } else {
        freshenMap.put( key, key );
      }
    }

    //System.out.println("RightTracerInScala:: -pat :"+pat.toString());
    /*
     System.out.println("RightTracerInScala - the seqVars"+seqVars);
     System.out.println("RightTracerInScala - the varsToExport"+varsToExport);
     */
    //System.out.println("RightTracerInScala::freshenMap :"+freshenMap);

        // "freshening"

        /* TEST
         */
    val tc = new TreeCloner( cf.unit.global, freshenMap, Type.IdMap );

        /*
        Transformer tc = new Transformer(cf.unit.global) {
                public Tree transform(Tree tree) {
                    tree = super.transform(tree);
                    if (tree.hasSymbol()) {
                        Object symbol = freshenMap.get(tree.symbol());
                        if (symbol != null) tree.setSymbol((Symbol)symbol);
                    }
                    return tree;
                }
            };
            */
    pat = tc.transform( pat );


        //System.out.println("RightTracerInScala:: -pat( after subst ) :"+pat);


        // val match { case <pat> => { <do binding>; true }
        //             case _     => false


    val ts = new Array[Tree]( helpMap3.keySet().size() );
    var j = 0;
    var jt = helpMap3.keySet().iterator();
    while(jt.hasNext()) {
      val vsym = jt.next().asInstanceOf[Symbol];
      val hv   = helpMap3.get( vsym ).asInstanceOf[Symbol];
      //hv.setType( defs.LIST_TYPE( elementType ) ) ; DEBUG ALARM ?
      val refv   = gen.Ident(cf.pos, vsym);
      val refhv  = gen.Ident(cf.pos, hv);
      ts( j ) = gen.Assign( refhv, refv );
      j = j + 1;
            // System.out.println( "the assign" + res[ j - 1 ] );
    }

    val res = gen.mkBooleanLit( cf.pos, true ); // just `true'
    val theBody =  gen.mkBlock(ts, res);

    am.construct( m, Predef.Array[Tree] (

      cf.gen.CaseDef( pat,           // if tree val matches pat -> update vars, return true
                     theBody/* "freshening */),
      cf.gen.CaseDef( cf.gen.Ident( pat.pos, defs.PATTERN_WILDCARD ),
                     gen.mkBooleanLit( pat.pos, false )) ), // else return false
                 true // do binding please
               );

    am.toTree();
  }

  /** returns translation of transition with label from i.
   *  returns null if there is no such transition(no translation needed)
   */
  override def code_delta(i: Int , label: Label  ): Tree = {
    val hmap    = dfa.deltaq( i );
    val ntarget =  hmap.get( label ).asInstanceOf[Integer];
    var algMatchTree: Tree = null;
    if( ntarget == null )
      return null;

    //System.out.println("delta("+i+","+label+")" );
    var  theLab: Label = null;
    label.match {
      case LPair ( state, lab2 )=>
        //assert ntarget == state;
        theLab = lab2;
      lab2.match {
        case TreeLabel( pat ) =>
          algMatchTree = _cur_match( pat );
        case _ =>
      }
      case DefaultLabel() =>
        throw new ApplicationError(); // should not happen
    }
    //assert dfa.qbinders != null : "qbinders ?";

    var vars = dfa.qbinders( i );

    //System.out.println("dfa.qbinders[ i ]"+vars);

    if( null == vars ) vars = new Vector(); // TODO: make this more consistent
    //assert vars != null;

    val stms = new Array[Tree]( vars.size()
                                + { if (algMatchTree != null ) 1 else 0 });
    var j = 0;
    var it = vars.iterator();
    while(it.hasNext()) {
      val vble = it.next().asInstanceOf[Symbol];
      val rhs = gen.Ident( pos, curSym );
      stms( j ) = prependToHelpVar( vble , rhs);
      j = j + 1;
    }

    if( algMatchTree != null ) {
      stms( j ) = algMatchTree ;
      j = j + 1
    }

    val value = callFun( Predef.Array[Tree] ( cf.SeqTrace_tail( _iter() ),
                                      gen.mkIntLit( cf.pos, ntarget.intValue() ) ) );

    gen.mkBlock( pos, stms, value );
  }

  override def stateWrap(i: Int): Tree = {
    if( i == 0 )
      code_state0_NEW();
    else
      code_state_NEW( i );
  }

  /* returns statements that do the work of the right-transducer
   */
  def getStms(trace: Tree, unit: CompilationUnit, body: Tree): Tree =  {

    val stms = new TreeList();
    val loopbody = code_body_NEW();

    stms.append( gen.ValDef( iterSym, trace ) );
    stms.append( gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0 ) ) );
    stms.append( helpVarDefs );
    stms.append( gen.LabelDef( this.funSym,
                              Predef.Array[Ident] (
                                gen.Ident( pos, iterSym ),
                                gen.Ident( pos, stateSym )
                              ), loopbody ));

    // bind variables handled by this righttracer
    var it = seqVars.iterator();
    while(it.hasNext()) {
      stms.append( bindVar( it.next().asInstanceOf[Symbol] ) );
    }

    val treeCloner = new Transformer(unit.global) {
      override def transform(tree1: Tree): Tree = {
        val tree = super.transform(tree1);
        if (tree.hasSymbol()) {
          val symbol = helpMap2.get(tree.symbol());
          if (symbol != null) tree.setSymbol(symbol.asInstanceOf[Symbol]);
        }
        tree;
      }
    };

    gen.mkBlock(stms.toArray(), treeCloner.transform( body ));
  }



  /** return the accumulator. (same as in LeftTracerInScala)
   *  todo: move tree generation of Unit somewhere else
   */
   override def run_finished(state: Int):  Tree = {
     gen.mkUnitLit(Position.FIRSTPOS);
    }

  def   current() = {   gen.Ident( pos, targetSym );}

    def refHelpVar(realVar: Symbol) = {
      val hv = helpMap.get( realVar ).asInstanceOf[Symbol];
      //assert hv != null : realVar;
      gen.Ident(Position.FIRSTPOS, hv);
    }

  def assignToHelpVar(realVar: Symbol, rhs: Tree): Tree = {
    val hv = refHelpVar( realVar );
    gen.Assign( hv, rhs );
  }

  def bindVar(realVar: Symbol): Tree = {
    val hv = refHelpVar( realVar );
        /*
          System.out.println("binding realVar.name "+realVar.name+" type:"+realVar.type()+" to hv type:"+hv.type());
          realVar.setOwner( owner );
          System.out.println("is same as realVar"+realVar.type().isSameAs( elementType ));
          System.out.println("is same as hv"+realVar.type().isSameAs( hv.type() ));
          if( realVar.type().isSameAs( elementType ))
          return gen.ValDef( realVar, cf.SeqList_head( hv ));
          else
          return gen.ValDef( realVar, hv );
        */
    if( realVar.getType().isSameAs( hv.getType())) {
      gen.ValDef( realVar, hv ); // e.g. x @ _*
    } else
      gen.ValDef( realVar, cf.SeqList_head( hv ));
  }
}
}

