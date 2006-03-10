/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$

package scala.tools.nsc.matching;

import java.util._ ;

import scala.tools.nsc.util.Position;
import scala.tools.nsc.symtab.Flags;

trait RightTracers requires TransMatcher {

  import global._ ;
  import java.util._ ;

//import Scope.SymbolIterator;

//import scalac.util.Name ;
//import scalac.util.Names ;

//import scala.tools.util.Position;

  /** translate right tracer to code
     * @param dfa determinized left tracer
     * @param left nondeterm. left tracer (only needed for variables!)
     * @param pat  ?
     * @param elementType ...
     */
abstract class RightTracerInScala  extends Autom2Scala {

  val seqVars: Set;
  val pat:Tree;
  val elementType: Type;


  def SeqTrace_headElem( arg: Tree  ) = { // REMOVE SeqTrace
    val t = Apply(Select(arg, definitions.List_head), Nil);
    Apply(Select(t, definitions.tupleField(2,2)),Nil)
  }

  def SeqTrace_headState( arg: Tree  ) = { // REMOVE SeqTrace
    val t = Apply(Select(arg, definitions.List_head), Nil);
    Apply(Select(t, definitions.tupleField(2,1)), Nil)
  }

  def SeqTrace_tail( arg: Tree ): Tree =  // REMOVE SeqTrace
    Apply(Select(arg, definitions.List_tail), Nil);

  final def collectVars(pat: Tree): HashSet = {
    var vars = new HashSet();

    def handleVariableSymbol(sym: Symbol): Unit  = {
      vars.add( sym );
    }
    def isVariableName(name: Name): Boolean =
      ( name != nme.WILDCARD ) && ( treeInfo.isVariableName( name ) ) ;

    def isVariableSymbol(sym: Symbol): Boolean = {
      ( sym != null )&&( !sym.isPrimaryConstructor );
    }

    def traverse(tree: Tree): Unit = {
      tree match {
        case x @ Ident(name)=>
          if(x.symbol != definitions.PatternWildcard)
            scala.Predef.error("shouldn't happen?!");

        case Star(t) =>
          traverse(t);
        case Bind(name, subtree) =>
          var sym: Symbol = null;
          if( isVariableName( name )
             && isVariableSymbol( {sym = tree.symbol; tree.symbol} ))
            handleVariableSymbol( sym );

          traverse( subtree );

        // congruence cases
        case Apply(fun, args) => args foreach {traverse};
        case Sequence(trees)  => trees foreach {traverse};
        case Typed(expr, tpe) => traverse(expr); // needed??
        case _ : Alternative | _ : Select | _ : Literal =>  ; // no variables
        case _ => Predef.error("unknown node:"+tree+" = "+tree.getClass());
      }
    }
    traverse( pat );
    return vars;
  }

  //final def defs = cf.defs;

  val allVars: Set = collectVars( pat );

  var varsToExport: Set = new HashSet(); // @todo HANDLE seqVars THESE GLOBALLY INSTEAD OF LOCALLY

  varsToExport.addAll( allVars );
  varsToExport.removeAll( seqVars );

  var targetSym:Symbol = _;

  var helpMap  = new HashMap();
  var helpMap2 = new HashMap();
  var helpVarDefs:scala.List[Tree] = Nil;

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
    val helpVar = owner.newVariable( pos,
                                    fresh.newName( realVar.name
                                                  .toString()+"RTIS" ));
    var rhs: Tree = null;

    //System.out.println("RTiS making helpvar : "+realVar+" -> "+helpVar);

    if( keepType ) {
      helpVar.setInfo( realVar.tpe );
      rhs = EmptyTree;
    } else {
      helpVar.setInfo( definitions.ListClass.info /* LIST_TYPE(elementType)*/ );
      rhs = gen.mkNil;
    }

    helpMap.put( realVar, helpVar );
    helpVar.setFlag(Flags.MUTABLE);
    val varDef = ValDef( helpVar, rhs );
    //((ValDef) varDef).kind = Kinds.VAR;
    helpVarDefs= varDef :: helpVarDefs;

  }

  def prependToHelpVar(realVar: Symbol, elem:Tree): Tree = {
    val hv = refHelpVar( realVar );
    Assign( hv, gen.mkNewCons( /*elementType, */elem, hv ));
    /*
     return cf.Block(pos,
     new Tree [] {
     cf.debugPrintRuntime( "ASSIGN" ),
     gen.Assign( hv, cf.newSeqCons( elem, hv ))
     }, defs.UNIT_TYPE());
     */
  }

  protected def initializeSyms(): Unit = {

    this.funSym = owner.newLabel( pos, fresh.newName( "right" ));

    this.iterSym = owner.newVariable( pos, fresh.newName("iter"))
    .setInfo( SeqTraceType( elementType ));

    this.stateSym = owner.newVariable ( pos, fresh.newName("q"))
    .setInfo( definitions.IntClass.info ) ;

    this.curSym = owner.newVariable( pos, fresh.newName("cur"))
    .setInfo( elementType ) ;

    this.targetSym = owner.newVariable( pos, fresh.newName("p"))
    .setInfo( definitions.IntClass.info ) ;

    funSym.setInfo(
      MethodType( scala.List (  // dummy symbol MethodType
        SeqTraceType(elementType),
        //funSym.newValueParameter( pos, fresh.newName("iter") /*, SeqTraceType elementType */),
        definitions.IntClass.info),
      //funSym.newValueParameter( pos, fresh.newName( "q" ) /*, definitions.IntClass.info */),
                     definitions.UnitClass.info)) // result

  }

  // load current elem and trace
  override def loadCurrentElem(body: Tree): Tree = {
    If( isEmpty( _iter() ),
       run_finished( 0 ),            // we are done
       Block( scala.List (
         ValDef( this.targetSym,
                SeqTrace_headState( Ident( iterSym))),
         ValDef( this.curSym,
                SeqTrace_headElem( Ident( iterSym )))),
             body )
     );
  }

  /** see code_state0_NEW
   */
  def code_state0(elseBody: Tree) = { // careful, map Int to Int

    If( Equals( _state(), Literal(0)),
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
      tmapBody.put( I, callFun( scala.List (
        SeqTrace_tail( _iter() ),
        Literal( targetR.intValue() ) )));
      i = i + 1;
    }
    //i = 0;

    var ncases: scala.List[CaseDef] = Nil;
    //val tags    = new Array[Int]( n );
    //val targets = new Array[Tree]( n );
    var jt = tmapTag.keySet().iterator();
    while(jt.hasNext()) {
      val tagI = jt.next().asInstanceOf[Integer];
      //tags( i )    = tagI.intValue();
      val I    = tmapTag.get( tagI ).asInstanceOf[Integer];
      //targets( i ) = tmapBody.get( I ).asInstanceOf[Tree];;
      ncases = CaseDef( Literal(tagI.intValue()),
                       tmapBody.get(I).asInstanceOf[Tree] ) :: ncases;
      //i = i + 1
    }
    //gen.Switch( gen.Ident( pos, targetSym ),
    //           tags,
    //           targets,
    //           code_error()/*cannot happen*/ );

    Match(Ident(targetSym), ncases);
  }

  override def currentMatches(label: Label): Tree = label match {
    case LPair( target, theLab ) =>
      Equals( Literal(target.intValue() ), current() );
    case _ =>
      scala.Predef.error("expected Pair label");
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

        stateBody = If( currentMatches( label.asInstanceOf[Label] ),
                       action,
                       stateBody);
      }
      j = j + 1;
    }
    val n = tmapTag.keySet().size();
    //j = 0;
    //val tags    = new Array[int]( n );
    //val targets = new Array[Tree]( n );
    var ncases: scala.List[CaseDef] = Nil;
    val it = tmapTag.keySet().iterator();
    while(it.hasNext()) {
      val tagI     = it.next().asInstanceOf[Integer];
      //tags( j )    = tagI.intValue();
      val J        = tmapTag.get( tagI ).asInstanceOf[Integer];
      //targets( j ) = tmapBody.get( J ).asInstanceOf[Tree];
      ncases = CaseDef(Literal(tagI.intValue()),
                       tmapBody.get( J ).asInstanceOf[Tree]) :: ncases;
      //j = j + 1;
    }
    if( n > 0 )
      //gen.Switch( gen.Ident( pos, targetSym ), tags, targets, code_error() );
      Match(Ident( targetSym ), ncases);
    else
      code_error();
  }

    // calling the AlgebraicMatcher here
  override def _cur_match(pat1: Tree): Tree = {
    var pat = pat1;
    //System.out.println("RTiS._cur_match("+pat.toString()+")");
    //System.out.println("calling algebraic matcher on type:"+pat.type);

    //System.err.println( "curT"+currentElem().type().widen() );
    val m = new PartialMatcher {
      val owner = RightTracerInScala.this.owner; // , //funSym,//this.funSym,
      val selector = currentElem(); //,
      // result type defs.boolean_TYPE() );
    }
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
        val newSym = key.cloneSymbol( owner /*funSym*/ );
        newSym.name = newTermName(key.name.toString() + "%") ; // erm
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

    //@nsc @todo @todo @todo @todo

    //val tc = new TreeCloner( global, freshenMap, Type.IdMap );
    //pat = tc.transform( pat );
    //@nsc   this commented out, is broken anyway.

    // val match  case <pat> =>  <do binding>; true
    //             case _     => false


    var ts: scala.List[Tree] = scala.List(); //new Array[Tree]( helpMap3.keySet().size() );
    //var j = 0;
    var jt = helpMap3.keySet().iterator();
    while(jt.hasNext()) {
      val vsym = jt.next().asInstanceOf[Symbol];
      val hv   = helpMap3.get( vsym ).asInstanceOf[Symbol];
      //hv.setInfo( defs.LIST_TYPE( elementType ) ) ; DEBUG ALARM ?
      ts = Assign( Ident(hv), Ident(vsym) ) :: ts;
      //ts( j ) = gen.;
      //j = j + 1;
      // System.out.println( "the assign" + res[ j - 1 ] );
    }

    val theBody =  Block(ts, Literal( true )); // just `true'

    am.construct( m, scala.List(
      CaseDef( pat, theBody), // freshening
      // if tree val matches pat -> update vars, return true
      CaseDef(Ident(definitions.PatternWildcard), Literal(false))),
                 // else return false
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
    label match {
      case LPair ( state, lab2 )=>
        //assert ntarget == state;
        theLab = lab2;
      lab2 match {
        case TreeLabel( pat ) =>
          algMatchTree = _cur_match( pat );
        case _ =>
      }
      case DefaultLabel() =>
        scala.Predef.error("bla"); // should not happen
    }
    //assert dfa.qbinders != null : "qbinders ?";

    var vars = dfa.qbinders(i);

    //System.out.println("dfa.qbinders[ i ]"+vars);

    if (null == vars) vars = new Vector(); // TODO: make this more consistent
    //assert vars != null;

    var stms = if (algMatchTree != null ) algMatchTree::Nil else Nil;

    var it = vars.iterator();
    while(it.hasNext()) {
      val vble = it.next().asInstanceOf[Symbol];
      val rhs = gen.Ident( curSym );
      stms = prependToHelpVar( vble , rhs) :: stms;
    }

    val value = callFun( scala.List( SeqTrace_tail( _iter() ),
                                    Literal(ntarget.intValue())));

    Block(stms, value );
  }

  override def stateWrap(i: Int): Tree = {
    if (i == 0)
      code_state0_NEW();
    else
      code_state_NEW(i);
  }

  /* returns statements that do the work of the right-transducer
   */
  def getStms(trace: Tree, unit: CompilationUnit, body: Tree): Tree =  {

    var stms: scala.List[Tree] = scala.List();
    val loopbody = code_body_NEW();

    stms = (
      scala.List(
        ValDef( iterSym, trace ),
        ValDef( stateSym, Literal( 0 ))
      ) ::: helpVarDefs
        ::: scala.List(
          LabelDef(
            this.funSym,
            scala.List (
              iterSym,
              stateSym
            ),
            loopbody )
        ));

    // bind variables handled by this righttracer
    var it = seqVars.iterator();
    while(it.hasNext())
      stms = stms ::: bindVar( it.next().asInstanceOf[Symbol] ) :: Nil;

    val treeCloner = new Transformer {
      override def transform(tree1: Tree): Tree = {
        val tree = super.transform(tree1);
        if (tree.hasSymbol) {
          val symbol = helpMap2.get(tree.symbol);
          if (symbol != null) tree.setSymbol(symbol.asInstanceOf[Symbol]);
        }
        tree;
      }
    };

    Block(stms, treeCloner.transform( body ));
  }


  /** return the accumulator. (same as in LeftTracerInScala)
   *  todo: move tree generation of Unit somewhere else
   */
  override def run_finished(state: Int):  Tree = Literal(());

  def   current() = Ident( targetSym );

  def refHelpVar(realVar: Symbol) = {
    val hv = helpMap.get( realVar ).asInstanceOf[Symbol];
    //assert hv != null : realVar;
    Ident(hv);
  }

  def assignToHelpVar(realVar: Symbol, rhs: Tree): Tree = {
    val hv = refHelpVar(realVar);
    Assign(hv, rhs);
  }

  def bindVar(realVar: Symbol): Tree = {
    val hv = refHelpVar(realVar);
    /*
     System.out.println("binding realVar.name "+realVar.name+" type:"+realVar.type()+" to hv type:"+hv.type());
     realVar.setOwner( owner );
     System.out.println("is same as realVar"+realVar.type().isSameAs( elementType ));
     System.out.println("is same as hv"+realVar.type().isSameAs( hv.type() ));
     if( realVar.type().isSameAs( elementType ))
     return gen.ValDef( realVar, SeqList_head( hv ));
     else
     return gen.ValDef( realVar, hv );
     */
    if( isSameType(realVar.tpe, hv.tpe))
      ValDef( realVar, hv ); // e.g. x @ _*
    else {
      ValDef( realVar, SeqList_head( hv ));
    }
  }
}
}
