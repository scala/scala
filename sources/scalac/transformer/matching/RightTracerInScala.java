/**
 *  $Id$
 */

package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;
import Scope.SymbolIterator;

import scalac.util.Name ;
import scalac.util.Names ;

import scala.tools.util.Position;

public class RightTracerInScala extends TracerInScala  {

    Set seqVars;
    Set allVars;

    Set varsToExport ;

    Symbol targetSym;

    HashMap helpMap;
    HashMap helpMap2 ;
    TreeList helpVarDefs;


    /** translate right tracer to code
     * @param dfa determinized left tracer
     * @param left nondeterm. left tracer (only needed for variables!)
     * @param cf   ...
     * @param pat  ?
     * @param elementType ...
     */
    public RightTracerInScala( DetWordAutom dfa,
                               Set seqVars,
                               Symbol owner,
                               CodeFactory cf,
                               Tree pat,
                               Type elementType ) {
        super( dfa, elementType, owner, cf );

        this.seqVars = seqVars; // HANDLE THESE GLOBALLY INSTEAD OF LOCALLY
        this.allVars = CollectVariableTraverser.collectVars( pat );

        this.varsToExport = new HashSet();
        varsToExport.addAll( allVars );
        varsToExport.removeAll( seqVars );

        this.helpMap = new HashMap();

        helpMap2 = new HashMap();
        helpVarDefs = new TreeList();

        for( Iterator it = seqVars.iterator(); it.hasNext(); ) {
            makeHelpVar( (Symbol) it.next() );
        }

        for( Iterator it = allVars.iterator(); it.hasNext(); ) {
            Symbol varSym = (Symbol) it.next();
            if(( varSym.name.toString().indexOf("$") == -1 )
               && ( !seqVars.contains( varSym ))) {
                makeHelpVar( varSym, true );
            }
        }

        //System.out.println("allVars: "+allVars);
        //System.out.println("seqVars: "+seqVars);
        //System.out.println("helpVarDefs now: "+helpVarDefs);

        initializeSyms();
    }

    void makeHelpVar( Symbol realVar ) {
        makeHelpVar( realVar, false );
    }

    /** makes a helpvar and puts mapping into helpMap, ValDef into helpVarDefs
     */

    void makeHelpVar( Symbol realVar, boolean keepType ) {
        Symbol helpVar = owner.newVariable( cf.pos,
                                            0,
                                            cf.fresh.newName( realVar.name
                                                              .toString()+"RTIS" ));
        Tree rhs;

        //System.out.println("RTiS making helpvar : "+realVar+" -> "+helpVar);

        if( keepType ) {
            helpVar.setType( realVar.type() );
            rhs = gen.mkDefaultValue( cf.pos, realVar.type() );
        } else {
            helpVar.setType( defs.LIST_TYPE(elementType) );
            rhs = gen.mkNil( cf.pos );
        }

        helpMap.put( realVar, helpVar );
        helpVar.flags |= Modifiers.MUTABLE;
        Tree varDef = gen.ValDef( helpVar, rhs );
        //((ValDef) varDef).kind = Kinds.VAR;
        helpVarDefs.append( varDef );

    }

    Tree prependToHelpVar( Symbol realVar, Tree elem ) {
        Tree hv = refHelpVar( realVar );
        return gen.Assign( hv, gen.mkNewCons( cf.pos, elementType, elem, hv ));
        /*
          return cf.Block(pos,
          new Tree [] {
          cf.debugPrintRuntime( "ASSIGN" ),
          gen.Assign( hv, cf.newSeqCons( elem, hv ))
          }, defs.UNIT_TYPE());
        */
    }

    protected void initializeSyms() {

        this.funSym = owner.newLabel( pos,
                                      cf.fresh.newName( "right" ));

        this.iterSym = owner.newVariable( pos,
                                          Modifiers.MUTABLE,
                                          cf.fresh.newName("iter"))
            .setType( cf.SeqTraceType( elementType ));

        this.stateSym = owner.newVariable ( pos,
                                            Modifiers.MUTABLE,
                                            cf.fresh.newName("q"))
            .setType( defs.INT_TYPE() ) ;

        this.curSym = owner.newVariable( pos,
                                         Modifiers.MUTABLE,
                                         cf.fresh.newName("cur"))
            .setType( elementType ) ;

        this.targetSym = owner.newVariable( pos,
                                         Modifiers.MUTABLE,
                                         cf.fresh.newName("p"))
            .setType( defs.INT_TYPE() ) ;

        funSym.setType( new Type.MethodType( new Symbol[] {  // dummy symbol MethodType
            funSym.newVParam( pos, 0, cf.fresh.newName("iter"), cf.SeqTraceType( elementType )),
            funSym.newVParam( pos, 0, cf.fresh.newName( "q" ), defs.INT_TYPE()) },
                                             defs.UNIT_TYPE() )); // result

    }

    // load current elem and trace
    Tree loadCurrentElem( Tree body ) {
        return
            gen.If( cf.isEmpty( _iter() ),
                    run_finished( 0 ),            // we are done
                    gen.mkBlock( new Tree[] {
                        gen.ValDef( this.targetSym,
                                    cf.SeqTrace_headState( gen.Ident( pos, iterSym))),
                        gen.ValDef( this.curSym,
                                    cf.SeqTrace_headElem( gen.Ident( pos, iterSym )))},
                        body )
                    );
    }

    /** see code_state0_NEW
     */
    Tree code_state0( Tree elseBody ) { // careful, map Int to Int

        return gen.If( cf.Equals( _state(), gen.mkIntLit( cf.pos, 0 )),
                       code_state0_NEW(),
                       elseBody );

    }

    /** this one is special, we check the first element of the trace
     *  and choose the next state depending only on the state part
     */
    Tree code_state0_NEW() { // careful, map Int to Int

        HashMap hmap    = (HashMap) dfa.deltaq( 0 ); // all the initial states

        int i = 0;
        final int n = hmap.keySet().size(); // all transitions defined

        TreeMap tmapTag = new TreeMap();
        TreeMap tmapBody = new TreeMap();
        for( Iterator it = hmap.keySet().iterator(); it.hasNext(); i++) {

            Integer targetL  = (Integer) it.next();
            Integer targetR  = (Integer) hmap.get( targetL );

            Integer I = new Integer( i );
            tmapTag.put( targetL, I );
            tmapBody.put( I, callFun( new Tree[] {
                cf.SeqTrace_tail( _iter() ),
                gen.mkIntLit( cf.pos, targetR.intValue() ) }));
        }
        i = 0;
        int[]  tags    = new int[ n ];
        Tree[] targets = new Tree[ n ];
        for( Iterator it = tmapTag.keySet().iterator(); it.hasNext(); i++) {
            Integer tagI = (Integer) it.next();
            tags[ i ] = tagI.intValue();
            Integer I = (Integer) tmapTag.get( tagI );
            targets[ i ] = (Tree) tmapBody.get( I );
        }
        return gen.Switch( gen.Ident( pos, targetSym ),
                           tags,
                           targets,
                           code_error()/*cannot happen*/ );

    }

    Tree currentMatches( Label label ) {
        switch( label ) {
        case Pair( Integer target, Label theLab ):
            return cf.Equals( gen.mkIntLit( cf.pos, target.intValue() ),
                              current() );
        }
        throw new ApplicationError("expected Pair label");
    }


    Tree code_state_NEW( int i ) { // precondition i != 0
        Tree stateBody = code_delta( i, Label.DefaultLabel );
        if( stateBody == null )
            stateBody = code_error();
        HashMap trans = ((HashMap[])dfa.deltaq)[ i ];

        TreeMap tmapTag = new TreeMap();
        TreeMap tmapBody = new TreeMap();
        int j = 0;
        for( Iterator labs = dfa.labels.iterator(); labs.hasNext(); j++) {
            Object label = labs.next();
            Integer next = (Integer) trans.get( label );

            Tree action = code_delta( i, (Label) label );

            if( action != null ) {
                Integer J = new Integer( j );
                tmapTag.put( ((Label.Pair) label).state, J );
                tmapBody.put( J, action );

                stateBody = gen.If( currentMatches((Label) label ),
                                    action,
                                    stateBody);
            }
        }
        final int n = tmapTag.keySet().size();
        j = 0;
        int[]  tags    = new int[ n ];
        Tree[] targets = new Tree[ n ];
        for( Iterator it = tmapTag.keySet().iterator(); it.hasNext(); j++) {
            Integer tagI = (Integer) it.next();
            tags[ j ] = tagI.intValue();
            Integer J = (Integer) tmapTag.get( tagI );
            targets[ j ] = (Tree) tmapBody.get( J );
        }
        if( n > 0 ) {
            return gen.Switch( gen.Ident( pos, targetSym ), tags, targets, code_error() );
        } else
            return code_error();
    }

    // calling the AlgebraicMatcher here
    Tree _cur_match( Tree pat ) {

        //System.out.println("RTiS._cur_match("+pat.toString()+")");
        //System.out.println("calling algebraic matcher on type:"+pat.type);

        //System.err.println( "curT"+currentElem().type().widen() );
        Matcher m = new Matcher( owner,//funSym,//this.funSym,
                                 currentElem(),
                                 defs.BOOLEAN_TYPE() );

        final HashMap freshenMap = new HashMap(); // sym2exp -> new sym
        HashMap helpMap3 = new HashMap();         // new sym -> original sym

        // "freshening": never use the same symbol more than once
        // (in later invocations of _cur_match)

        for( Iterator it = varsToExport.iterator(); it.hasNext(); ) {
            Symbol key = (Symbol) it.next();
            if( key.name.toString().indexOf("$") == -1 ) {
                this.helpMap2.put( key, helpMap.get( key ));
                // "freshening" by appending string ( a bit dangerous )
                Symbol newSym = key.cloneSymbol().setOwner( owner /*funSym*/ );
                newSym.name = Name.fromString( key.name + "%" );
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
        TreeCloner tc = new TreeCloner( cf.unit.global, freshenMap, Type.IdMap );

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


        Tree ts[] = new Tree[ helpMap3.keySet().size() ];
        int j = 0;
        for( Iterator it = helpMap3.keySet().iterator(); it.hasNext(); ) {
            Symbol vsym = (Symbol) it.next();
            Symbol hv   = (Symbol) helpMap3.get( vsym );
            //hv.setType( defs.LIST_TYPE( elementType ) ) ; DEBUG ALARM ?
            Tree refv   = gen.Ident(cf.pos, vsym);
            Tree refhv  = gen.Ident(cf.pos, hv);
            ts[ j++ ] = gen.Assign( refhv, refv );
            // System.out.println( "the assign" + res[ j - 1 ] );
        }

        Tree res = gen.mkBooleanLit( cf.pos, true ); // just `true'
        Tree theBody =  gen.mkBlock(ts, res);

        am.construct( m, new CaseDef[] {


            cf.gen.CaseDef( pat,           // if tree val matches pat -> update vars, return true
                            theBody/* "freshening */),
            cf.gen.CaseDef( cf.gen.Ident( pat.pos, defs.PATTERN_WILDCARD ),
                            gen.mkBooleanLit( pat.pos, false )) }, // else return false
                      true // do binding please
                      );

        return  am.toTree();
    }

    /** returns translation of transition with label from i.
     *  returns null if there is no such transition(no translation needed)
     */
    Tree code_delta( int i, Label label ) {
        HashMap hmap    = (HashMap) dfa.deltaq( i );
        Integer ntarget = (Integer) hmap.get( label );
        Tree    algMatchTree = null;
        if( ntarget == null )
            return null;

        //System.out.println("delta("+i+","+label+")" );
        Label theLab = null;
        switch(label) {
        case Label.Pair( Integer state, Label lab2 ):
            //assert ntarget == state;
            theLab = lab2;
            switch( lab2 ) {
            case TreeLabel( Tree pat ):
                algMatchTree = _cur_match( pat );
                break;
            }
            break;
        case DefaultLabel:
            throw new ApplicationError(); // should not happen
        }
        assert dfa.qbinders != null : "qbinders ?";

        Vector vars = dfa.qbinders[ i ];

        //System.out.println("dfa.qbinders[ i ]"+vars);

        if( vars == null ) vars = new Vector(); // TODO: make this more consistent
        assert vars != null;

        Tree stms[] = new Tree[ vars.size()
                                + ((algMatchTree != null )? 1 : 0 ) ];
        int j = 0;
        for( Iterator it = vars.iterator(); it.hasNext(); ) {
            Symbol var = (Symbol) it.next();

            Tree rhs = gen.Ident( pos, curSym );

            stms[ j++ ] = prependToHelpVar( var , rhs);
        }

        if( algMatchTree != null )
            stms[ j++ ] = algMatchTree ;

        Tree value = callFun( new Tree[] { cf.SeqTrace_tail( _iter() ),
                                           gen.mkIntLit( cf.pos, ntarget.intValue() ) } );

        return gen.mkBlock( pos, stms, value );
    }

    Tree stateWrap(int i) {
        if( i == 0 )
            return code_state0_NEW();
        return code_state_NEW( i );
    }

    /* returns statements that do the work of the right-transducer
     */
    Tree getStms( Tree trace, Unit unit, Tree body ) {

        TreeList stms = new TreeList();
        Tree loopbody = code_body_NEW();

        stms.append( gen.ValDef( iterSym, trace ) );
        stms.append( gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0 ) ) );
        stms.append( helpVarDefs );
        stms.append( gen.LabelDef( this.funSym,
                                   new Ident[] {
                                       gen.Ident( pos, iterSym ),
                                       gen.Ident( pos, stateSym )
                                   }, loopbody ));

        // bind variables handled by this righttracer
        for( Iterator it = seqVars.iterator(); it.hasNext(); ) {
            stms.append( bindVar( (Symbol) it.next() ) );
        }

        Transformer treeCloner = new Transformer(unit.global) {
                public Tree transform(Tree tree) {
                    tree = super.transform(tree);
                    if (tree.hasSymbol()) {
                        Object symbol = helpMap2.get(tree.symbol());
                        if (symbol != null) tree.setSymbol((Symbol)symbol);
                    }
                    return tree;
                }
            };

        return gen.mkBlock(stms.toArray(), treeCloner.transform( body ));
    }



    /** return the accumulator. (same as in LeftTracerInScala)
     *  todo: move tree generation of Unit somewhere else
     */
    Tree run_finished( int state ) {
        return gen.mkUnitLit(Position.FIRSTPOS);
    }

    Tree current() {   return gen.Ident( pos, targetSym );}

    Tree refHelpVar( Symbol realVar ) {
        Symbol hv = (Symbol)helpMap.get( realVar );
        assert hv != null : realVar;
        return gen.Ident(Position.FIRSTPOS, hv);
    }

    Tree assignToHelpVar( Symbol realVar, Tree rhs ) {
        Tree hv = refHelpVar( realVar );
        return gen.Assign( hv, rhs );
    }

    Tree bindVar(Symbol realVar) {
        Tree hv = refHelpVar( realVar );
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
        if( realVar.type().isSameAs( hv.getType())) {
            return gen.ValDef( realVar, hv ); // e.g. x @ _*
        }
        return gen.ValDef( realVar, cf.SeqList_head( hv ));

    }


}
