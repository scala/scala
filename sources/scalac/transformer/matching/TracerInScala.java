package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;
import scalac.util.Name;
import scalac.util.Names;

import scalac.transformer.TransMatch.Matcher ;

import java.util.* ;

import ch.epfl.lamp.util.Position;


/** 2do: factor common things of LeftTracerInScala and RightTracerInScala
 */
public class TracerInScala extends Autom2Scala {

    protected boolean optimize = true;

    HashMap helpMap;

      public TracerInScala( DetWordAutom dfa,
			    Type elementType,
			    Symbol ownerSym,
			    CodeFactory cf ) {
	super( dfa, elementType, ownerSym, cf );

	this.helpMap = new HashMap();

      }

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

