package scalac.transformer.matching ;

import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** 2do: factor common things of LeftTracerInScala and RightTracerInScala
 */
public class TracerInScala extends Autom2Scala {

    protected boolean optimize = true;


    public TracerInScala( DetWordAutom dfa,
                          Type elementType,
                          Symbol ownerSym,
                          CodeFactory cf ) {
        super( dfa, elementType, ownerSym, cf );

    }

}

