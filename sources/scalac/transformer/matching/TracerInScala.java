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


    public TracerInScala( DetWordAutom dfa,
                          Type elementType,
                          Symbol ownerSym,
                          CodeFactory cf ) {
        super( dfa, elementType, ownerSym, cf );

    }

}

