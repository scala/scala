package scalac.transformer.matching ;

import scalac.ast.Tree ;
import scalac.ast.TreeInfo ;
import scalac.symtab.Symbol ;
import scalac.symtab.Type ;
import Tree.Literal ;

/** this class represents the label that a transition in an automaton may carry.
 *  these get translated to specific (boolean) tests
 */

public class Label {


      public case DefaultLabel;
      public case SimpleLabel( Literal lit );
      public case TreeLabel( Tree pat ); // Apply, Sequence

      public case TypeLabel( Type tpe ); // Apply, Sequence

      public case Pair( Integer state, Label lab );

      //public case RLabel( Object rstate, Label lab, Symbol vars[] );

      public int hashCode() {
            switch( this ) {
            case DefaultLabel:
                  return 0;
            case SimpleLabel( Literal lit ):
                  return lit.value.hashCode();
            case TreeLabel( Tree pat ):
                // if pat is an  Apply, than this case can only be correctly
                // handled there are no other similar Applys (nondeterminism)
                return pat.type().hashCode();
            case TypeLabel( Type type ):
                  return type.hashCode();
            default:
                  return super.hashCode();
            }
      }

      public boolean equals( Object o ) {
            if( !(o instanceof Label ))
                  return false;
            Label oL = (Label) o;
            //System.out.print(this + " equals " + oL);
            switch( this ) {
            case DefaultLabel:
                  switch( oL ) {
                  case DefaultLabel:
                        return true;
                  } //
                  break;
            case SimpleLabel( Literal lit ):
                  switch( oL ) {
                  case SimpleLabel( Literal lit2 ):
                        return /*(lit.kind == lit2.kind)
				 && */lit.value.equals( lit2.value );
                  }
                  break;
            case TreeLabel( Tree pat ):
                  switch( oL ) {
                  case TreeLabel( Tree pat2 ):
		      switch( pat ) {
		      case Apply( _, _ ):
			  switch( pat2 ) {
			  case Apply( _, _ ):
			      return TreeInfo.methSymbol( pat ) == TreeInfo.methSymbol( pat2 );
			  }
		      }
		      return pat == pat2;
                  }
                  break ;
            case TypeLabel( Type tpe ):
                  switch( oL ) {
                  case TypeLabel( Type tpe2):
                        return tpe.equals( tpe2 );
                  }
                  break ;
            case Pair( Integer state, Label lab ):
                  switch( oL ) {
                  case Pair(  Integer state2, Label lab2 ):
                        return  state.equals( state2 )
                                 && lab.equals( lab2 ) ;
                  }
                  break;
            }
            return false;
      }


      public String toString2() {
            String ext = System.getProperty("extendedMatching");
            if(( ext != null )
               && ext.equals( "true" )) {
                  switch( this ) {
                  case DefaultLabel:
                        return "<>:p"+p;
                  case SimpleLabel( Literal lit ):
                        return lit.toString()+":p"+p;
                  case TreeLabel( Tree pat ):
                        return pat.getType()+":p"+p;

                  }
            }
            throw new scalac.ApplicationError
                  ("this never happens");
      }

      public String toString() {

            switch( this ) {
            case DefaultLabel:
                  return "<>";
            case SimpleLabel( Literal lit):
                  return lit.toString();
            case TreeLabel( Tree pat):
                  return pat.toString();
            case TypeLabel( Type tpe ):
                  return tpe.toString();
            case Pair( Integer state, Label lab  ):
                  return "("+state.toString()+","+lab.toString()+")";
            }
            throw new scalac.ApplicationError("this never happens");
      }

      int p = -1; // tree state - only needed for extended matching


}
