import scalac.ApplicationError;
import scalac.ast.Tree ;
import scalac.ast.TreeInfo ;
import scalac.symtab.Symbol ;
import scalac.symtab.Type ;
import Tree.Literal ;

/** this class represents the label that a transition in an automaton may carry.
 *  these get translated to specific (boolean) tests
 */
package scala.tools.scalac.transformer.matching {

class Label {


      //case class RLabel( Object rstate, Label lab, Symbol vars[] );

      override def hashCode(): Int = match {
        case DefaultLabel() =>
          return 0;
        case SimpleLabel( lit )=>
          return lit.value.hashCode();
        case TreeLabel( pat ) =>
          // if pat is an  Apply, than this case can only be correctly
          // handled there are no other similar Applys (nondeterminism)
          return pat.getType().hashCode();
        case TypeLabel( tpe ) =>
          return tpe.hashCode();
        case _ =>
          return super.hashCode();
      }

      override def  equals( o: Any ): Boolean = {
            if( !(o.isInstanceOf[Label] ))
              return false;
        val oL = o.asInstanceOf[Label];
        //System.out.print(this + " equals " + oL);
         this.match {
           case DefaultLabel()=>
           oL.match {
             case DefaultLabel() =>
               return true;
             case _ => false;
           } //
           case SimpleLabel( lit ) =>
           oL.match {
             case SimpleLabel( lit2 ) =>
             return /*(lit.kind == lit2.kind)
	     && */lit.value.equals( lit2.value );
             case _ => false;
           }

           case TreeLabel( pat ) =>
             oL.match {
               case TreeLabel( pat2 ) =>
	       pat.match {
		 case Tree.Apply( _, _ ) =>
		   pat2.match {
		     case Tree.Apply( _, _ ) =>
		       return TreeInfo.methSymbol( pat ) == TreeInfo.methSymbol( pat2 );
		   }
                 case _ => false;
	       }
               case _ => false
             }

           case TypeLabel( tpe ) =>
             oL.match {
               case TypeLabel( tpe2) =>
                 return tpe.equals( tpe2 );
               case _ => false;
             }
           case LPair(  state,  lab ) =>
             oL.match {
               case LPair(  state2,  lab2 ) =>
                 return  state.equals( state2 ) && lab.equals( lab2 ) ;
               case _ => false;
             }
           case _ => return false;
         }
      }


  def toString2(): String  = {
    val ext = System.getProperty("extendedMatching");
    if(( ext != null )&& ext.equals( "true" )) {
      this.match {
        case DefaultLabel() =>
          return "<>:p"+p;
        case SimpleLabel( lit ) =>
          return lit.toString()+":p"+p;
        case TreeLabel( pat ) =>
          return pat.getType().toString()+":p"+p;

        case _ => throw new ApplicationError("this never happens");
      }
    }
    throw new ApplicationError("this never happens");
  }

  override def toString():  String = {

    this.match {
      case DefaultLabel() =>
        return "<>";
      case SimpleLabel(  lit) =>
        return lit.toString();
      case TreeLabel(pat) =>
        return pat.toString();
      case TypeLabel( tpe ) =>
        return tpe.toString();
      case LPair( state, lab  ) =>
        return "("+state.toString()+","+lab.toString()+")";
      case _ =>
        throw new ApplicationError("this never happens");
    }
  }

  val p = -1; // tree state - only needed for extended matching


}


  case class DefaultLabel() extends Label;
  case class SimpleLabel(  lit: Literal ) extends Label;
  case class TreeLabel(  pat: Tree ) extends Label; // Apply, Sequence

  case class TypeLabel( tpe: Type  ) extends Label; // Apply, Sequence

  case class LPair(  state: Integer, lab: Label  ) extends Label;
}

