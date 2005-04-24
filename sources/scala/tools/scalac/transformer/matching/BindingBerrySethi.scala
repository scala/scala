

import scalac.CompilationUnit;
import scalac.Global ;
import scalac.ApplicationError ;
import scalac.ast.Tree ;
import scalac.util.Name ;
import scalac.util.Names ;
import Tree._ ;

import java.util._ ;

//import scala.compiler.printer.XMLTreePrinter ;
//import scala.compiler.printer.XMLAutomPrinter ;

package scala.tools.scalac.transformer.matching {
/** a Berry-Sethi style construction for nfas.
 *  this class plays is the "Builder" for the "Director" class
 *  WordRecognizer.
 */

class BindingBerrySethi(unit:CompilationUnit)  extends BerrySethi(unit) {

  // variables

  var deltaqRev: Array[HashMap ] = _;    // delta of Rev
  var defaultqRev:Array[Vector] = _;  // default transitions of Rev
  var qbinders:Array[Vector] = _;  // transitions <-> variables
  var revnfa:  NondetWordAutom = _ ;
  var varAt: HashMap = _;   // chi:    Positions -> Vars (Symbol)

  override def makeTransition(srcI: Integer, destI: Integer, label: Label): Unit = {
    val src  = srcI.intValue() ;
    val dest = destI.intValue() ;
    var arrows: Vector = _;
    var revArrows: Vector = _;
    val revLabel = new LPair( srcI, label  );
    label.match {
      case DefaultLabel() =>
        arrows = defaultq( src );
        revArrows = defaultqRev( dest );

      case _ =>
        arrows = deltaq( src ).get( label ).asInstanceOf[Vector];
        if( arrows == null )
          deltaq( src ).put( label,
                            {arrows = new Vector(); arrows} );
        revArrows = deltaqRev( dest ).get( revLabel ).asInstanceOf[Vector];
      if( revArrows == null )
        deltaqRev( dest ).put(revLabel, {revArrows = new Vector(); revArrows} );
    }
    arrows.add( destI );
    revArrows.add( srcI );
  }

  override def seenLabel( pat:Tree , label:Label  ): Unit = {
    var i = new Integer({pos = pos + 1; pos} );
    seenLabel( pat, i, label );
    pat.match {
      case Apply(_, _) | Literal( _ ) | Select(_, _) | Typed(_,_) =>
        this.varAt.put( i, activeBinders.clone() ); // below @ ?

      case Ident( name ) =>
        //assert ( pat.symbol() == Global.instance.definitions.PATTERN_WILDCARD )||( name.toString().indexOf("$") > -1 ) : "found variable label "+name;

        val binders = activeBinders.clone().asInstanceOf[Vector];
      /*
       if( pat.symbol() != Global.instance.definitions.PATTERN_WILDCARD) {
       binders.add( pat.symbol() );
       }
       */
        this.varAt.put( i, binders );
    }
  }

   override def initialize( pats:Array[Tree]  ): Unit = {
    this.varAt = new HashMap(); // Xperiment
    super.initialize( pats );
  }

   override def initializeAutom(): Unit = {
    super.initializeAutom();
    deltaqRev = new Array[HashMap]( pos );   // deltaRev
    defaultqRev = new Array[Vector]( pos );  // default transitions
    qbinders = new Array[Vector]( pos );    // transitions <-> variables

    var  j = 0;
    while(j < pos) {
      deltaqRev( j ) = new HashMap();
      defaultqRev( j ) = new Vector();
      qbinders( j ) = varAt.get( new Integer( j ) ).asInstanceOf[Vector];
      j = j + 1;
    }
    varAt.clear(); // clean up
  }


  override def automatonFrom( pat:Tree ,  finalTag:Integer ):  NondetWordAutom = {

    this.finalTag = finalTag ;
    //System.out.println( "enter automatonFrom("+ pat +")");
    pat.match {
      case Sequence( subexpr  ) =>

        initialize( subexpr );

        // (1) compute first + follow;
       pos = pos + 1;

        globalFirst = compFollow( subexpr );



      initializeAutom();   // explicit representation

      collectTransitions();

      val result =
        new NondetWordAutom(pos, // = nstates
                            labels,
                            initials,
                            finals,
                            deltaq,
                            defaultq,
                            qbinders);

      result.leftTrans = true;

      val revInitials = new TreeSet( finals.keySet() );
      /*
       pos++; // adding a state
       HashSet deltaqRev2[]   = new HashSet[ deltaqRev.length + 1];
       HashSet defaultqRev2[] = new HashSet[ deltaqRev.length + 1];
       HashSet qbinders[]     = new HashSet[ deltaqRev.length + 1];
       for(Iterator it = finals.keySet().iterator(); it.hasNext(); ) {

       }
       */
      val revFinals = new TreeMap();
      var it = initials.iterator();
      while(it.hasNext()) {
        revFinals.put( it.next(), finalTag);
      }
      revnfa = new NondetWordAutom(pos,
                                   labels,
                                   revInitials,
                                   revFinals,
				   deltaqRev,
				   defaultqRev,
                                   qbinders);

      revnfa.rightTrans = true;

      /*
       System.out.println("inBerrySethi");
       XMLAutomPrinter pr = new XMLAutomPrinter( System.out );
       pr.begin();
       pr.print( result );
       pr.print( revnfa );
       pr.end();
       System.out.println("initialsRev = "+initialsRev);
       System.out.println("outBerrySethi");
       */
      //System.exit(0);
      return result;                  //print();
    }
  }
}
}

