/*
** $Id$
*/
package scala.tools.nsc.matching ;

import scala.tools.nsc.util.Position;

mixin class CodeFactory requires TransMatcher  {

  import global._ ;

  import definitions._;             // standard classes and methods
  import typer.typed;               // methods to type trees
  import posAssigner.atPos;         // for filling in tree positions


  /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
  def SeqTraceType( elemType: Type  ):  Type = {
    appliedType(definitions.ListClass.typeConstructor,
                List(pairType(definitions.IntClass.info,
                              elemType)))
  }



  def pairType(left: Type, right: Type) = {
    appliedType( definitions.TupleClass(2).typeConstructor,
                List(left,right))
  }

  /**  returns `Iterator[ elemType ]' */
  def _seqIterType( elemType: Type  ):  Type = {
    appliedType( definitions.IteratorClass.typeConstructor,
                List(elemType))
  }

  /** returns A for T <: Sequence[ A ]
   */
  def getElemType_Sequence(tpe: Type):  Type = {
    //System.err.println("getElemType_Sequence("+tpe.widen()+")");
    val tpe1 = tpe.widen.baseType( definitions.SeqClass );

    if( tpe1 == NoType )
      Predef.error("arg "+tpe+" not subtype of Seq[ A ]");

    return tpe1.typeArgs( 0 );
  }


  // --------- these are new

  /** a faked switch statement
   */
  def Switch(condition: Array[Tree], body: Array[Tree], defaultBody: Tree): Tree = {
    //assert condition != null:"cond is null";
    //assert body != null:"body is null";
    //assert defaultBody != null:"defaultBody is null";
    var result = defaultBody;

    var i = condition.length-1;
    while (i >= 0) {
      result = If(condition(i), body(i), result);
      i = i - 1
    }

    return result ;
  }

  /** returns code `<seqObj>.elements' */
  def  newIterator( seqObj:Tree  ): Tree =
    Apply(Select(seqObj, newTermName("elements")), List());


  /** `it.next()'     */
  def  _next(iter: Tree) =
    Apply(Select(iter, definitions.Iterator_next), List());


  /** `it.hasNext()'  */
  def _hasNext(iter: Tree) =
    Apply(Select(iter, definitions.Iterator_hasNext), List());


  /** `!it.hasCur()'  */
  def  _not_hasNext( iter:Tree  ) =
    Apply(Select(_hasNext(iter), definitions.Boolean_not), List());


  /** `trace.isEmpty' */
  def isEmpty( iter: Tree  ):  Tree =
    Apply(Select(iter, definitions.List_isEmpty), List());


  /** `arg.head' */
  def SeqList_head( arg: Tree ) =
    Apply(Select(arg, definitions.List_head), List());


  def Negate(tree: Tree) = tree match {
    case Literal(Constant(value:Boolean))=>
      Literal(Constant(!value))
    case _ =>
      Apply(Select(tree, definitions.Boolean_not), List());
  }

  /*protected*/ def And(left: Tree, right: Tree): Tree = left match {
    case Literal(Constant(value:Boolean)) =>
      if(value) right else left;
    case _ =>
      right match {
        case Literal(Constant(true)) =>
	  left;
        case _ =>
          Apply(Select(left, definitions.Boolean_and), List(right));
      }
  }

  /*protected*/ def Or(left: Tree, right: Tree): Tree = {
    left match {
/*
      case If(cond: Tree, thenp: Tree, Literal(Constant(false))) =>  // little opt, frequent special case
        If(cond, thenp, right)
*/
      case Literal(Constant(value: Boolean))=>
	if(value) left else right;
      case _ =>
        right match {
          case Literal(Constant(false)) =>
	    left;
          case _ =>
            Apply(Select(left, definitions.Boolean_or), List(right));
        }
    }
  }

  // used by Equals
  /*
  private def getCoerceToInt(left: Type): Symbol = {
    val sym = left.nonPrivateMember( nme.coerce );
    //assert sym != Symbol.NONE : Debug.show(left);

    sym.alternatives.find {
      x => x.info match {
        case MethodType(vparams, restpe) =>
          vparams.length == 0 && isSameType(restpe,definitions.IntClass.info)
      }
    }.get
  }
  */
  // used by Equals
/*
  private def getEqEq(left: Type, right: Type): Symbol = {
    //Console.println("getEqeq of left  ==  "+left);
    val sym = left.nonPrivateMember( nme.EQEQ );


    //if (sym == NoSymbol)
    //  error("no eqeq for "+left);
    //    : Debug.show(left) + "::" + Debug.show(left.members());

    var fun: Symbol  = null;
    var ftype:Type  = null; // faster than `definitions.AnyClass.tpe'
    sym.alternatives.foreach {
      x =>
        //Console.println("getEqEq: "+x);
        val vparams = x.info.paramTypes;
        //Console.println("vparams.length ==  "+vparams.length);

        if (vparams.length == 1) {
          val vptype = vparams(0);
          //Console.println("vptype ==  "+vptype);
          //Console.println("   ftype ==  "+ftype);
          //Console.println("   cond1 ==  "+isSubType(right, vptype));
          //Console.println("   cond2("+vptype+","+ftype+") ==  "+(ftype == null || isSubType(vptype, ftype)));
          //Console.println("vptype.getClass "+vptype.getClass());
          if (isSubType(right, vptype) && (ftype == null || isSubType(vptype, ftype)) ) {
            fun = x;
            ftype = vptype;
            //Console.println("fun now: "+fun+"  ftype now "+ftype);
          }
        }
    }
    //if (fun == null) scala.Predef.error("couldn't find eqeq for left"+left);
    fun;
  }
*/
  def  Equals(left: Tree , right: Tree ): Tree = Apply(Select(left, nme.EQEQ), List(right));

  //deprecated
  def ThrowMatchError(pos: Int, tpe: Type ) =
    atPos(pos) {
      Throw(
        New(
          TypeTree(definitions.MatchErrorClass.tpe),
          List(List(
            Literal(cunit.toString()),
            Literal(Position.line(cunit.source, pos))))))
    }
//new
  def ThrowMatchError(pos: Int, obj: Tree ) =
    atPos(pos) {
      Throw(
        New(
          TypeTree(definitions.MatchErrorClass.tpe),
          List(List(
            obj
          ))))
    }

/*
 Apply(
      TypeApply(
        gen.mkRef(definitions.MatchError_fail),
        List(TypeTree(tpe))
      ),
      List(
        Literal(cunit.toString()),
        Literal(Position.line(cunit.source, pos))
      )
    );
*/

  /* // ?!
  def ThrowMatchError(pos:int , tree:Tree ) =
   Apply(
     gen.mkRef(definitions.MatchError_report),
     List(
       Literal(cunit.toString()),
       Literal(Position.line(cunit.source, pos)),
       tree
     )
   );
  */

//  def Error(pos: Int) =
//    ThrowMatchError(pos);


  /*
  def newPair(left: Tree, right: Tree) =
    New(
      Apply(
        gen.mkRef(definitions.TupleClass(2)),
        List(left,right)
      )
    );
  */
}

