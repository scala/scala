/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */


import scala.tools.util.Position;

import scalac._;
import scalac.ast._;
import scalac.atree.AConstant._;
import scalac.util._;
import scalac.symtab._;
//import PatternNode._;
import Tree._;
package scala.tools.scalac.transformer.matching {

class CodeFactory(val unit: CompilationUnit, pos1: Int) extends PatternTool(unit) {

    var pos = pos1;

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
      result = gen.If(condition(i), body(i), result);
      i = i - 1
    }

    return result ;
  }

  /** returns  `List[ Tuple2[ scala.Int, <elemType> ] ]' */
  def SeqTraceType( elemType: Type  ):  Type = {
    defs.LIST_TYPE(pairType(defs.int_TYPE(), elemType));
  }

  /**  returns `Iterator[ elemType ]' */
  def _seqIterType( elemType: Type  ):  Type = {
    defs.ITERATOR_TYPE(elemType);
  }

    /**  returns `<seqObj.elements>' */
  def newIterator( seqObj:Tree , elemType:Type  ):  Tree  = {
    gen.mkApply__(gen.Select(seqObj, defs.ITERABLE_ELEMENTS()));
  }

  /** returns code `<seqObj>.elements'
   *  the parameter needs to have type attribute `Sequence[<elemType>]'
   */
  def  newIterator( seqObj:Tree  ): Tree = {
    newIterator( seqObj, getElemType_Sequence( seqObj.getType() ));
  }

    // EXPERIMENTAL
  def newRef(init: Tree): Tree = {
    //System.out.println( "hello:"+refSym().type() );
    gen.New( gen.mkApplyTV( gen.mkPrimaryConstructorGlobalRef( pos, defs.REF_CLASS),
                           Predef.Array[Type] ( init.getType() ),
                           Predef.Array[Tree] ( init ) ));
  }

  /** returns A for T <: Sequence[ A ]
   */
  def getElemType_Sequence( tpe: Type):  Type = {
    //System.err.println("getElemType_Sequence("+tpe.widen()+")");
    val tpe1 = tpe.widen().baseType( defs.SEQ_CLASS );

    if( tpe1 == Type.NoType )
      throw new ApplicationError("arg "+tpe+" not subtype of Seq[ A ]");

    return tpe1.typeArgs()( 0 );
  }

    /** returns A for T <: Iterator[ A ]
     */
    def getElemType_Iterator( tpe: Type  ): Type = {
	//System.err.println("getElemType_Iterator("+tpe+")");

	val tpe1 = tpe.widen().baseType( defs.ITERATOR_CLASS );

	tpe1 match {
	case Type.TypeRef(_,_,args) =>
	    return args( 0 );
	case _ =>
	  throw new ApplicationError("arg "+tpe+" not subtype of Iterator[ A ]");
	}

    }

  /** `it.next()'
   */
  def  _next( iter: Tree  ):  Tree = {
    gen.mkApply__(gen.Select(iter, defs.ITERATOR_NEXT()));
  }

  /** `it.hasNext()'
   */
   def _hasNext(  iter: Tree ):  Tree =  {
     gen.mkApply__(gen.Select(iter, defs.ITERATOR_HASNEXT()));
   }

  /** `!it.hasCur()'
   */
  def  _not_hasNext( iter:Tree  ): Tree = {
    gen.mkApply__(gen.Select(_hasNext(iter), defs.BOOLEAN_NOT()));
  }

  /** `trace.isEmpty'
   */
  def isEmpty( iter: Tree  ):  Tree = {
    gen.mkApply__(gen.Select(iter, defs.LIST_ISEMPTY()));
  }

  def SeqTrace_headElem( arg: Tree  ):  Tree = { // REMOVE SeqTrace
    val t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
    gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 2)));
  }

  def SeqTrace_headState( arg: Tree  ):  Tree = { // REMOVE SeqTrace
    val t = gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
    gen.mkApply__(gen.Select(t, defs.TUPLE_FIELD(2, 1)));

  }

  def SeqTrace_tail( arg: Tree ): Tree = { // REMOVE SeqTrace
    gen.mkApply__(gen.Select(arg, defs.LIST_TAIL()));
    }

    /** `<seqlist>.head()'
     */
  def SeqList_head( arg: Tree ): Tree = {
    gen.mkApply__(gen.Select(arg, defs.LIST_HEAD()));
  }

  def Negate(tree: Tree): Tree = {
    tree match {
      case Literal(BOOLEAN(value))=>
        gen.mkBooleanLit(tree.pos, !value);
      case _ =>
        gen.mkApply__(gen.Select(tree, defs.BOOLEAN_NOT()));
    }
  }

  /*protected*/ def And(left: Tree, right: Tree): Tree = {
    left match {
      case Literal(BOOLEAN(value)) =>
	return if(value) right else left;
      case _ =>
    }
    right match {
      case Literal(BOOLEAN(value)) =>
	if (value) return left;
      case _ =>
    }
    gen.mkApply_V(gen.Select(left, defs.BOOLEAN_AND()), Predef.Array[Tree](right));
  }

  /*protected*/ def Or(left: Tree, right: Tree): Tree = {
    left match {
      case Literal(BOOLEAN(value))=>
	    return if(value) left else right;
      case _ =>
    }
    right match {
      case Literal(BOOLEAN(value)) =>
	if (!value) return left;
      case _ =>
    }
    gen.mkApply_V(gen.Select(left, defs.BOOLEAN_OR()), Predef.Array[Tree](right));
  }

  // used by Equals
  private def getCoerceToInt(left: Type):  Symbol = {
    val sym = left.lookupNonPrivate(Names.coerce);
    //assert sym != Symbol.NONE : Debug.show(left);
    val syms = sym.alternativeSymbols();
    var i = 0; while( i < syms.length) {
      syms(i).info() match {
        case Type.MethodType(vparams, restpe) =>
          if (vparams.length == 0 && restpe.isSameAs(defs.INT_TYPE()))
            return syms(i);
        case _ =>
      }
      i = i + 1;
    }
    //assert false : Debug.show(left);
    throw new ApplicationError(left);
  }

  // used by Equals
  private def getEqEq(left: Type, right: Type): Symbol = {
    val sym = left.lookupNonPrivate(Names.EQEQ);
    //assert sym != Symbol.NONE
    //    : Debug.show(left) + "::" + Debug.show(left.members());
    var fun: Symbol  = null;
    var ftype:Type  = defs.ANY_TYPE();
    val syms = sym.alternativeSymbols();
    var  i = 0; while(i < syms.length) {
      val vparams = syms(i).valueParams();
      if (vparams.length == 1) {
        val vptype = vparams(0).info();
        if (right.isSubType(vptype) && vptype.isSubType(ftype)) {
          fun = syms(i);
          ftype = vptype;
        }
      }
      i = i + 1
    }
    //assert fun != null : Debug.show(sym.info());
    fun;
  }

  /*protected*/ def  Equals(left1:Tree , right1:Tree ): Tree = {
    var left = left1;
    var right = right1;
    val ltype = left.getType().widen();
    var rtype = right.getType().widen();
    if (ltype.isSameAs(rtype)
        && (ltype.isSameAs(defs.CHAR_TYPE())
            || ltype.isSameAs(defs.BYTE_TYPE())
            || ltype.isSameAs(defs.SHORT_TYPE())))
      {
        right = gen.mkApply__(gen.Select(right, getCoerceToInt(rtype)));
        rtype = defs.INT_TYPE();
      }
    val eqsym = getEqEq(ltype, rtype);
    gen.mkApply_V(gen.Select(left, eqsym), Predef.Array[Tree](right));
  }

  /*protected*/ def ThrowMatchError(pos: Int, tpe: Type ): Tree = {
    gen.mkApplyTV(
      gen.mkGlobalRef(pos, defs.MATCHERROR_FAIL()),
      Predef.Array[Tree](gen.mkType(pos, tpe)),
      Predef.Array[Tree](
        gen.mkStringLit(pos, unit.toString()),
        gen.mkIntLit(pos, Position.line(pos))
      ));
  }

  def ThrowMatchError(pos:int , tpe:Type , tree:Tree ):  Tree = {
    gen.mkApplyTV(
      gen.mkGlobalRef(pos, defs.MATCHERROR_REPORT()),
      Predef.Array[Tree](gen.mkType(pos, tpe)),
      Predef.Array[Tree](
        gen.mkStringLit(pos, unit.toString()),
        gen.mkIntLit(pos, Position.line(pos)),
        tree
      ));
  }

  def Error(pos: Int, tpe: Type): Tree = {
    gen.mkApplyTV(
      gen.mkGlobalRef(pos, defs.MATCHERROR_FAIL()),
      Predef.Array[Tree](gen.mkType(pos, tpe)),
      Predef.Array[Tree](
        gen.mkStringLit(pos, unit.toString()),
        gen.mkIntLit(pos, Position.line(pos))
      ));
  }


  def pairType(left: Type, right: Type): Type = {
    defs.TUPLE_TYPE(Predef.Array[Type] ( left, right ) );
  }

    def newPair(left: Tree, right: Tree): Tree = {
      gen.New(
        gen.mkApplyTV(
          gen.mkPrimaryConstructorGlobalRef( pos, defs.TUPLE_CLASS(2)),
          Predef.Array[Type] ( left.getType(), right.getType() ),
          Predef.Array[Tree] ( left, right )));
    }

}
}
