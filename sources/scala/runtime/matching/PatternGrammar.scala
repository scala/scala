package scala.runtime.matching ;
import scala.util.grammar._;
import scala.collection.{ immutable, mutable, Map, Set };

object PatternGrammar {
  def encodeTreeRHS(x:TreeRHS):String = x match {
    case LabelledRHS(TestLabel(test),hnt) =>
      val sb = new StringBuffer();
      sb.append(test);
      sb.append('/');
      sb.append(hnt);
      sb.toString();
    case AnyTreeRHS => "_"
  }
  def decodeTreeRHS(s:String):TreeRHS = s.charAt(0) match {
    case '_' => AnyTreeRHS;
    case _   =>
      val s2   = s.split("/");
      val test = Integer.parseInt(s2(0));
      val hnt  = Integer.parseInt(s2(1));
      LabelledRHS(TestLabel(test),hnt);
  }
  def encodeHedgeRHS(x:HedgeRHS):String = x match {
    case ConsRHS(tnt, hnt) =>
      val sb = new StringBuffer();
      sb.append(tnt);
      sb.append('/');
      sb.append(hnt);
      sb.toString();
    case AnyHedgeRHS => "A"
  }
  def decodeHedgeRHS(s:String):HedgeRHS = s.charAt(0) match {
    case 'A' => AnyHedgeRHS;
    case _   =>
      val s2   = s.split("/");
      val tnt = Integer.parseInt(s2(0));
      val hnt  = Integer.parseInt(s2(1));
      ConsRHS(tnt,hnt);
  }
  def decode(str: String, tests: PatternTests): PatternGrammar = {
    val sq:Seq[String] = str.split("#");
    //Console.println("sq.length"+sq.length);
    val it = sq.elements;

    def readIntArray(length: Int): Array[Int] = {
      val arr = new Array[Int](length);
      var i = 0;
      while(i < length) {
        arr(i) = Integer.parseInt(it.next);
        i = i + 1;
      }
      arr
    }
    def readBitSet(n: Int): immutable.BitSet = {
      val len = (n >>> 5) + (if( (n & 0x1F)!= 0 ) 1 else 0);
      new immutable.BitSet(n,readIntArray(len),false);
    }
    def readTransitionsT: immutable.Set[TreeRHS] = {
      val trans = it.next;
      //Console.println("T trans = "+trans);
      var r = new immutable.ListSet[TreeRHS];
      if(trans.length() == 0)
        return r;
      val s:Array[String] = trans.split(",");
      var i = 1;
      while( i < s.length) {
        r = r + PatternGrammar.decodeTreeRHS(s(i));
        i = i + 1;
      }
      r
    }
    def readTransitionsH: immutable.Set[HedgeRHS] = {
      val trans = it.next;
      //Console.println("H trans = "+trans);
      val s:Array[String] = trans.split(",");
      var r = new immutable.ListSet[HedgeRHS];
      if(trans.length() == 0)
        return r;
      var i = 1;
      while(i < s.length) {
        r = r + PatternGrammar.decodeHedgeRHS(s(i));
        i = i + 1;
      }
      r
    }

    val _nTreeNT  = Integer.parseInt( it.next );
    //Console.println("read _nTreeNT:"+_nTreeNT);
    val _nHedgeNT = Integer.parseInt( it.next );
    //Console.println("read _nHedge:"+_nHedgeNT);
    val _treeInitials  = readBitSet( _nTreeNT );
    //Console.println("read treeInitials:" + _treeInitials.toArray);
    val _hedgeInitials = readBitSet( _nHedgeNT );
    //Console.println("read hedgeInitials:"  + _hedgeInitials.toArray);
    val _isNullable    = readBitSet( _nHedgeNT );
    //Console.println("read isNullable:"  + _isNullable.toArray);
    var i = 0;
    val _treeTransitions = new Array[immutable.Set[TreeRHS]](_nTreeNT);
    while(i < _nTreeNT) {
      _treeTransitions(i) = readTransitionsT;
      i = i + 1;
    }
    i = 0;
    val _hedgeTransitions = new Array[immutable.Set[HedgeRHS]](_nHedgeNT);
    while(i < _nHedgeNT) {
      _hedgeTransitions(i) = readTransitionsH;
      i = i + 1;
    }
    new PatternGrammar {
      val nTreeNT          = _nTreeNT;
      val nHedgeNT         = _nHedgeNT;
      val treeInitials     = _treeInitials;
      val hedgeInitials    = _hedgeInitials;
      val isNullable       = _isNullable;
      val treeTransitions  = _treeTransitions;
      val hedgeTransitions = _hedgeTransitions;
      val vars             = new Array[Int](0); // @todo
      final def test(i:Int, inp:Any) = tests(i,inp);
    }
  }
}

/** runtime representation of patterns. This class augments
 *  scala.util.grammar.TreeHedgeGrammar, with an abstract representation
 *  of variable bindings. Variables are simply consecutive integers,
 *  following pre-order of occurrence in pattern
 *  @caseVars an array, field i holding the number of variables in case i
 */
abstract class PatternGrammar extends scala.util.grammar.ImmutableTreeHedgeGrammar {

  val vars:Array[Int];

  def test(test:Int, inp:Any): Boolean;

  def isSequenceType: Boolean = { treeInitials.toSet(true).isEmpty };

  def encode: String = {
    val sb = new StringBuffer();
    def writeInt(i: Int) = {
      sb.append(i);
      sb.append('#');
    }
    def writeIntArray(arr:Array[Int]) = {
      var i = 0;
      while(i < arr.length) {
        sb.append(arr(i));
        sb.append('#');
        i = i + 1;
      }
    }

    writeInt( nTreeNT );                    // nTreeNT
    writeInt( nHedgeNT );                   // nHedgeNT
    writeIntArray(treeInitials.toArray);    // treeInitials
    writeIntArray(hedgeInitials.toArray);   // hedgeInitials
    writeIntArray(isNullable.toArray);      // isNullable
    // treeTransitions
    var i = 0;
    while(i < nTreeNT) {
      val set = treeTransitions(i).elements;
      sb.append('n');
      while( set.hasNext ) {
        sb.append(',');
        sb.append(PatternGrammar.encodeTreeRHS(set.next));
      }
      sb.append('#');
      i = i + 1;
    }
    // hedgeTransitions
    i = 0;
    while(i < nHedgeNT) {
      val set = hedgeTransitions(i).elements;
      sb.append('n');
      while( set.hasNext ) {
        sb.append(',');
        sb.append(PatternGrammar.encodeHedgeRHS(set.next));
      }
      sb.append('#');
      i = i + 1;
    }
    sb.toString();
  }


}
