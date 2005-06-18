package scala.xml.dtd;

import ContentModel.ElemName ;
import scala.util.automata._ ;

class ElementValidator() extends Function1[Node,Boolean] {

  protected var _dfa: DetWordAutom[ElemName] = _;

  def setContentModel(cm:ContentModel) = cm match {
    //case ANY         => true ;
    //case EMPTY       => //@todo
    //case PCDATA      =>
    //case m@MIXED(r)  =>
    case ELEMENTS( r ) =>
      val nfa = ContentModel.Translator.automatonFrom(r, 1);
      _dfa = new SubsetConstruction(nfa).determinize;
  }

  def getIterator(ns: Seq[Node]): Iterator[ElemName] =
    ns . toList
	   . filter { x => x.namespace == null }
	   . map { x => ElemName(x.label) }
	   . elements;
 /** @pre _dfa != null
 */
  def runDFA(ns: Seq[Node]): Boolean = {
    var q = 0;
    val it = getIterator(ns);
    //Console.println("it empty from the start? "+(!it.hasNext));
    while( it.hasNext ) {
      val e = it.next;
      //  Console.println("next = "+e);
      //  Console.println(" got :"+ElemName(e));
      //  Console.println("delta:" + _dfa.delta(q));

      _dfa.delta(q).get(e).match {
         case Some(p) => q = p;
         case _       => throw ValidationException("element "+e+" not allowed here")
      }
        //Console.println("q now " + q);
    }
    _dfa.isFinal(q)
  }

  def apply(n: Node): Boolean = {
    var res = (null == _dfa) || runDFA(n.child);
	 // res = ... // @todo attributes
	res
  }

}
