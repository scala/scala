/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml;

/** Attribute information item, and linked list of attribute information items.
 *  These are triples consisting of prefix,key,value. To obtain the namespace,
 *  getNamespace must be called with the parent. If next is null, this is
 *  the last attribute in the MetaData list.
 *
 * either an UnprefixedAttribute or a PrefixedAttribute
 *
 * @todo _vlue should be a normalized attribute value
 */
[serializable]
abstract class MetaData extends Iterable[MetaData] {

  /** appends given MetaData items to this MetaData list */
  def append(m: MetaData): MetaData =
    next.append(copy(m));

  def apply(s:String) = getValue(s);

  def apply(uri:String, scp:NamespaceBinding, k:String)= getValue(uri, scp, k);

   def containedIn1(m: MetaData): Boolean =
     m.equals1(this) || containedIn1(m.next);

  /** returns a copy of this MetaData item with next field set to argument */
  def copy(next: MetaData): MetaData;

  /** if owner is the element of this metadata item, returns namespace */
  def getNamespace(owner: Node): String;

  def hasNext = (Null != next);

  def length: Int = length(0);

  def length(i: Int): Int = next.length(i + 1);

  def isPrefixed: Boolean;

  //def containedIn(m:MetaData): Boolean;

  //def deepCopy: MetaData;

  //def deepCopy(tail:MetaData): MetaData;

  /** deep equals method */
  override def equals(that: Any) = {
    that match {
      case m: MetaData =>
        var res = (this.length == m.length) && (this.hashCode() == m.hashCode());
        val it = this.elements;
        while (res && it.hasNext) { res = it.next.containedIn1(m) }
      res

      case _          => false;
    }
  }

  /** returns an iterator on attributes */
  def elements = new Iterator[MetaData] {
    var x: MetaData = MetaData.this;
    def hasNext = Null != x;
    def next = {
      val y = x;
      x = x.next;
      y
    }
  }

  /* returns a sequences of "pseudo nodes" that contain attribute info.
     not sure if this useful, and it violates contract for nodes...
  def nodes = {
    class NodeProxy(last:MetaData) extends Node {
      override def prefix   = last match {
        case p:PrefixedAttribute => p.pre;
        case _ => null
      }
      override def label = "@"+last.key;
      override def child = Text(last.value);
      override def text = last.value
    }
    val ns = new Array[Node](this.length);
    var i = 0;
    val it = elements;
    while(it.hasNext) {
      val a = it.next;
      ns(i) = new NodeProxy(a);
      i = i + 1;
    }
    val seq = array2seq(ns);
    NodeSeq.fromSeq(seq);
  }
   */

  /** shallow equals method */
  def equals1(that: MetaData): Boolean;

  /** filters this sequence of meta data */
  def filter(f: MetaData => Boolean): MetaData = {
    if (f(this)) copy(next filter f) else next filter f;
  }

  /** returns key of this MetaData item */
  def key: String;

  /** returns key of this MetaData item */
  def value: Any;

  /** maps this sequence of meta data */
  def map(f: MetaData => Text): List[Text] = f(this)::(next map f);

  /** returns Null or the next MetaData item */
  def next: MetaData;

  /** gets value of unqualified (unprefixed) attribute with given key */
  def getValue(key: String): Any;

  /** gets value of qualified (prefixed) attribute with given key */
  def getValue(namespace: String, owner: Node, key: String): Any =
    getValue(namespace, owner.scope, key);

  /** gets value of qualified (prefixed) attribute with given key */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): Any;

  override def hashCode(): Int;

  def toString1(): String = {
    val sb = new StringBuffer();
    toString1(sb);
    sb.toString();
  }

  //appends string representations of single attribute to StringBuffer
  def toString1(sb:StringBuffer): Unit;

  override def toString(): String = {
    val sb = new StringBuffer();
    toString(sb);
    sb.toString();
  }

  def toString(sb: StringBuffer): Unit = {
    sb.append(' ');
    toString1(sb);
    next.toString(sb);
  }

  def wellformed(scope: NamespaceBinding): Boolean;

}
