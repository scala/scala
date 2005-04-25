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
abstract class MetaData extends Iterable[MetaData] with java.io.Serializable {

  /** appends given MetaData items to this MetaData list */
  def append(m: MetaData): MetaData =
    next.append(copy(m));

   def containedIn1(m:MetaData): Boolean =
     m.equals1(this) || containedIn1(m.next);

  /** returns a copy of this MetaData item with next field set to argument */
  def copy(next: MetaData): MetaData;

  /** if owner is the element of this metadata item, returns namespace */
  def getNamespace(owner: Node): String;

  def hasNext = (Null != next);

  def length: Int = length(1);

  def length(i:Int): Int = next.length(i+1);

  def isPrefixed: Boolean;

  //def containedIn(m:MetaData): Boolean;

  //def deepCopy: MetaData;

  //def deepCopy(tail:MetaData): MetaData;

  /** deep equals method */
  override def equals(that: Any) = {
    that.match {
      case m:MetaData =>
        var res = (this.length == m.length) && (this.hashCode() == m.hashCode());
        val it = this.elements;
        while (res && it.hasNext) { res = it.next.containedIn1(m) }
      res

      case _          => false;
    }
  }

  def elements = new Iterator[MetaData] {
    var x = MetaData.this;
    def hasNext = x.hasNext;
    def next = {
      x = x.next;
      x
    }
  }

  /** shallow equals method */
  def equals1(that:MetaData): Boolean;

  /** returns key of this MetaData item */
  def key: String;

  /** returns key of this MetaData item */
  def value: String;

  /** returns Null or the next MetaData item */
  def next: MetaData;

  /** gets value of unqualified (unprefixed) attribute with given key */
  def getValue(key: String): String;

  /** gets value of qualified (prefixed) attribute with given key */
  def getValue(namespace: String, owner: Node, key: String): String =
    getValue(namespace, owner.scope, key);

  /** gets value of qualified (prefixed) attribute with given key */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): String;
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

/** prefixed attributes always have a non-null namespace
 */
class PrefixedAttribute(val pre: String, val key: String, val value: String, val next: MetaData) extends MetaData {

  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) =
    new PrefixedAttribute(pre, key, value, next);

  //** duplicates the MetaData (deep copy), not preserving order */
  //def deepCopy: MetaData = deepCopy(null);

  //** duplicates the MetaData (deep copy), prepending it to tail */
  /*
  def deepCopy(tail:MetaData): MetaData = {
    val md = copy(tail);
    if(null == next)
      md
    else
      next.deepCopy(md)
  }
  */

  def equals1(m:MetaData) = m.isPrefixed && (m.asInstanceOf[PrefixedAttribute].pre == pre) && (m.key == key) && (m.value == value);

  def getNamespace(owner: Node) =
    owner.getNamespace(pre);

  /** forwards the call to next */
  def getValue(key: String): String = next.getValue(key);

  /** gets attribute value of qualified (prefixed) attribute with given key
   */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): String = {
    if(key == this.key && scope.getURI(pre) == namespace)
      value
    else
      next.getValue(namespace, scope, key);
  }

  /** returns true */
  final def isPrefixed = true;

  override def hashCode() =
    pre.hashCode() * 41 + key.hashCode() * 7 + value.hashCode() * 3 + next.hashCode();


  def toString1(sb:StringBuffer): Unit = {
    sb.append(pre);
    sb.append(':');
    sb.append(key);
    sb.append('=');
    Utility.appendQuoted(value, sb);
  }

  def wellformed(scope: NamespaceBinding): Boolean = {
    (null == next.getValue(scope.getURI(pre), scope, key)
     && next.wellformed(scope));
  }

}

/** unprefixed attributes have the null namespace
 */
class UnprefixedAttribute(val key: String, val value: String, val next: MetaData) extends MetaData {

  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) =
    new UnprefixedAttribute(key, value, next);

  def equals1(m:MetaData) = !m.isPrefixed && (m.key == key) && (m.value == value);

  /** returns null */
  final def getNamespace(owner: Node): String =
    null;

  /** gets value of unqualified (unprefixed) attribute with given key */
  def getValue(key: String): String =
    if(key == this.key)
      value
    else
      next.getValue(key);

  /** forwards the call to next */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): String =
    next.getValue(namespace, scope, key);

  override def hashCode() =
    key.hashCode() * 7 + value.hashCode() * 53 + next.hashCode();

  /** returns false */
  final def isPrefixed = false;

  def toString1(sb:StringBuffer): Unit = {
    sb.append(key);
    sb.append('=');
    Utility.appendQuoted(value, sb);
  }

  def wellformed(scope: NamespaceBinding): Boolean =
    (null == next.getValue(null, scope, key)) && next.wellformed(scope);

}

case object Null extends MetaData  {

  /** appends given MetaData items to this MetaData list */
  override def append(m: MetaData): MetaData = m;

  override def containedIn1(m:MetaData): Boolean = false;

  /** returns a copy of this MetaData item with next field set to argument */
  def copy(next: MetaData) = next;

  /** returns null */
  def getNamespace(owner: Node) = null;

  final override def hasNext = false;

  final override def length = 0;

  final override def length(i:Int) = i;

  def isPrefixed = false;

  /** deep equals method */
  override def equals(that: Any) = that match {
    case m:MetaData => m.length == 0
    case _ => false;
  }

  def equals1(that:MetaData) = that.length == 0;

  def key = null;

  def value = null;

  def next = null;

  /** null */
  def getValue(key: String) = null;

  /** gets value of qualified (prefixed) attribute with given key */
  def getValue(namespace: String, scope: NamespaceBinding, key: String) =
    null;

  override def hashCode(): Int = 0;

  override def toString1(): String = "";

  //appends string representations of single attribute to StringBuffer
  def toString1(sb:StringBuffer) = {};

  override def toString(): String = "";

  override def toString(sb: StringBuffer): Unit = {}

  override def wellformed(scope: NamespaceBinding) = true;


}
