package scala.xml;

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
