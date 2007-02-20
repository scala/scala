/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import compat.StringBuilder

/** <p>
 *    Attribute information item, and linked list of attribute information items.
 *    These are triples consisting of <code>prefix,key,value</code>. To obtain
 *    the namespace, <code>getNamespace</code> must be called with the parent.
 *    If next is null, this is the last attribute in the MetaData list.
 * </p>
 * <p>
 *   Either an UnprefixedAttribute or a PrefixedAttribute
 * </p>
 *
 * @todo _vlue should be a normalized attribute value
 */
@serializable
abstract class MetaData extends Iterable[MetaData] {

  /** appends given MetaData items to this MetaData list.
   *
   *  @param m ...
   *  @return  ...
   */
  def append(m: MetaData): MetaData =
    next.append(copy(m))

  /**
   * Gets value of unqualified (unprefixed) attribute with given key, null if not found
   *
   * @param  key
   * @return value as Seq[Node] if key is found, null otherwise
   */
  def apply(key: String): Seq[Node]

  /** convenience method, same as <code>apply(namespace, owner.scope, key)</code>.
   *
   *  @param namespace ...
   *  @param owner     ...
   *  @param key       ...
   *  @return          ...
   */
  final def apply(namespace: String, owner: Node, key: String): Seq[Node] =
    apply(namespace, owner.scope, key)

  /**
   * Gets value of prefixed attribute with given key and namespace, null if not found
   *
   * @param  uri namespace of key
   * @param  scp a namespace scp (usually of the element owning this attribute list)
   * @param  key to be looked fore
   * @return value as Seq[Node] if key is found, null otherwise
   */
  def apply(uri:String, scp:NamespaceBinding, k:String): Seq[Node]

  /**
   *  @param m ...
   *  @return  <code>true</code> iff ...
   */
  def containedIn1(m: MetaData): Boolean =
    m.equals1(this) || containedIn1(m.next)

  /** returns a copy of this MetaData item with next field set to argument.
   *
   *  @param next ...
   *  @return     ...
   */
  def copy(next: MetaData): MetaData

  /** if owner is the element of this metadata item, returns namespace */
  def getNamespace(owner: Node): String

  def hasNext = (Null != next)

  def length: Int = length(0)

  def length(i: Int): Int = next.length(i + 1)

  def isPrefixed: Boolean

  /** deep equals method */
  override def equals(that: Any) =
    that match {
      case m: MetaData =>
        var res = (this.length == m.length) && (this.hashCode() == m.hashCode())
        val it = this.elements
        while (res && it.hasNext) { res = it.next.containedIn1(m) }
        res
      case _ =>
        false
    }

  /** returns an iterator on attributes */
  def elements: Iterator[MetaData] = new Iterator[MetaData] {
    var x: MetaData = MetaData.this
    def hasNext = Null != x
    def next = {
      val y = x
      x = x.next
      y
    }
  }

  /** shallow equals method */
  def equals1(that: MetaData): Boolean

  /** filters this sequence of meta data */
  override def filter(f: MetaData => Boolean): MetaData = {
    if (f(this)) copy(next filter f) else next filter f
  }

  /** returns key of this MetaData item */
  def key: String

  /** returns value of this MetaData item */
  def value: Seq[Node]

  /** maps this sequence of meta data */
  def map(f: MetaData => Text): List[Text] = f(this)::(next map f)

  /** returns Null or the next MetaData item */
  def next: MetaData

  /**
   * Gets value of unqualified (unprefixed) attribute with given key, None if not found
   *
   * @param  key
   * @return value in Some(Seq[Node]) if key is found, None otherwise
   */
  final def get(key: String): Option[Seq[Node]] = apply(key) match {
    case null => None
    case x    => Some(x)
  }

  /** same as get(uri, owner.scope, key) */
  final def get(uri: String, owner: Node, key: String): Option[Seq[Node]] =
    get(uri, owner.scope, key)


  /** gets value of qualified (prefixed) attribute with given key.
   *
   * @param  uri namespace of key
   * @param  scope a namespace scp (usually of the element owning this attribute list)
   * @param  key to be looked fore
   * @return value as Some[Seq[Node]] if key is found, None otherwise
   */
  final def get(uri: String, scope: NamespaceBinding, key: String): Option[Seq[Node]] =
    apply(uri, scope, key) match {
      case null => None
      case x    => Some(x)
    }

  override def hashCode(): Int

  def toString1(): String = {
    val sb = new StringBuilder()
    toString1(sb)
    sb.toString()
  }

  //appends string representations of single attribute to StringBuilder
  def toString1(sb:StringBuilder): Unit

  override def toString(): String = {
    val sb = new StringBuilder()
    toString(sb)
    sb.toString()
  }

  def toString(sb: StringBuilder): StringBuilder = {
    sb.append(' ')
    toString1(sb)
    next.toString(sb)
  }

  /**
   *  @param scope ...
   *  @return      <code>true</code> iff ...
   */
  def wellformed(scope: NamespaceBinding): Boolean

  /**
   *  @param key ...
   *  @return    ...
   */
  def remove(key:String): MetaData

  /**
   *  @param namespace ...
   *  @param scope     ...
   *  @param key       ...
   *  @return          ...
   */
  def remove(namespace: String, scope: NamespaceBinding, key: String): MetaData

  /**
   *  @param namespace ...
   *  @param owner     ...
   *  @param key       ...
   *  @return          ...
   */
  final def remove(namespace: String, owner: Node, key: String): MetaData =
    remove(namespace, owner.scope, key)

}
