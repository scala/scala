/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;


case object Null extends MetaData {

  /** appends given MetaData items to this MetaData list */
  override def append(m: MetaData): MetaData = m;

  override def containedIn1(m: MetaData): Boolean = false;

  /** returns its argument */
  def copy(next: MetaData) = next;


  override def elements = Iterator.empty[MetaData];

  override def filter(f: MetaData => Boolean): MetaData = this;

  /** returns null */
  def getNamespace(owner: Node) = null;

  final override def hasNext = false;

  final override def length = 0;

  final override def length(i: Int) = i;

  def isPrefixed = false;

  /** deep equals method */
  override def equals(that: Any) = that match {
    case m: MetaData => m.length == 0
    case _ => false;
  }

  def equals1(that:MetaData) = that.length == 0;

  def key = null;

  def value = null;

  override def map(f: MetaData => Text): List[Text] = Nil;

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
