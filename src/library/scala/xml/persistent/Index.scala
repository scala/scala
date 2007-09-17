package scala.xml.persistent

/** an Index returns some unique key that is part of a node
 */
abstract class Index[A] extends Function1[Node,A] {}
