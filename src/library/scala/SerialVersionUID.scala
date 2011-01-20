/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/



package scala


/**
 * Annotation for specifying the <code>static SerialVersionUID</code> field
 * of a serializable class.
 */
class SerialVersionUID(uid: Long) extends annotation.StaticAnnotation
