/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Attribute.scala 8926 2006-10-11 09:58:51 +0000 (Wed, 11 Oct 2006) dragos $


package scala

/** <p>A base class for classfile attributes. These are stored as
 *     Java annotations in classfiles.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
trait ClassfileAttribute extends Attribute {}
