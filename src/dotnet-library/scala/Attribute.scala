/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>A base class for attributes. Atributes extending this class directly
 *     are not preserved for the Scala type checker and are also not stored as
 *     Java annotations in classfiles. To enable either or both of these, one needs to
 *     inherit from <code>StaticAttribute</code> or/and <code>ClassfileAttribute</code>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
abstract class Attribute {}
