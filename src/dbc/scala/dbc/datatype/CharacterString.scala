/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:CharacterString.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package datatype;


/** A type category for all SQL types that store strings of characters. */
abstract class CharacterString extends String {

  type NativeType = java.lang.String;
  val nativeTypeId = DataType.STRING;

  /** The name of the character set in which the string is encoded. */
  def encoding: Option[java.lang.String] = None;

}
