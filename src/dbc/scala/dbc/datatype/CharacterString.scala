/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


/** A type category for all SQL types that store strings of characters. */
@deprecated(DbcIsDeprecated) abstract class CharacterString extends String {

  type NativeType = java.lang.String;
  val nativeTypeId = DataType.STRING;

  /** The name of the character set in which the string is encoded. */
  def encoding: Option[java.lang.String] = None;

}
