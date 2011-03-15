/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc;


/** A SQL-99 value of any type. */
@deprecated(DbcIsDeprecated) abstract class Value {

  /** The SQL-99 type of the value. */
  val dataType: DataType;

  type NativeType = dataType.type#NativeType;

  val nativeValue: NativeType;

  /** A SQL-99 compliant string representation of the value. */
  def sqlString: String;

}
