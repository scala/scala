/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package result


import scala.dbc.datatype._
import scala.dbc.value._

/** An ISO-9075:2003 (SQL) table field. */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class Field {

  /** The content (value) of the field. The type of this value is undefined,
   *  transformation into a useful type will be done by an automatic view
   *  function defined in the field object.
   */
  def content: Value

  final def value[Type <: Value]: Type =
    content.asInstanceOf[Type]

  final def exactNumericValue[NativeType] =
    content.asInstanceOf[dbc.value.ExactNumeric[NativeType]]

  final def approximateNumericValue[NativeType] =
    content.asInstanceOf[dbc.value.ApproximateNumeric[NativeType]]

  final def booleanValue =
    content.asInstanceOf[dbc.value.Boolean]

  final def characterValue =
    content.asInstanceOf[dbc.value.Character]

  final def characterLargeObjectValue =
    content.asInstanceOf[dbc.value.CharacterLargeObject]

  final def characterVaryingValue =
    content.asInstanceOf[dbc.value.CharacterVarying]

  final def unknownValue =
    content.asInstanceOf[dbc.value.Unknown]

  /** The tuple that contains this field. */
  def originatingTuple: Tuple

  /** The field metadata attached to this field. */
  def metadata: FieldMetadata

}

@deprecated(DbcIsDeprecated, "2.9.0") object Field {

  implicit def fieldToValue (field: Field): Value = field.content

}
