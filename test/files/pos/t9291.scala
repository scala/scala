// more than one field is required to trigger crash
// there must be a default value for one of the parameters
case class OuterObject(field: Int = 1, anotherField: Int = 2)

object Test {
  OuterObject().copy(field = OuterObject().field)

  // declaring something without explicit type, with the same name as OuterObject.field
  def field = "anything"
}
