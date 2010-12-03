trait Document {
  sealed trait FieldBase
  trait StaticFieldBase extends FieldBase with StaticDocument
  trait DynamicFieldBase extends FieldBase with DynamicDocument
}

sealed trait StaticDocument extends Document {
  abstract class AbstractField extends FieldBase
}

sealed trait DynamicDocument extends Document {
  abstract class AbstractField extends FieldBase
}

class Coll extends StaticDocument

// similiar issue with annotations
class ann[T] extends StaticAnnotation

trait StatDoc extends Doc {
  @ann[StatFB]
  def foo: Int
}

trait Doc {
  @ann[DynDoc#ForceDynDoc]
  def bar: Int
  trait StatFB
  trait DynFB
}

trait DynDoc extends Doc {
  @ann[DynFB]
  def baz: Int
  trait ForceDynDoc
}
