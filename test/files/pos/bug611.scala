package bug.contrib_60;

abstract class Field {
  type FieldType;

  var internalValue: FieldType;
}

case class IntField(value: int) extends Field {
  type FieldType = int;

  var internalValue: FieldType = value;
}

case class StringField(value: String) extends Field {
  type FieldType = String;

  var internalValue: FieldType = value;
}

object Test {
  def main (ars:scala.Array[String]): unit = {
    Console.println(List(new StringField ("bar"), new IntField(8)))
  }
}
