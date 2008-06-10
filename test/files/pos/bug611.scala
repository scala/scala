package bug.contrib_60;

abstract class Field {
  type FieldType;

  var internalValue: FieldType;
}

case class IntField(value: Int) extends Field {
  type FieldType = Int;

  var internalValue: FieldType = value;
}

case class StringField(value: String) extends Field {
  type FieldType = String;

  var internalValue: FieldType = value;
}

object Test {
  def main (args: scala.Array[String]) {
    Console.println(List(new StringField ("bar"), new IntField(8)))
  }
}
