package scala.xml.parsing ;

/** a container for attribute values */
trait AttribValue {
  def asString: String = this match {
    case CDataValue(value) => value;
  }
  def asInt: Int = this match {
    case IntValue(value) => value;
  }
};

case class NamespaceDecl(uri: String) extends AttribValue;
case class CDataValue(value: String) extends AttribValue;
case class CustomValue[A](value:A) extends AttribValue;

case class IntValue(value: Int) extends AttribValue;
