package scala.xml.parsing ;

/** a container for attribute values */
trait AttribValue;

case class NamespaceDecl(uri: String) extends AttribValue;
case class CDataValue(value: String) extends AttribValue;
case class CustomValue[A](value:A) extends AttribValue;
