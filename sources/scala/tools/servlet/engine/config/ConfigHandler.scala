package scala.tools.servlet.engine.config ;

import scala.collection.mutable;

import scala.xml.{ Attribute, AttributeSeq, Elem };
import scala.xml.parsing._;

class ConfigHandler extends MarkupHandler[Config] {

  final val config_namespace = "http://scala.epfl.ch/scala.tools.servlet.engine/config";

  def attributeCDataValue(pos: int, str:String) = CDataValue(str);

  def attributeIntValue(pos: int, value:Int) = IntValue(value);

  def attributeNamespaceDecl(pos: int, uri: String) = NamespaceDecl(uri);

  override def attribute(pos: int, key: String, value:String): AttribValue =
    if( key == "port" )
      attributeIntValue(pos, Integer.parseInt(value));
    else
      super.attribute(pos,key,value);

  var hmap = new mutable.HashMap[String,String];

  /** be careful to copy everything from attrMap1, as it will change
   *  @param attrMap1 the attribute map.
   */
  def element(pos: int, uri: String, label: String, attrMap1: mutable.Map[String,AttribValue], args: mutable.Buffer[Config]): Option[Config] = {
    if( uri == config_namespace ) {
      label match {

        case "engine"    =>
          var list:List[ConnectorConfig] = Nil;
          for( val c <- args ) c match {
            case x:ConnectorConfig =>
              list = x::list;
            case _ =>
          }
          Some(EngineConfig( list ));

        case "connector" if attrMap1("protocol").asString == "http" =>
          val res = HttpConnectorConfig( attrMap1("port").asInt, hmap );
          hmap = new mutable.HashMap[String,String];
          Some(res)

        case "map"       =>
          hmap.update(attrMap1("url").asString, attrMap1("to").asString);
          None
      }
    } else None

  }

  def charData(pos: Int, txt: String ) = None;
  def procInstr(pos: Int, target: String, txt: String) = None;
  def comment(pos: Int, comment: String ) = None;
  def entityRef(pos: Int, n: String) = None;

  def text(pos: Int, n: String) = None;

}
