package scala.tools.servlet.engine.config ;

import scala.collection.mutable;

import scala.xml.{ Attribute, IntAttribute, AttributeSeq, Elem };
import scala.xml.parsing._;

class ConfigHandler extends MarkupHandler[Config] {

  final val config_namespace = "http://scala.epfl.ch/scala.tools.servlet.engine/config";

  override def attribute(pos: int, uri:String, key: String, value:String): Attribute =
    if( key == "port" )
      new IntAttribute(uri, key, Integer.parseInt(value));
    else
      super.attribute(pos, uri, key, value);

  var hmap = new mutable.HashMap[String,String];

  /** be careful to copy everything from attrMap1, as it will change
   *  @param attrMap1 the attribute map.
   */
  def element(pos: int, uri: String, label: String, attrMap1: mutable.Map[Pair[String,String],Attribute], args: mutable.Buffer[Config]): Option[Config] = {
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

        case "connector" if attrMap1(Pair(config_namespace,"protocol")).value == "http" =>
          val res = HttpConnectorConfig( attrMap1(Pair(config_namespace,"port")).intValue, hmap );
          hmap = new mutable.HashMap[String,String];
          Some(res)

        case "map"       =>
          hmap.update(attrMap1(Pair(config_namespace,"url")).value, attrMap1(Pair(config_namespace,"to")).value);
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
