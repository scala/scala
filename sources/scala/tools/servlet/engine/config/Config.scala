package scala.tools.servlet.engine.config ;

abstract class Config;

case class EngineConfig(xs:List[ConnectorConfig]) extends Config;

abstract class ConnectorConfig extends Config;

case class HttpConnectorConfig(port:Int, mapping: PartialFunction[String,String]) extends ConnectorConfig;

