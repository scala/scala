package scala.tools.servlet.engine;

trait Mapping {

  def switch(uri: String): String;

}
