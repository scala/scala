package scala.tools.dtd2scala.regexp ;

abstract class RegExp ;

//case class Node(name:String) extends RegExp;
//case object PCDATA_ extends RegExp ;

//case object Eps extends RegExp;
//case class Star(r:RegExp) extends RegExp;
//case class Seq(rs:RegExp*) extends RegExp;
//case class Alt(rs:RegExp*) extends RegExp;

object RegExp {
        def parse(s:String):RegExp = Parser.parse( s );
}
