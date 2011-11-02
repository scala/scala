object myBreak extends scala.xml.Unparsed("<br />")

object Test extends App {
  val com = <!-- thissa comment -->
  val pi  = <?this is a pi foo bar = && {{ ?>
  val crz = <![CDATA[
 "Come, come again, whoever you are, come!
Heathen, fire worshipper or idolatrous, come!
Come even if you broke your penitence a hundred times,
Ours is the portal of hope, come as you are."
                              Mevlana Celaleddin Rumi]]>

  val nazim = <foo>{myBreak}</foo> // shows use of unparsed
                                          
  Console println com
  Console println pi
  Console println crz // this guy will escaped, and rightly so
  Console println nazim
  Console println "End Test"

  <x:foo xmlns:x="gaga"/> match {
    case scala.xml.QNode("gaga","foo",md,child@_*) =>
  }

  <x:foo xmlns:x="gaga"/> match {
    case scala.xml.Node("foo",md,child@_*) =>
  }

}
