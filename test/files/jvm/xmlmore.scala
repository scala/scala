object Test extends Application {
  val com = <!-- thissa comment -->
  val pi  = <?this is a pi foo bar = && {{ ?>
  val crz = <![CDATA[
 "Come, come again, whoever you are, come!
Heathen, fire worshipper or idolatrous, come!
Come even if you broke your penitence a hundred times,
Ours is the portal of hope, come as you are."
                              Mevlana Celaleddin Rumi

]]>

  Console println com
  Console println pi
  Console println crz // this guy will escaped, and rightly so
  Console println "End Test"
}
