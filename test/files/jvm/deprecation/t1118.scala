import scala.xml._

object Test {
  def main(args: Array[String]) {
    println(<xml:group>
<hi/> <!-- literal short -->
<there></there> <!-- literal long -->
<guys who="you all"></guys> <!-- literal long with attribute-->
<hows it="going"/> <!-- literal short with attribute -->
<this>scala stuff is pretty cool</this> <!-- literal not empty -->
</xml:group>)

    println(Elem(null, "bob", Null, TopScope, false) ++ Text(" ") ++ Comment("programmatic long"))

    println(Elem(null, "dobbs", Null, TopScope, true) ++ Text(" ") ++ Comment("programmatic short"))

    println(Elem(null, "is", Attribute("really", Text("yep"), Null), TopScope, true) ++ Text(" ") ++ Comment ("programmatic short with attribute"))

    println(Elem(null, "slack", Attribute("sing", Text("it"), Null), TopScope, false) ++ Text(" ") ++ Comment ("programmatic long with attribute"))
  }
}