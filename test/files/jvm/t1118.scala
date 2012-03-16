import scala.xml._

object Test {
  def main(args: Array[String]) {
    println(<xml:group>
<hi/> <!-- literal short -->
<there></there> <!-- literal long -->
<guys who="you all"></guys> <!-- literal long with attribute-->
<hows it="going"/> <!-- literal short with attribute -->
<this>is pretty cool</this> <!-- literal not empty -->
</xml:group>)

    println(Elem(null, "emptiness", Null, TopScope, false) ++ Text(" ") ++ Comment("programmatic long"))

    println(Elem(null, "vide", Null, TopScope, true) ++ Text(" ") ++ Comment("programmatic short"))

    println(Elem(null, "elem", Attribute("attr", Text("value"), Null), TopScope, true) ++ Text(" ") ++ Comment ("programmatic short with attribute"))

    println(Elem(null, "elem2", Attribute("attr2", Text("value2"), Null), TopScope, false) ++ Text(" ") ++ Comment ("programmatic long with attribute"))
  }
}