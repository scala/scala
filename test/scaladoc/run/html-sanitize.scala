import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def scaladocSettings = ""
  override def code = """
  object Test {
    /** <img src='http://example.com/a.png' alt="kept">
      * <a href="../b.html?x=1&y=2#frag">relative</a>
      * <a href="https://example.com/?x=1&y=2">absolute</a>
      * <br/>
      * <b><i>ab</i> bab</b>
      */
    def goodTags = 1

    /** <img src=x onerror="alert(1)">
      * <img src="x"/ONLOAD=alert(1) title=t>
      * <img src=x/onerror=alert(1)>
      * <a href="javascript:alert(1)">a</a>
      * <a href="jav&#x61;script:alert(1)">b</a>
      * <a href=" JavaScript:alert(1)">c</a>
      * <img src="data:text/html,x">
      * <form action="https://example.com"><input name=q></form>
      * <img src="x title=t>
      * <b><img src=x onerror="alert(1)"></b>
      * <i><script>alert(1)</script></i>
      * MARK<img src=x onerror="alert(1)">MARK
      * {@code <img src=x onerror="alert(1)">}
      */
    def badTags = 1

    /** A tag split by a section:
      * <b title="
      * @param x "><img src=x onerror="alert(1)">
      */
    def badSplitTag(x: Int) = 1

    /** A tag split by the end of a code block:
      * {{{
      * <b title="}}}"><img src=x onerror="alert(1)">
      */
    def badCodeSplitTag = 1

    /** [[https://example.com ok]] */
    def goodLinks = 1

    /** [[javascript:alert(1) js]] [[data:text/html,x]] */
    def badLinks = 1
  }
  """.replace("MARK", "\u000E") // the marker Scaladoc uses internally for safe tags

  def testModel(rootPackage: Package): Unit = {
    import access._
    val t = rootPackage._object("Test")
    for (m <- List("goodTags", "badTags", "badSplitTag", "badCodeSplitTag", "goodLinks", "badLinks")) {
      println(s"$m:")
      val c = t._method(m).comment.get
      println(c.body)
      c.valueParams.values.foreach(println)
    }
  }
}
