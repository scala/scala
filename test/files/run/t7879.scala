
import scala.tools.partest.SessionTest

object Test extends SessionTest {
  override def extraSettings = "-Yimports:java.lang,scala,scala.Predef,scala.util.chaining -Xsource:3 -Xsource-features:case-copy-by-name"
}
