
import tools.partest.DirectTest

// verify that only languageFeature names are accepted by -language
object Test extends DirectTest {
  override def code = "class Code"

  override def show() = {
    //compile("-language", "--")  // no error
    compile(s"-language:noob")
  }
}

