import scala.tools.partest.JSR45Test

// Test JSR45 info when inlining from an external file.
object Test extends JSR45Test {
  override val definedClasses = Seq("Main_1", "Utils_1$")
}
