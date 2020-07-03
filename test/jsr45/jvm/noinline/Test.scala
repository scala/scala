import scala.tools.partest.JSR45Test

// Test JSR45 info for a simple program, without inlining.
object Test extends JSR45Test {
  override val definedClasses = Seq("Main_1")
}
