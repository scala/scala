//> using options -Werror -Wunused:privates -Xsource:3 -Xsource-features:case-companion-function

object Test {
  private case class Unused(i: Int = 42)
}
