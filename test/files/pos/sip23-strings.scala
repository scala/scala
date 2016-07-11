object Test {
  val s0: """foo""" = """foo"""
  type broken =
"""
foo
bar
"""

  val s1: broken =
"""
foo
bar
"""

  type escaped = "\nBar\n"
  val s2: escaped = "\nBar\n"
}
