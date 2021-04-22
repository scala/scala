import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
raw"\""
raw"\" // this used to be a comment, but after scala/pull#8830 it's part of the string! "
raw"\" // this used to compile, now it's unclosed
"""
}
