package imptwice

class foo(s: String);

object Util {
  def foo(s: String) = new foo(s)
}

import imptwice.Util._


object User {
  def main(args: Array[String]) = {
    foo("blah")
  }
}
