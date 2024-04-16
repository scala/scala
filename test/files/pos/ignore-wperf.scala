//> using options -Werror -Wperformance -Wconf:cat=lint-performance:s

class C {
  var x = 0

  def f: (Int => Int) = {
    var y = x
    z => y + 1
  }
}
//test/files/neg/ignore-wperf.scala:7: warning: Modification of variable y within a closure causes it to be boxed.
