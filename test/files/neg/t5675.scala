//> using options -Xfatal-warnings
//
// without -feature, don't double-count the warning
//
class OneWarningOnly {
  implicit def `this is why we warn`(x: Any): Int = x.toString.toInt
}
