//> using options -Werror -Xlint -Xmaxwarns 0
//
// nowarn should mean no warnings are emitted,
// irrespective of other flags, and also no
// warnings should be summarized.
//
class C {
  def f = 1 → 2
  def g: Unit = 1
}
