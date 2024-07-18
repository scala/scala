//> using options -Wconf:src=test/files/neg/wconfSource2.scala&cat=deprecation:e,src=test/.*&msg=statement:i,src=/neg/.*&cat=feature-reflective-calls:i

class C {
  @deprecated("", "") def dep = 0
  def t = dep
  def u = { 1; 2 }
  def v(a: { def f: Int }) = a.f
}
