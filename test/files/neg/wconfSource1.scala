//> using options -Wconf:src=.*Source.*&cat=deprecation:e,src=Source1.scala&msg=statement:e,src=wconfSource1&msg=statement:e,src=wconfSource1.scala&msg=statement:i

// src=Source1.scala doesn't match: the pattern needs to start at a path segment (after `/`)
// src=wconfSource1 doesn't match: the pattern needs to match to the end of the path (.scala)
// src=wconfSource1.scala matches

class C {
  @deprecated("", "") def dep = 0
  def t = dep
  def u = { 1; 2 }
}
