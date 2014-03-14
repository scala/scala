trait Bug {
  val u: { type Amb } = ???
  import u._
 
  class Amb { def x = 0 }
  class C(x: Amb) { // after dbd8457e4, "reference to Amb is ambiguous"
    x.x
  }
}
