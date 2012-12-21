object Test extends App {
  import Macros._
  // tree { (x:((Int,Int,Int),(Int,Int,Int))) => { val y=x; val ((r1,m1,c1),(r2,m2,c2))=y; (r1, m1 + m2 + r1 * c1 * c2, c2) } }
  tree { (x:((Int,Int,Int),(Int,Int,Int))) => { val ((r1,m1,c1),(r2,m2,c2))=x; (r1, m1 + m2 + r1 * c1 * c2, c2) } }
}