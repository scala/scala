import collection._

object Proxies {
  class C1 extends MapProxy[Int,Int] { def self = Map[Int,Int]() }
  class C2 extends mutable.MapProxy[Int,Int] { def self = mutable.Map[Int,Int]() }
  class C3 extends immutable.MapProxy[Int,Int] { def self = immutable.Map[Int,Int]() }
  
  class C4 extends SetProxy[Int] { def self = Set[Int]() }
  class C5 extends mutable.SetProxy[Int] { def self = mutable.Set[Int]() }
  class C6 extends immutable.SetProxy[Int] { def self = immutable.Set[Int]() }
  
  class C7 extends SeqProxy[Int] { def self = Seq[Int]() }
}