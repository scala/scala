object Tet {
  import scala.xml._;
  def fooz(x: Node=>String) = {}
    def foo( m:Node ):Unit = fooz {
      case Elem(_,_,_,_,n,_*) if (n == m) => "gaga"
    }
}
