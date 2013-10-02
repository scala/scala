class ForSomeVsUnapply {
  def test {
    def makeWrap: Wrap = ???
    def useRep[e](rep: (e, X[e])) = ()

    val repUnapply = Wrap.unapply(makeWrap).get
    useRep(repUnapply)  // okay

    val Wrap(rep0) = makeWrap
    useRep(rep0) // error

    val rep = makeWrap match {
      case Wrap(r) => r
    };

    useRep(rep) // error
  }
}

class X[e]

case class Wrap(rep: (e, X[e]) forSome { type e })
