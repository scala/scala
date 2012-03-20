object Test extends App {
  val a = Map("a" -> Some(1), "b" -> None)
  println(a)

// inferred type should be Map[String, Int]
  val res = a collect {case (p, Some(a)) => (p, a)}

// variations: const target -> switch, non-const -> normal match, char target --> scrut needs toInt,
// eta-expanded --> work is done by typedFunction, non-eta-expanded --> typedMatch

  object nonConstCharEta {
    final val GT      : Char = 'a'
    final val GTGT    : Char = 'b'
    final val GTGTGT  : Char = 'c'
    final val GTEQ    : Char = 'd'
    final val GTGTEQ  : Char = 'e'
    final val GTGTGTEQ: Char = 'f'
    final val ASSIGN  : Char = 'g'

    def acceptClosingAngle(in: Char) {
      val closers: PartialFunction[Char, Char] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  object nonConstChar {
    final val GT      : Char = 'a'
    final val GTGT    : Char = 'b'
    final val GTGTGT  : Char = 'c'
    final val GTEQ    : Char = 'd'
    final val GTGTEQ  : Char = 'e'
    final val GTGTGTEQ: Char = 'f'
    final val ASSIGN  : Char = 'g'

    def acceptClosingAngle(in: Char) {
      val closers: PartialFunction[Char, Char] = x => x match {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  object constCharEta {
    final val GT      = 'a'
    final val GTGT    = 'b'
    final val GTGTGT  = 'c'
    final val GTEQ    = 'd'
    final val GTGTEQ  = 'e'
    final val GTGTGTEQ= 'f'
    final val ASSIGN  = 'g'

    def acceptClosingAngle(in: Char) {
      val closers: PartialFunction[Char, Char] = x => x match {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  object constChar {
    final val GT      = 'a'
    final val GTGT    = 'b'
    final val GTGTGT  = 'c'
    final val GTEQ    = 'd'
    final val GTGTEQ  = 'e'
    final val GTGTGTEQ= 'f'
    final val ASSIGN  = 'g'

    def acceptClosingAngle(in: Char) {
      val closers: PartialFunction[Char, Char] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  object constIntEta {
    final val GT       = 1
    final val GTGT     = 2
    final val GTGTGT   = 3
    final val GTEQ     = 4
    final val GTGTEQ   = 5
    final val GTGTGTEQ = 6
    final val ASSIGN   = 7

    def acceptClosingAngle(in: Int) {
      val closers: PartialFunction[Int, Int] = x => {println("hai!"); (x + 1)} match {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  object constInt {
    final val GT       = 1
    final val GTGT     = 2
    final val GTGTGT   = 3
    final val GTEQ     = 4
    final val GTGTEQ   = 5
    final val GTGTGTEQ = 6
    final val ASSIGN   = 7

    def acceptClosingAngle(in: Int) {
      val closers: PartialFunction[Int, Int] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => ASSIGN
      }
      if (closers isDefinedAt in) println(closers(in))
      else println("undefined")
    }

    def test() = {
      acceptClosingAngle(GTGT)
      acceptClosingAngle(ASSIGN)
    }
  }

  println(res) // prints "Map(a -> 1)"

  nonConstCharEta.test()
  nonConstChar.test()
  constCharEta.test()
  constChar.test()
  constIntEta.test()
  constInt.test()
}
