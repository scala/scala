object Test {
  object A {
    var x: Int = 0
    var ~~ : Int = 0
  }

  import A.{x => y, ~~ => !!}

  y = 1
  !! = 2
  y += 3
  !! -= 4
}

