object Test {
  object A {
    val x: Int = 0
    def ~~ : Int = 0
  }

  import A.{x => y, ~~ => !!}

  y = 1
  !! = 2
  y += 3
  !! -= 4
}
