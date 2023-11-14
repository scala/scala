object Outer {
  class Inner { type Xyz }

  type TypeInner = Inner { type Xyz = Int }
}
