trait S {
  trait T {
    this: Any =>

    trait U {
      trait V {
        S.this
      }
    }
  }
}
