class M(val t: Int) extends AnyVal {
   def lazyString = {
      object X
      class Y

      () => {X; new Y}
   }
}
