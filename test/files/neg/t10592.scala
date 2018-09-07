object O {
  type A = AnyRef {
      def X: AnyRef {
        def X: AnyRef {
          def X: AnyRef {
            def X:AnyRef {
              def X: AnyRef {
                def X: AnyRef {
                  def X: AnyRef {
                    def a: Int }}}}}}}}

  type B = AnyRef {
    def X: AnyRef {
      def X: AnyRef {
        def X:AnyRef {
          def X: AnyRef {
            def X: AnyRef {
              def X: AnyRef {
                def X: AnyRef {
                  def b: Int }}}}}}}}

  class U[-X]

  def f(a: U[A], b: U[B]) = Seq(a, b)

}
