object O {
  type A = AnyRef {
      def X: AnyRef {
        def X: AnyRef {
          def X:AnyRef {
            def X: AnyRef {
              def X: AnyRef {
                def X: AnyRef {
                    def a: Int }}}}}}}

  type B = AnyRef {
        def X: AnyRef {
          def X:AnyRef {
            def X: AnyRef {
              def X: AnyRef {
                def X: AnyRef {
                  def X: AnyRef {
                    def b: Int }}}}}}}

  // glb of A and B
  type C = AnyRef {
        def X: AnyRef {
          def X:AnyRef {
            def X: AnyRef {
              def X: AnyRef {
                def X: AnyRef {
                  def X: AnyRef {
                    def a: Int; def b: Int }}}}}}}

  class U[-X]

  def f(a: U[A], b: U[B]) = Seq(a, b)
  def f_bound_R[X >: A](a: U[X], b: U[B]): Seq[U[C]] = Seq(a, b)
  def f_bound_L[X >: B](a: U[A], b: U[X]): Seq[U[C]] = Seq(a, b)
  def f_bound[X >: A, Y >: B](a: U[X], b: U[Y]): Seq[U[C]] = Seq(a, b)

}
