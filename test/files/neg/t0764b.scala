// see neg/t0764 why this should probably be a pos/ test -- alas something's wrong with existential subtyping (?)

// In all cases when calling "prepend" the receiver 'v'
// has static type NodeAlias[A] or (equivalently) Node { type T = A }.
// Since prepend explicitly returns the singleton type of the receiver,
// the return type of prepend in all cases is "v.type", and so the call
// to "new Main" can be parameterized with any of the following, in order
// of decreasing specificity with a tie for second place:
//
//   new Main[v.type](v.prepend)
//   new Main[NodeAlias[A]](v.prepend)
//   new Main[Node { type T = A }](v.prepend)
//   new Main(v.prepend)

// the `fail` comments below denote what didn't compile before SI-8177 fixed all of them

package p1 {
  object t0764 {
    type NodeAlias[A] = Node { type T = A }
    trait Node { outer =>
      type T <: Node
      def prepend: Node { type T = outer.type } = ???
    }

    class Main1[A <: Node](v: NodeAlias[A]) {
      private[this] def f1 = new Main1(v.prepend)                        // fail
      private[this] def f2 = new Main1[NodeAlias[A]](v.prepend)          // fail
      private[this] def f3 = new Main1[Node { type T = A }](v.prepend)   // fail
      private[this] def f4 = new Main1[v.type](v.prepend)                // ok
    }

    class Main2[A <: Node](v: Node { type T = A }) {
      private[this] def f1 = new Main2(v.prepend)                        // fail
      private[this] def f2 = new Main2[NodeAlias[A]](v.prepend)          // fail
      private[this] def f3 = new Main2[Node { type T = A }](v.prepend)   // fail
      private[this] def f4 = new Main2[v.type](v.prepend)                // ok
    }
  }
}

package p2 {
  object t0764 {
    type NodeAlias[A] = Node { type T = A }
    trait Node { outer =>
      type T <: Node
      def prepend: NodeAlias[outer.type] = ???
    }

    class Main1[A <: Node](v: NodeAlias[A]) {
      private[this] def f1 = new Main1(v.prepend)                        // ok!  <<========== WOT
      private[this] def f2 = new Main1[NodeAlias[A]](v.prepend)          // fail
      private[this] def f3 = new Main1[Node { type T = A }](v.prepend)   // fail
      private[this] def f4 = new Main1[v.type](v.prepend)                // ok
    }

    class Main2[A <: Node](v: Node { type T = A }) {
      private[this] def f1 = new Main2(v.prepend)                        // fail
      private[this] def f2 = new Main2[NodeAlias[A]](v.prepend)          // fail
      private[this] def f3 = new Main2[Node { type T = A }](v.prepend)   // fail
      private[this] def f4 = new Main2[v.type](v.prepend)                // ok
    }
  }
}
