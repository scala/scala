trait Frog[T] {
      def hello: T
      def size: Int
    }

    trait OnlyWithFrogs {
      self: Frog[_] =>

        def sizeStr = size.toString
    }
