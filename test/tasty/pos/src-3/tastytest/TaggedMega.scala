package tastytest

import TaggedMega.Nested.Nested2.Tags

object TaggedMega {

  sealed trait ForceChildren

  @Tags("xyz,foo")
  class Tagged

  object Nested {

    def foo = 23

    object Nested2 {

      def bar = 47

      def xyz = "xyz"

      class Tags(tags: String) extends scala.annotation.Annotation with ForceChildren

    }

  }

}
