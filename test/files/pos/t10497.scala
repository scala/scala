import scala.annotation._

// also works with subsets of {@annotation.meta.field @annotation.meta.getter @annotation.meta.setter}
class baz(out: Foo => Int) extends StaticAnnotation

class Foo {
  @baz(out = _.value) val value: Int = 5
}
