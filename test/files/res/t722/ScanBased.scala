package t722;
trait ScanBased extends Parser {
 trait AdjacentLink extends Link {
    override def foo() = super.foo;
 }
 trait WhitespaceLink extends AdjacentLink {
   override def foo() = super.foo;
 }
}

