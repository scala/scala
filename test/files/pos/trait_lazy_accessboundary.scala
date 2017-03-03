package foo { trait HasLazy { private[foo] lazy val myLazy = "my lady" } }
package bar { class MixInSuperLazy extends foo.HasLazy }
