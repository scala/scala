object Test {
  def main(args: Array[String]) {
    assert( <k a="1" b="2"/> == <k a="1" b="2"/> )
    assert( <k a="1" b="2"/> != <k a="1" b="3"/> )
    assert( <k a="1" b="2"/> != <k a="2" b="2"/> )

    assert( <k a="1" b="2"/> != <k/> )
    assert( <k a="1" b="2"/> != <k a="1"/> )
    assert( <k a="1" b="2"/> != <k b="2"/> )
  }
}
