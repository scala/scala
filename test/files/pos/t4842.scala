class Foo (x: AnyRef) {
  def this() = {
    this(new { } ) // okay
  }
}


class Blerg (x: AnyRef) {
   def this() = {
     this(new { class Bar { println(Bar.this); new { println(Bar.this) } }; new Bar } ) // okay
   }
}


class Outer {
	class Inner (x: AnyRef) {
	  def this() = {
	    this(new { class Bar { println(Bar.this); new { println(Bar.this) } }; new Bar } ) // okay
	  }

	  def this(x: Boolean) = {
	    this(new { println(Outer.this) } ) // okay
	  }
	}
}

