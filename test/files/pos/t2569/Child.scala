package varargs

  class Child extends Parent {

    override def concatenate(strings: String*): String =
      strings map("\"" + _ + "\"") mkString("(", ", ", ")")

  }

