//> using options -Yimports:java.lang,scala,scala.Predef,scala.util.chaining

class C {
  def f = 42.tap(println)
}

// was: error: bad preamble import scala.util.chaining
