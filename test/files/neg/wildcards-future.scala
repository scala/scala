//> using options -Xsource:3
//
object Test {
  val underscores: Map[_ <: AnyRef, _ >: Null] = Map()
  val qmarks: Map[? <: AnyRef, ? >: Null] = Map()

  underscores : Map[String, String] // error wildcard variables starting with `_`

  qmarks : Map[String, String] // error – wildcard variables should start with `?` to differentiate from the old syntax
                               // (and have a mildly more readable error...)
}
