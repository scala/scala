//> using options -Werror -Xlint:deprecation
//
// an innocent gotcha
//
class C {
  var x = 0
  locally (
    x = 27
  )
}
