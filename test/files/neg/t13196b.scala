//> using options -Werror -release 16
//> using jvm 17+

// ct.sym encodes the annotation differently

class C {
  def localDate(d: java.time.LocalDate) = d.synchronized {} // warn
}
