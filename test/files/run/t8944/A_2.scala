case class Other(private val x: String) // consume a fresh name suffix
 
// the param accessor will now be called "x$2",
// whereas the previously compiled client expects it to be called
// x$1
case class A(private val x: String)
