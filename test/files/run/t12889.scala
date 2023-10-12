//> using options -Xscript Test

class Test(private val c: Int)

object Test

println(s"Args ${args.mkString(", ")}")
println(new Test(42).c)
