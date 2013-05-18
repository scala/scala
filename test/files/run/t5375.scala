object Test extends App {
  val foos = (1 to 1000).toSeq
  try
    foos.par.map(i => if (i % 37 == 0) sys.error("i div 37") else i)
  catch {
    case ex: RuntimeException => println("Runtime exception")
  }
}
