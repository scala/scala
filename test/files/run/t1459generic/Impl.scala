class Impl extends VarargGeneric[String] {
  def genericOne(x: String, arg: String): String = x + arg
  def genericVar(x: String, args: String*): String = x + args.head
}
