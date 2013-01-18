// not a peep out of the pattern matcher's unreachability analysis
// its budget should suffice for these simple matches (they do have a large search space)
class Test {
  import foo.Bar // a large enum
  def exhaustUnreachabilitysStack_ENUM_STYLE = (null: Bar) match {
    case Bar.BULGARIA =>
    case _            =>
  }

  // lots of strings
  def exhaustUnreachabilitysStack_StringStyle = "foo" match {
    case "a"          =>
    case "b"          =>
    case "c"          =>
    case "d"          =>
    case "e"          =>
    case "f"          =>
    case "aa"         =>
    case "ba"         =>
    case "ca"         =>
    case "da"         =>
    case "ea"         =>
    case "f1a"        =>
    case "a1a"        =>
    case "b1a"        =>
    case "c1a"        =>
    case "d1a"        =>
    case "e1a"        =>
    case "f1a2"       =>
    case "f1a0"       =>
    case "a1a2"       =>
    case "b1a2"       =>
    case "c1a2"       =>
    case "d1a2"       =>
    case "e1a2"       =>
    case "f1a3"       =>
    case "_a"         =>
    case "_b"         =>
    case "_c"         =>
    case "_d"         =>
    case "_e"         =>
    case "_f"         =>
    case "_aa"        =>
    case "_ba"        =>
    case "_ca"        =>
    case "_da"        =>
    case "_ea"        =>
    case "_f1a"       =>
    case "_a1a"       =>
    case "_b1a"       =>
    case "_c1a"       =>
    case "_d1a"       =>
    case "_e1a"       =>
    case "_f1a0"      =>
    case "_f1a2"      =>
    case "_a1a2"      =>
    case "_b1a2"      =>
    case "_c1a2"      =>
    case "_d1a2"      =>
    case "_e1a2"      =>
    case "_f1a3"      =>
    case _            =>
  }
}
