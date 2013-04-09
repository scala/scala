object foo  {
  case class Bar(a:String, b:AnyRef, c:String*);

  Bar("foo","meets","bar") match {
    case Bar("foo",_*) => sys.error("huh?");
  }

}
