object foo  {
  case class Bar(a:String, b:Object, c:String*);


  Bar("foo","meets","bar") match {
    case Bar("foo",_*) => error("huh?");
  }

}
