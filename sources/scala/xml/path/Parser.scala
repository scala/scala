package scala.xml.path ;

class Parser( it:Iterator[char] ) with Scanner( it ) {

  def acc( tok:int ) = {
    if( token != tok ) error("unexpected token") else {};
    nextToken
  }

  def getName: String = {
    if( token != NAME ) error("expected * or a name") else {};
    val x = value;
    nextToken;
  }

  def parseString( xpath:String ) = {
    //initScanner( new IterableString( xpath ));
    expr
  }

 def expr:Expr = {
    var es : Expr = Root;
    while( token != ENDTOK ) token.match {

      case AT =>
	nextToken; es = Attribute( ident ) :: es;

      case SLASH =>
        val x = getName;
        if( x == "*" )
          es = Child(Wildcard, es)
        else
          es = Child(NameTest( x ), es)

      case SLASHSLASH =>
        val x = getName;
        if( x == "*" )
          es = DescOrSelf(Wildcard, es)
        else
          es = DescOrSelf(NameTest( x ), es)

    }
    es.reverse;
  }

  def conds = token match  {
    case LBRACKET =>
      nextToken;
      var cond :List[List[Expression]] = List( expr );
      while( COMMA == token ) { cond = expr :: cond }
      acc( RBRACKET );
      Some( cond.reverse )
    case _ =>
      None
  }

  def ident = {
    val label = value; acc(IDENT); label
  }
}
