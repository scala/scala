package scala.xml.path ;

class Parser( it:Iterator[char] ) with Scanner( it ) {

  def acc( tok:int ) = {
    if( token != tok ) error("unexpected token") else {};
    nextToken
  }

  def parseString( xpath:String ) = {
    //initScanner( new IterableString( xpath ));
    expr
  }

 def expr:List[Expression] = {
    var es : List[Expression] = Nil;
    while( token != ENDTOK ) token.match {

      case AT =>
	nextToken; es = Attribute( ident ) :: es;

      case DOT =>
	nextToken; acc( SLASH );

      case IDENT => {
	val label = value;
        nextToken;
        val cs = conds;
        es = PathNode( label, cs ) :: es
      }
      case SLASH =>
	nextToken;

      case SLASHSLASH =>
	nextToken; es = Descendant :: es ;

      case STAR =>
	nextToken; es = Wildcard :: es;

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
