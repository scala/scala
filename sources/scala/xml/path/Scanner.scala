package scala.xml.path ;

class Scanner( it:Iterator[char] ) with Tokens {

  val ENDCH : char = (-1).asInstanceOf[char];

  var c : char = '%';
  var token : int = 0;
  var value : String = "";

  def next = if( it.hasNext ) { c = it.next } else { c = ENDCH };

  def nextToken = if( ENDCH == c ) ENDTOK else c match {
    case '@' => AT
    case ',' => COMMA
    case '.' =>{
      next;
      if ( '.' == c ) { next; token = DOTDOT } else { token = DOT }
    }
    case '=' =>next; token = EQUALS
    case '[' =>next; token = LBRACKET
    case ']' =>next; token = RBRACKET
    case '/' =>{
      next;
      if ( '/' == c ) { next; token = SLASHSLASH } else { token = SLASH }
    }
    case '*' =>next; token = STAR
    case _  =>getIdentOrKeyword;
  }

  def isIdentChar = ('a' <= c && c <= 'z');

  def getIdentOrKeyword = {
    val str = getIdent; token = IDENT /*
    keywords.get( str ) match {
      case Some( tok ) => value=""; token = tok
      case None => token = IDENT
    }*/
  }

  def getIdent:String = {
    var cs = c :: Nil;
    next;
    while ( isIdentChar ) { cs = c::cs; next }
    cs.foldLeft ("") { (c,s) => s+c }
  }

}
