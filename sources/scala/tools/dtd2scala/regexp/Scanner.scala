package scala.tools.dtd2scala.regexp ;

/** Scanner for regexps (content models in DTD element declarations) */
class Scanner with Tokens {

  //                                                   zzz   constants   zzz
  private val ENDCH:char = '%';

  //                                                      zzz   fields   zzz
  var token:int = 0;
  var value:String = "";

  private var it:Iterator[char] = null;
  private var c:char = 'z';


  /** initializes the scanner on input s */
  def initScanner( s:String ) = {
    it = Iterator.fromString(s);
    next;
    token = 0;
    nextToken;
  }

  /** scans the next token */
  def nextToken = { token = if( token != END ) readToken else END }

  //                                            zzz    scanner methods   zzz

  // todo: see XML specification... probably isLetter,isDigit is fine
  private def isIdentChar = { ('a' <= c && c <= 'z')
                      || ('A' <= c && c <= 'Z')};

  private def next = if( it.hasNext ) c = it.next else c = ENDCH;

  private def skip( i:int ):Unit = if( i > 0 ) { next; skip(i-1) } else { }

  private def readToken:int = c.match {
    case '(' => next; LPAREN;
    case ')' => next; RPAREN;
    case ',' => next; COMMA;
    case '*' => next; STAR;
    case '+' => next; PLUS;
    case '?' => next; OPT;
    case '|' => next; CHOICE;
    case '#' => skip( "PCDATA".length() ); next; PCD
    case '%' => END;
    case _   => if( isIdentChar )
		  { readValue; NODE }
		else
		  { error("unexpected character:"+c); END }
  }

  private def readValue = {
    var buf = c :: Nil; next; while ( isIdentChar ) { buf = c::buf; next }
    value = buf.foldLeft ("") { ( s, c ) => c+s };
  }

}
