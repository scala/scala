package scala.tools.dtd2scala.regexp ;

/** Parser for regexps (content models in DTD element declarations) */

object Parser with Scanner { // a bit too permissive concerning #PCDATA

  /** parses the argument to a regexp */
  def parse(s:String):RegExp = { initScanner( s ); regexp }

  //                                              zzz   parser methods   zzz
  private def accept( tok:int ) = {
    if( token != tok ) error("unexpected token:"+token2string(token));
    nextToken
  }

  private def suffix = (token==STAR)||(token==PLUS)||(token==OPT);

  private def handleSuffix(s:RegExp) = {
    if( token == STAR ) { nextToken; Star( s ) }
    else if( token == PLUS ){ nextToken; Seq( s, Star( s )) }
    else if( token == OPT ) { nextToken; Alt( Eps, s ); }
    else { error("suffix expected"); PCDATA_ }
  }

  private def maybeSuffix(s:RegExp) = if( suffix ) handleSuffix( s ) else s;

  //                                regexp ::= seq [ '+' | '*' | '?' ]
  private def regexp:RegExp = maybeSuffix( seq );

  //                                seq    ::= factor { ',' factor }
  private def seq = {
    val f = factor;
    if( token != COMMA ) f else {
      var fs:List[ RegExp ] = f :: Nil;
      while( token == COMMA ) { nextToken; fs = factor :: fs }
      Seq( fs.reverse:_* );
    }
  }

  //                                factor ::= atomSuffix  { '|' atomSuffix }
  private def factor:RegExp = {
    var a = atomSuffix;
    if( token != CHOICE ) a else {
      var as:List[ RegExp ] = a :: Nil;
      while( token == CHOICE ) { nextToken; as = atomSuffix :: as; }
      Alt( as.reverse:_* );
    }
  }

  //                            atomSuffix ::= atom [ '+' | '*' | '?' ]
  private def atomSuffix = maybeSuffix( atom );

  //                                  atom ::=  '(' regexp ')'
  //                                         |  '#PCDATA'
  //                                         |  node
  //                                         |  END
  private def atom =
    if( token == LPAREN )   { nextToken; val a = regexp; accept( RPAREN ); a }
    else if( token == PCD ) { nextToken; PCDATA_ }
    else if( token == NODE ){ val a = Node( value ); nextToken; a }
    else if( token == END ) { Eps }
    else error("unexpected token:"+token2string( token ));

}
