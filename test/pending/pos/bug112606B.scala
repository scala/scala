package test;
trait Test2 {
  case class Token(text : String);
  class KeywordToken(text : String) extends Token(text);
  def decode(tok : KeywordToken) = tok match {
  // constructor cannot be instantiated to expected type;
  //  found   : Test2.this.Token
  //  required: Test2.this.KeywordToken	
  case Token("final") => true;   
  }
}
