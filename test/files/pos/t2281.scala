import scala.collection.mutable.ArrayBuffer

class A {
  def f(x: Boolean) = if (x) <br/><br/> else <br/>
}

class B {
  def splitSentences(text : String) : ArrayBuffer[String] = {
     val outarr = new ArrayBuffer[String]
     var outstr = new StringBuffer
     var prevspace = false
     val ctext = text.replaceAll("\n+","\n")
  		ctext foreach {c =>
  		    outstr append c
  			if(c == '.' || c == '!' || c == '?' || c == '\n' || c == ':' || c == ';' || (prevspace && c == '-') ){
  				outarr += outstr.toString
  				outstr = new StringBuffer
  			}
  		    if(c == '\n'){
  		    	outarr += "\n\n"
  		    }
  		    prevspace = c == ' '
  		}
  	    if(outstr.length > 0){
  	    	outarr += outstr.toString
  	    }
  	    outarr
  	  }

  def spanForSentence(x : String,picktext : String) = 
    if(x == "\n\n"){
      <br/><br/>
    }else{
      <span class='clicksentence' style={if(x == picktext) "background-color: yellow" else ""}>{x}</span>    		
    }

  def selectableSentences(text : String, picktext : String) = {
    val sentences = splitSentences(text)
    sentences.map(x => spanForSentence(x,picktext))
  }
}