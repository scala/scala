package scalap;

object Names {

	val operatorName = new Array[String](128);
	operatorName('$') = "$";
    operatorName('~') = "$tilde";
	operatorName('=') = "$eq";
	operatorName('<') = "$less";
	operatorName('>') = "$greater";
	operatorName('!') = "$bang";
	operatorName('#') = "$hash";
	operatorName('%') = "$percent";
	operatorName('^') = "$up";
	operatorName('&') = "$amp";
	operatorName('|') = "$bar";
	operatorName('*') = "$times";
	operatorName('/') = "$div";
	operatorName('+') = "$plus";
	operatorName('-') = "$minus";
	operatorName(':') = "$colon";

    /** Replace operator symbols by corresponding "$op_name" in names.
     */
    def encode(name: String): String = {
      	var i = 0;
      	val len = name.length();
      	val res = new StringBuffer();
       	while (i < len) {
     		val c = name.charAt(i);
         	if (c < 128) {
         		val nop = operatorName(c);
         		if (nop == null)
            		res.append(c);
         		else
             		res.append(nop);
         	} else
         		res.append(c);
         	i = i + 1;
       }
       res.toString()
    }

    /** Replace "$op_name" by corresponding operator symbols in names.
     */
    def decode(name: String): String = {
        var i = 0;
		val len = name.length();
		val res = new StringBuffer();
		while (i < len) {
			val c = name.charAt(i);
			if (c == '$') {
			    var j = len;
			    while (j > i) {
			        val prefix = name.substring(i, j);
			        val c = lookup(prefix);
	    			if (c != null) {
	    				i = j;
						res.append(c);
					} else
			        	j = j - 1;
			    }
			} else {
				i = i + 1;
			    res.append(c);
			}
		}
    	res.toString()
    }

    /** Looks up the array entry for the operator name.
     */
    def lookup(string: String): String = {
    	var i = 0;
    	var res: String = null;
    	while (i < 128) {
    	    if (string.equals(operatorName(i))) {
    	        res = String.valueOf(i.asInstanceOf[Char]);
    	        i = 128;
    	    }
    	    i = i + 1;
    	}
	    res
    }
}
