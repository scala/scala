/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.util;

/** A name transformer for replacing operator symbols in names by predefined
 *  names of the form $opname.
 *
 *  @author     Martin Odersky, Christine Roeckl
 *  @version    1.1
 */
public class NameTransformer {

    public static String[] operatorName = new String[128];

    static {
       operatorName['~'] = "$tilde";
       operatorName['='] = "$eq";
       operatorName['<'] = "$less";
       operatorName['>'] = "$greater";
       operatorName['!'] = "$bang";
       operatorName['#'] = "$hash";
       operatorName['%'] = "$percent";
       operatorName['^'] = "$up";
       operatorName['&'] = "$amp";
       operatorName['|'] = "$bar";
       operatorName['*'] = "$times";
       operatorName['/'] = "$div";
       operatorName['+'] = "$plus";
       operatorName['-'] = "$minus";
       operatorName[':'] = "$colon";
    }

    /** Replace operator symbols by corresponding "$op_name" in names.
     */
    public static Name encode(Name name) {
       int i = name.index;
       int len = i + name.length();
       StringBuffer res = new StringBuffer();
       while (i < len) {
         byte c = Name.names[i++];
         String nop = operatorName[c];
         if (nop == null)
             res.append((char)c);
         else
             res.append(nop);
       }
       return Name.fromString(res.toString());
    }

    /** Replace "$op_name" by corresponding operator symbols in names.
     */
    public static Name decode(Name name) {
        int i = name.index;
	int length = i + name.length();
	StringBuffer res = new StringBuffer();
	while (i < length) {
	    byte c = Name.names[i++];
	    if ((char)c == '$') {
		String op = cutOut(i, length);
		res.append(decode("$" + op));
		i = i + op.length();
	    }
	    else res.append((char)c);
	}
       return Name.fromString(res.toString());
    }

    /** Decodes (a prefix of) a string.
     */
    public static String decode(String string) {
	for (int i = string.length(); i > 0; i--) {
	    String prefix = string.substring(0, i);
	    String c = lookup(prefix);
	    if (c != null) {
		String suffix = string.substring(i, string.length());
		return c + suffix;
	    }
	}
	return string;
    }

    /** Cuts out the name of an operator plus succeeding characters.
     *  Does NOT return the preceeding '$' symbol.
     */
    private static String cutOut(int pos, int length) {
	int i = pos;    // avoid side-effects
	StringBuffer res = new StringBuffer();
	while (i < length) {
	    char c = (char)Name.names[i++];
	    if (c == '$') return res.toString();
	    else res.append(c);
	}
	return res.toString();
    }

    /** Looks up the array entry for the operator name.
     */
    private static String lookup(String string) {
	for (int i = 0; i < 128; i++) {
	    if ((operatorName[i] != null) && (string.compareTo(operatorName[i]) == 0))
		return String.valueOf((char)i);
	}
	return null;
    }
}
