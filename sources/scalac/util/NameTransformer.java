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
       operatorName['\\']= "$bslash";
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
    public static String decode(Name name) {
        return decode(name.toString());
    }
    public static String decode(String string) {
        StringBuffer buffer = null;
        for (int i = 0; i < string.length(); i++) {
            char c = string.charAt(i);
            if (c == '$') {
                int index = -1;
                int length = -1; // an operator may be a prefix of another one
                for (int j = 0; j < operatorName.length; j++) {
                    String operator = operatorName[j];
                    if (operator == null) continue;
                    if (operator.length() <= length) continue;
                    if (!string.startsWith(operator, i)) continue;
                    index = j;
                    length = operator.length();
                }
                if (length >= 0) {
                    if (buffer == null) {
                        int capacity = string.length() - length + 1;
                        buffer = new StringBuffer(capacity);
                        buffer.append(string.substring(0, i));
                    }
                    buffer.append((char)index);
                    i += length - 1;
                    continue;
                }
            }
            if (buffer != null) buffer.append(c);
        }
        return buffer == null ? string : buffer.toString();
    }

}
