// $Id$

import java.io.*;

public class verbfilterScala {

    static String[] reserved = {
      "abstract", "case", "class", "def", "do", "else",
      "extends", "final", "for", "if", "import",
      "let", "module", "new", "null", "outer", "override",
      "package", "private", "protected", "qualified", "super",
      "this", "type", "val", "var", "with", "yield"};

    static final int TABINC = 8;

    final static String beginVerbatim = "\\begin{verbatim}";
    final static String endVerbatim = "\\end{verbatim}";
    final static String verb = "\\verb";

    static int lineCount, verbLine;

    static boolean startsWith(byte[] buf, int offset, String s) {
	int i = 0;
	while (i < s.length() && buf[offset+i] == s.charAt(i)) i++;
	return i == s.length();
    }

    static void writeString(OutputStream out, String s) throws IOException {
	for (int i = 0; i < s.length(); i++)
	    out.write(s.charAt(i));
    }

    static int skipBlanks(byte[] buf, int offset) {
	while (buf[offset] == ' ') offset++;
	return offset;
    }

    static int compare(byte[] buf, int i, String key) {
	int j = 0;
	int l = key.length();
	while (i < buf.length && j < l) {
	    char bch = (char)buf[i];
	    char kch = key.charAt(j);
	    if (bch < kch) return -1;
	    else if (bch > kch) return 1;
	    i++;
	    j++;
	}
	if (j < l)
	    return -1;
	else if (i < buf.length &&
		 ('A' <= buf[i] && buf[i] <= 'Z' ||
		  'a' <= buf[i] && buf[i] <= 'z' ||
		  '0' <= buf[i] && buf[i] <= '9' ||
		  buf[i] == '_'))
	    return 1;
	else
	    return 0;
    }

    static int keyIndex(byte[] buf, int i, String[] keys) {
	int lo = 0;
	int hi = keys.length - 1;
	while (lo <= hi) {
	    int mid = (hi + lo) / 2;
	    int diff = compare(buf, i, keys[mid]);
	    if (diff < 0) hi = mid - 1;
	    else if (diff > 0) lo = mid + 1;
	    else return mid;
	}
	return -1;
    }

    static int processLeadingWhitespace(byte[] buf,
					int i,
					OutputStream out) throws IOException {
	int col = 0;
	while (true) {
	    if (buf[i] == ' ') {
		writeString(out, "~~"); i++; col++;
	    } else if (buf[i] == '\t') {
		i++;
		do {
		    writeString(out, "~~"); col++;
		} while (col % TABINC != 0);
	    } else {
		return i;
	    }
	}
    }

    static int processVerbatim(byte[] buf,
			       int i,
			       OutputStream out,
			       String delimiter) throws IOException {

        verbLine = lineCount;
        int delimiter0 = delimiter.charAt(0);
	int j = skipBlanks(buf, i);
	if (buf[j] == '\n') { i = j+1; lineCount++; }
	i = processLeadingWhitespace(buf, i, out);
	while (true) {
	    if (buf[i] == delimiter0 && startsWith(buf, i, delimiter))
		return i + delimiter.length();
	    switch (buf[i]) {
	    case ' ':
		writeString(out, "~");
	        break;
	    case '\n':
		writeString(out, "\n");
		j = i+1;
		lineCount++;
		if (buf[j] == delimiter0 && startsWith(buf, j, delimiter))
		    return j + delimiter.length();
		writeString(out, "\\\\");
		if (buf[i+1] == '\n') {
		    writeString(out, "[0.5em]"); i++;
		    lineCount++;
		}
		i = processLeadingWhitespace(buf, i+1, out)-1;
	        break;
	    case '^':
		writeString(out, "\\^~$\\!\\!$");
	        break;
	    case '&':
	        writeString(out, "\\&");
	        break;
	    case '*':
		writeString(out, "$*$");
	        break;
	    case '%':
	        writeString(out, "$\\%$");
		break;
	    case '_':
		writeString(out, "\\_");
		break;
	    case '~':
		writeString(out, "\\~~$\\!\\!$");
		break;
	    case '{':
		writeString(out, "{\\small\\{}");
	        break;
	    case '}':
		writeString(out, "{\\small\\}}");
	        break;
	    case '[':
		writeString(out, "$[$");
		//if (buf[i+1] == ']') out.write('~');
	        break;
	    case ']':
		writeString(out, "$]$");
	        break;
	    case '(':
		writeString(out, "$($");
		break;
	    case ')':
		writeString(out, "$)$");
		break;
	    case ':':
		if (i > 0 && Character.isJavaIdentifierPart((char)buf[i-1]))
		    writeString(out, "\\,");
		writeString(out, "{\\rm :}");
		break;
	    case '<':
		if (buf[i+1] == '=') {
		    writeString(out, "$\\leq$"); i++;
		} else if (buf[i+1] == '-') {
		    writeString(out, "$\\leftarrow$"); i++;
		} else if (buf[i+1] == '<') {
		    writeString(out, "$<\\!$");
		} else {
		    writeString(out, "$<$");
		}
	        break;
	    case '>':
		if (buf[i+1] == '=') {
		    writeString(out, "$\\geq$"); i++;
		} else if (buf[i+1] == '>') {
		    writeString(out, "$>\\!$");
		} else {
		    writeString(out, "$>$");
		}
	        break;
	    case '=':
		if (buf[i+1] == '=') {
		    writeString(out, "$==$"); i++;
		} else if (buf[i+1] == '>') {
		    writeString(out, "$\\Rightarrow$"); i++;
		} else {
		    out.write('=');
		}
	        break;
	    case '/':
                if (buf[i+1] == '/') {
		    out.write ('/');
		    do {
			out.write(buf[i+1]);
			i++;
		    } while (buf[i+1] != '\n' &&
			     (buf[i+1] != delimiter0 ||
			      !startsWith(buf, i+1, delimiter)));
		} else {
		    out.write('/');
		}
		break;
	    case '-':
		if (buf[i+1] == '>') {
		    writeString(out, "$\\rightarrow$");
		    i++;
		} else {
		    writeString(out, "$-$");
		}
	        break;
	    case '+':
		writeString(out, "$+$");
		break;
	    case '|':
	        writeString(out, "$\\,|$");
		break;
	    case '#':
		writeString(out, "\\#");
		break;
	    case '\\':
		if (buf[i+1] == '=' || buf[i+1] == '>') {
		    out.write(buf[i]);
		    i++;
		    out.write(buf[i]);
		} else if (buf[i+1] == '$') {
		    writeString(out, "\\$");
		    i++;
		} else if (buf[i+1] == 'R') {
		    writeString(out, "\\color{red}"); i++;
		} else if (buf[i+1] == 'S') {
		    writeString(out, "\\color{black}"); i++;
		} else if (buf[i+1] == 'B') {
		    writeString(out, "\\color{blue}"); i++;
		} else if (buf[i+1] == 'G') {
		    writeString(out, "\\color{green}"); i++;
		} else {
		    writeString(out, "$\\backslash$");
		}
		break;
	    case '$':
		out.write(buf[i]);
		do {
		    i++;
		    out.write(buf[i]);
		} while (i + 1 < buf.length && buf[i] != '$');
		break;
	    default:
  	        if (i == 0 || !Character.isJavaIdentifierPart((char)buf[i-1])) {
	            int k = keyIndex(buf, i, reserved);
		    if (k >= 0) {
		        writeString(out, "{\\vem " + reserved[k] + "}");
		        i = i + reserved[k].length() - 1;
                        break;
                    }
                }
	        out.write(buf[i]);
		break;
	    }
	    i++;
	}
    }

    static void process(byte[] buf, OutputStream out) throws IOException {
	int i = 0;
	while (i < buf.length - 1) {
	    if (buf[i] == '%') {
		do {
		    out.write(buf[i]);
		    i++;
		} while (buf[i] != '\n' && buf[i] != 0);
	    } else if (startsWith(buf, i, beginVerbatim)) {
		writeString(out, "\\begin{program}");
		i = processVerbatim(buf, i + beginVerbatim.length(), out,
				    endVerbatim);
		writeString(out, "\\end{program}");
	    } else if (startsWith(buf, i, verb)) {
		writeString(out, "\\prog{");
		i = i + verb.length();
		char[] delimiterArray = {(char)buf[i]};
		String delimiter = new String(delimiterArray);
		i = processVerbatim(buf, i + 1, out, delimiter);
		writeString(out, "}");
	    } else {
	        if (buf[i] == '\n') lineCount++;
		out.write(buf[i]);
		i++;
	    }
	}
    }

    public static void main(String[] argv) throws IOException {
        if (argv.length != 2) {
            String classname = new Error().getStackTrace()[0].getClassName();
            System.err.println(
                "Usage: " + classname + " <source-file> <destination-file>");
            System.exit(1);
        }
        InputStream in = new FileInputStream(new File(argv[0]));
        byte[] buf = new byte[in.available() + 1];
        in.read(buf, 0, buf.length-1);
        in.close();
        OutputStream out =
            new BufferedOutputStream(
                new FileOutputStream(
                    new File(argv[1])));
        try {
            writeString(out,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
            writeString(out,"% DO NOT EDIT.  Automatically generated file! %\n");
            writeString(out,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
            writeString(out,"\n");
            lineCount = 1;
            process(buf, out);
        } catch (RuntimeException ex) {
            System.err.println ("\n **** error at line " + verbLine);
            throw ex;
        }
        out.close();
    }
}
