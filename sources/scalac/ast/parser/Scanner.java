/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

import scalac.*;
import scalac.util.Name;
import scalac.util.Position;

/** A scanner for the programming language Scala.
 *
 *  @author     Matthias Zenger, Martin Odersky
 *  @version    1.0
 */
public class Scanner extends TokenData {

    /** layout & character constants
     */
    public int tabinc = 8;
    public final static byte LF = 0xA;
    protected final static byte FF = 0xC;
    protected final static byte CR = 0xD;
    protected final static byte SU = Sourcefile.SU;

    /** the names of all tokens
     */
    public Name[]       tokenName = new Name[128];
    public int          numToken = 0;

    /** keyword array; maps from name indices to tokens
     */
    protected byte[]    key;
    protected int       maxKey = 0;

    /** we need one token lookahead
     */
    protected TokenData next = new TokenData();
    protected TokenData prev = new TokenData();

    /** the first character position after the previous token
     */
    public int          lastpos = 0;

    /** the last error position
     */
    public int          errpos = -1;

    /** the input buffer:
     */
    protected byte[]    buf;
    protected int       bp;

    /** the current character
     */
    protected byte      ch;

    /** the line and column position of the current character
     */
    public int          cline;
    public int          ccol;

    /** the current sourcefile
     */
    public Sourcefile   currentSource;

    /** a buffer for character and string literals
     */
    protected byte[]    lit = new byte[64];
    protected int       litlen;

    /** the compilation unit
     */
    public Unit unit;


    /** Construct a scanner from a file input stream.
     */
    public Scanner(Unit unit) {
        this.unit = unit;
        buf = (currentSource = unit.source).getBuffer();
        cline = 1;
        bp = -1;
        ccol = 0;
	nextch();
        token = EMPTY;
        init();
        nextToken();
    }

    private void nextch() {
	ch = buf[++bp]; ccol++;
    }

    /** read next token and return last position
     */
    public int skipToken() {
        int p = pos;
        nextToken();
        return p;
    }

    public void nextToken() {
	if (token == RBRACE) {
	    int prevpos = pos;
	    fetchToken();
	    switch (token) {
	    case ELSE:   case EXTENDS:  case WITH:
	    case YIELD:  case DO:
	    case COMMA:  case SEMI:     case DOT:
	    case COLON:  case EQUALS:   case ARROW:
            case LARROW: case SUBTYPE:  case AT:
            case HASH:   case AS:       case IS:
	    case RPAREN: case RBRACKET: case RBRACE:
		break;
	    default:
		if (token == EOF ||
		    ((pos >>> Position.LINESHIFT) >
		     (prevpos >>> Position.LINESHIFT))) {
		    next.copyFrom(this);
		    this.token = SEMI;
		    this.pos = prevpos;
		}
	    }
	} else {
	    if (next.token == EMPTY) {
		fetchToken();
	    } else {
		copyFrom(next);
		next.token = EMPTY;
	    }
	    if (token == CASE) {
		prev.copyFrom(this);
		fetchToken();
		if (token == CLASS) {
		    token = CASECLASS;
		} else {
		    next.copyFrom(this);
		    this.copyFrom(prev);
		}
	    } else if (token == SEMI) {
		prev.copyFrom(this);
		fetchToken();
		if (token != ELSE) {
		    next.copyFrom(this);
		    this.copyFrom(prev);
		}
	    }
	}
	//System.out.println("<" + token2string(token) + ">");//DEBUG
    }

    /** read next token
     */
    public void fetchToken() {
        if (token == EOF) return;
        lastpos = Position.encode(cline, ccol, currentSource.id);
	int index = bp;
	while(true) {
	    switch (ch) {
	    case ' ':
		nextch();
		break;
	    case '\t':
		ccol = ((ccol - 1) / tabinc * tabinc) + tabinc;
		nextch();
		break;
	    case CR:
		cline++;
		ccol = 0;
		nextch();
		if (ch == LF) {
		    ccol = 0;
		    nextch();
		}
		break;
	    case LF:
	    case FF:
		cline++;
		ccol = 0;
		nextch();
		break;
	    default:
		pos = Position.encode(cline, ccol, currentSource.id);
		index = bp;
		switch (ch) {
		case 'A': case 'B': case 'C': case 'D': case 'E':
		case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O':
		case 'P': case 'Q': case 'R': case 'S': case 'T':
		case 'U': case 'V': case 'W': case 'X': case 'Y':
		case 'Z': case '$':
		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o':
		case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y':
		case 'z':
		    nextch();
		    getIdentRest(index);
		    return;
		case '~': case '!': case '@': case '#': case '%':
		case '^': case '*': case '+': case '-': case '<':
		case '>': case '?': case ':':
		case '=': case '&': case '|':
		    nextch();
		    getOperatorRest(index);
		    return;
		case '/':
		    nextch();
		    if (!skipComment()) {
			getOperatorRest(index);
			return;
		    }
		    break;
		case '_':
		    nextch();
		    getIdentOrOperatorRest(index);
		    return;
		case '0':
		    nextch();
		    if (ch == 'x' || ch == 'X') {
			nextch();
			getNumber(index + 2, 16);
		    } else
			getNumber(index, 8);
		    return;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		    getNumber(index, 10);
		    return;
		case '\"':
		    nextch();
		    litlen = 0;
		    while (ch != '\"' && ch != CR && ch != LF && ch != SU)
			getlitch();
		    if (ch == '\"') {
			token = STRINGLIT;
			name = Name.fromSource(lit, 0, litlen);
			nextch();
		    }
		    else
			syntaxError("unclosed character literal");
		    return;
		case '\'':
		    nextch();
		    litlen = 0;
		    getlitch();
		    if (ch == '\'') {
			nextch();
			token = CHARLIT;
			byte[] ascii = new byte[litlen * 2];
			int alen = SourceRepresentation.source2ascii(lit, 0, litlen, ascii);
			if (alen > 0)
			    intVal = SourceRepresentation.ascii2string(ascii, 0, alen).charAt(0);
			else
			    intVal = 0;
		    } else
			syntaxError("unclosed character literal");
		    return;
		case '.':
		    nextch();
		    if (('0' <= ch) && (ch <= '9')) getFraction(index);
		    else token = DOT;
		    return;
		case ';':
		    nextch(); token = SEMI;
		    return;
		case ',':
		    nextch(); token = COMMA;
		    return;
		case '(':
		    nextch(); token = LPAREN;
		    return;
		case '{':
		    nextch(); token = LBRACE;
		    return;
		case ')':
		    nextch(); token = RPAREN;
		    return;
		case '}':
		    nextch(); token = RBRACE;
		    return;
		case '[':
		    nextch(); token = LBRACKET;
		    return;
		case ']':
		    nextch(); token = RBRACKET;
		    return;
		case SU:
		    token = EOF;
		    currentSource.lines = cline;
		    return;
		default:
		    nextch();
		    syntaxError("illegal character");
		    return;
		}
	    }
        }
    }

    private boolean skipComment() {
	if (ch == '/') {
	    do {
		nextch();
	    } while ((ch != CR) && (ch != LF) && (ch != SU));
	    return true;
	} else if (ch == '*') {
	    int openComments = 1;
	    while (openComments > 0) {
		do {
		    do {
			if (ch == CR) {
			    cline++;
			    ccol = 0;
			    nextch();
			    if (ch == LF) {
				ccol = 0;
				nextch();
			    }
			} else if (ch == LF) {
			    cline++;
			    ccol = 0;
			    nextch();
			}
			else if (ch == '\t') {
			    ccol = ((ccol - 1) / tabinc * tabinc) + tabinc;
			    nextch();
			} else if (ch == '/') {
			    nextch();
			    if (ch == '*') {
				nextch();
				openComments++;
			    }
			} else {
			    nextch();
			}
		    } while ((ch != '*') && (ch != SU));
		    while (ch == '*') {
			nextch();
		    }
		} while (ch != '/' && ch != SU);
		if (ch == '/') {
		    nextch();
		    openComments--;
		} else {
		    syntaxError("unclosed comment");
		    return true;
		}
	    }
	    return true;
	} else {
	    return false;
	}
    }

    private void getIdentRest(int index) {
	while (true) {
	    switch (ch) {
	    case 'A': case 'B': case 'C': case 'D': case 'E':
	    case 'F': case 'G': case 'H': case 'I': case 'J':
	    case 'K': case 'L': case 'M': case 'N': case 'O':
	    case 'P': case 'Q': case 'R': case 'S': case 'T':
	    case 'U': case 'V': case 'W': case 'X': case 'Y':
	    case 'Z': case '$':
	    case 'a': case 'b': case 'c': case 'd': case 'e':
	    case 'f': case 'g': case 'h': case 'i': case 'j':
	    case 'k': case 'l': case 'm': case 'n': case 'o':
	    case 'p': case 'q': case 'r': case 's': case 't':
	    case 'u': case 'v': case 'w': case 'x': case 'y':
	    case 'z':
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
		nextch();
		break;
	    case '_':
		nextch();
		getIdentOrOperatorRest(index);
		return;
	    default:
		treatIdent(index, bp);
		return;
	    }
	}
    }

    private void getOperatorRest(int index) {
	while (true) {
	    switch (ch) {
	    case '~': case '!': case '@': case '#': case '%':
	    case '^': case '*': case '+': case '-': case '<':
	    case '>': case '?': case ':':
	    case '=': case '&': case '|':
		nextch();
		break;
	    case '/':
		int lastbp = bp;
		nextch();
		if (skipComment()) {
		    treatIdent(index, lastbp);
		    return;
		} else {
		    break;
		}
	    case '_':
		nextch();
		getIdentOrOperatorRest(index);
		return;
	    default:
		treatIdent(index, bp);
		return;
	    }
	}
    }

    private void getIdentOrOperatorRest(int index) {
	switch (ch) {
	case 'A': case 'B': case 'C': case 'D': case 'E':
	case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O':
	case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y':
	case 'Z': case '$':
	case 'a': case 'b': case 'c': case 'd': case 'e':
	case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o':
	case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y':
	case 'z':
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    getIdentRest(index);
	    return;
	case '~': case '!': case '@': case '#': case '%':
	case '^': case '*': case '+': case '-': case '<':
	case '>': case '?': case ':':
	case '=': case '&': case '|':
	case '/':
	    getOperatorRest(index);
	    return;
	case '_':
	    nextch();
	    getIdentOrOperatorRest(index);
	    return;
	default:
	    treatIdent(index, bp);
	    return;
	}
    }

    void treatIdent(int start, int end) {
	name = Name.fromAscii(buf, start, end - start);
	if (name.index <= maxKey)
	    token = key[name.index];
	else
	    token = IDENTIFIER;
    }

    /** generate an error at the given position
     */
    void syntaxError(int pos, String msg) {
        unit.error(pos, msg);
        token = ERROR;
        errpos = pos;
    }

    /** generate an error at the current token position
     */
    void syntaxError(String msg) {
        syntaxError(pos, msg);
    }

    /** append characteter to "lit" buffer
     */
    protected void putch(byte c) {
        if (litlen == lit.length) {
            byte[] newlit = new byte[lit.length * 2];
            System.arraycopy(lit, 0, newlit, 0, lit.length);
            lit = newlit;
        }
        lit[litlen++] = c;
    }

    /** return true iff next 6 characters are a valid unicode sequence:
     */
    protected boolean isUnicode() {
        return
            (bp + 6) < buf.length &&
            (buf[bp] == '\\') &&
            (buf[bp+1] == 'u') &&
            (SourceRepresentation.digit2int(buf[bp+2], 16) >= 0) &&
            (SourceRepresentation.digit2int(buf[bp+3], 16) >= 0) &&
            (SourceRepresentation.digit2int(buf[bp+4], 16) >= 0) &&
            (SourceRepresentation.digit2int(buf[bp+5], 16) >= 0);
    }

    /** read next character in character or string literal:
     */
    protected void getlitch() {
        if (ch == '\\') {
            if (isUnicode()) {
                putch(ch); nextch();
                putch(ch); nextch();
                putch(ch); nextch();
                putch(ch); nextch();
                putch(ch); nextch();
                putch(ch); nextch();
            } else {
                nextch();
                if ('0' <= ch && ch <= '7') {
                    byte leadch = ch;
                    int oct = SourceRepresentation.digit2int(ch, 8);
                    nextch();
                    if ('0' <= ch && ch <= '7') {
                        oct = oct * 8 + SourceRepresentation.digit2int(ch, 8);
                        nextch();
                        if (leadch <= '3' && '0' <= ch && ch <= '7') {
                            oct = oct * 8 + SourceRepresentation.digit2int(ch, 8);
                            nextch();
                        }
                    }
                    putch((byte)oct);
                } else if (ch != SU) {
                    switch (ch) {
                        case 'b': case 't': case 'n':
                        case 'f': case 'r': case '\"':
                        case '\'': case '\\':
                            putch((byte)'\\');
                            putch(ch);
                            break;
                        default:
                            syntaxError(Position.encode(cline, ccol, currentSource.id) - 1, "invalid escape character");
                            putch(ch);
                    }
                    nextch();
                }
            }
        } else if (ch != SU) {
            putch(ch);
            nextch();
        }
    }

    /** read fractional part of floating point number;
     *  Then floatVal := buf[index..], converted to a floating point number.
     */
    protected void getFraction(int index) {
        while (SourceRepresentation.digit2int(ch, 10) >= 0) {
            nextch();
        }
        token = DOUBLELIT;
        if ((ch == 'e') || (ch == 'E')) {
            nextch();
            if ((ch == '+') || (ch == '-')) {
                byte sign = ch;
                nextch();
                if (('0' > ch) || (ch > '9')) {
                    ch = sign;
                    bp--;
                    ccol--;
                }
            }
            while (SourceRepresentation.digit2int(ch, 10) >= 0) {
                nextch();
            }
        }
        double limit = Double.MAX_VALUE;
        if ((ch == 'd') || (ch == 'D')) {
            nextch();
        } else if ((ch == 'f') || (ch == 'F')) {
            token = FLOATLIT;
            limit = Float.MAX_VALUE;
            nextch();
        }
        try {
            floatVal = Double.valueOf(new String(buf, index, bp - index)).doubleValue();
            if (floatVal > limit)
                syntaxError("floating point number too large");
        } catch (NumberFormatException e) {
            syntaxError("malformed floating point number");
        }
    }

    /** intVal := buf[index..index+len-1], converted to an integer number.
     *  base = the base of the number; one of 8, 10, 16.
     *  max  = the maximal number before an overflow.
     */
    protected void makeInt (int index, int len, int base, long max) {
        intVal = 0;
        int divider = (base == 10 ? 1 : 2);
        for (int i = 0; i < len; i++) {
            int d = SourceRepresentation.digit2int(buf[index + i], base);
            if (d < 0) {
                syntaxError("malformed integer number");
                return;
            }
            if (intVal < 0 ||
                max / (base / divider) < intVal ||
                max - (d / divider) < (intVal * (base / divider) - 0)) {
                syntaxError("integer number too large");
                return;
            }
            intVal = intVal * base + d;
        }
    }

    /** read a number,
     *  and convert buf[index..], setting either intVal or floatVal.
     *  base = the base of the number; one of 8, 10, 16.
     */
    protected void getNumber(int index, int base) {
        while (SourceRepresentation.digit2int(ch, base == 8 ? 10 : base) >= 0) {
            nextch();
        }
        if (base <= 10 && ch == '.') {
            nextch();
            if ((ch >= '0') && (ch <= '9'))
                getFraction(index);
            else {
                ch = buf[--bp]; ccol--;
                makeInt(index, bp - index, base, Integer.MAX_VALUE);
                intVal = (int)intVal;
                token = INTLIT;
            }
        } else if (base <= 10 &&
           (ch == 'e' || ch == 'E' ||
            ch == 'f' || ch == 'F' ||
            ch == 'd' || ch == 'D'))
            getFraction(index);
        else {
            if (ch == 'l' || ch == 'L') {
                makeInt(index, bp - index, base, Long.MAX_VALUE);
                nextch();
                token = LONGLIT;
            } else {
                makeInt(index, bp - index, base, Integer.MAX_VALUE);
                intVal = (int)intVal;
                token = INTLIT;
            }
        }
    }

    public int name2token(Name name) {
        if (name.index <= maxKey)
            return key[name.index];
        else
            return IDENTIFIER;
    }

    public String token2string(int token) {
        switch (token) {
            case IDENTIFIER:
                return "identifier";
            case CHARLIT:
                return "character literal";
            case INTLIT:
                return "integer literal";
            case LONGLIT:
                return "long literal";
            case FLOATLIT:
                return "float literal";
            case DOUBLELIT:
                return "double literal";
            case STRINGLIT:
                return "string literal";
            case LPAREN:
                return "'('";
            case RPAREN:
                return "')'";
            case LBRACE:
                return "'{'";
            case RBRACE:
                return "'}'";
            case LBRACKET:
                return "'['";
            case RBRACKET:
                return "']'";
            case EOF:
                return "eof";
            case ERROR:
                return "something";
            case SEMI:
                return "';'";
            case COMMA:
                return "','";
            default:
                try {
                    return "'" + tokenName[token].toString() + "'";
                } catch (ArrayIndexOutOfBoundsException e) {
                    return "'<" + token + ">'";
                }
        }
    }

    public String toString() {
        switch (token) {
            case IDENTIFIER:
                return "id(" + name + ")";
            case CHARLIT:
                return "char(" + intVal + ")";
            case INTLIT:
                return "int(" + intVal + ")";
            case LONGLIT:
                return "long(" + intVal + ")";
            case FLOATLIT:
                return "float(" + floatVal + ")";
            case DOUBLELIT:
                return "double(" + floatVal + ")";
            case STRINGLIT:
                return "string(" + name + ")";
            case SEMI:
                return ";";
            case COMMA:
                return ",";
            default:
                return token2string(token);
        }
    }

    protected void enterKeyword(String s, int tokenId) {
        while (tokenId > tokenName.length) {
            Name[]  newTokName = new Name[tokenName.length * 2];
            System.arraycopy(tokenName, 0, newTokName, 0, newTokName.length);
            tokenName = newTokName;
        }
        Name n = Name.fromString(s);
        tokenName[tokenId] = n;
        if (n.index > maxKey)
            maxKey = n.index;
        if (tokenId >= numToken)
            numToken = tokenId + 1;
    }

    protected void init() {
        initKeywords();
        key = new byte[maxKey+1];
        for (int i = 0; i <= maxKey; i++)
            key[i] = IDENTIFIER;
        for (byte j = 0; j < numToken; j++)
            if (tokenName[j] != null)
                key[tokenName[j].index] = j;
    }

    protected void initKeywords() {
        enterKeyword("if", IF);
        enterKeyword("for", FOR);
        enterKeyword("else", ELSE);
        enterKeyword("this", THIS);
        enterKeyword("null", NULL);
        enterKeyword("new", NEW);
        enterKeyword("with", WITH);
        enterKeyword("super", SUPER);
        enterKeyword("case", CASE);
        enterKeyword("val", VAL);
        enterKeyword("abstract", ABSTRACT);
        enterKeyword("final", FINAL);
        enterKeyword("private", PRIVATE);
        enterKeyword("protected", PROTECTED);
        enterKeyword("qualified", QUALIFIED);
        enterKeyword("override", OVERRIDE);
        enterKeyword("var", VAR);
        enterKeyword("def", DEF);
        enterKeyword("type", TYPE);
        enterKeyword("extends", EXTENDS);
        enterKeyword("let", LET);
        enterKeyword("module", MODULE);
        enterKeyword("class",CLASS);
        enterKeyword("constr",CONSTR);
        enterKeyword("import", IMPORT);
        enterKeyword("package", PACKAGE);
        enterKeyword(".", DOT);
        enterKeyword("_", USCORE);
        enterKeyword(":", COLON);
        enterKeyword("=", EQUALS);
        enterKeyword("=>", ARROW);
        enterKeyword("<-", LARROW);
        enterKeyword("<:", SUBTYPE);
        enterKeyword("yield", YIELD);
        enterKeyword("do", DO);
        enterKeyword("@", AT);
        enterKeyword("#", HASH);
        enterKeyword("trait", TRAIT);
	enterKeyword("as", AS);
	enterKeyword("is", IS);
    }
}


