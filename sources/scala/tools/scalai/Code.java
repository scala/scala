/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Code.java,v 1.13 2002/09/06 13:04:01 paltherr Exp $
// $Id$

package scalai;

import scalac.symtab.Symbol;
import scalac.util.Debug;

public class Code {

    //########################################################################
    // Public Cases

    public case Block(Code[] stats, Code value);

    public case Label(Symbol symbol, Variable[] variables, Code expression);

    public case Create(ScalaTemplate template); // !!! remove ?
    public case Invoke(Code target, Function function, Code[] arguments, int pos);
    public case Load(Code target, Variable variable);
    public case Store(Code target, Variable variable, Code expression);

    public case Box(Code value);

    public case If(Code cond, Code thenp, Code elsep);
    public case Or(Code lf, Code rg);
    public case And(Code lf, Code rg);

    public case IsScala(Code target, Symbol symbol);
    public case IsJava(Code target, Class clasz);

    public case Literal(Object value);
    public case Self;
    public case Null;

    //########################################################################
    // Public Methods

    public String toString() {
        switch (this) {

        case Block(Code[] stats, Code value):
            StringBuffer buffer = new StringBuffer();
            buffer.append("Block([").append('\n');
            for (int i = 0; i < stats.length; i++) {
                if (i > 0) buffer.append(",\n");
                buffer.append(stats[i]);
            }
            buffer.append("], ").append(value).append(")");
            return buffer.toString();

        case Label(Symbol symbol, Variable[] variables, Code expression):
            StringBuffer buffer = new StringBuffer();
            buffer.append("Label(").append(symbol).append(",[");
            for (int i = 0; i < variables.length; i++) {
                if (i > 0) buffer.append(",\n");
                buffer.append(variables[i]);
            }
            buffer.append("],").append(expression).append(")");
            return buffer.toString();

        case Create(ScalaTemplate template):
            return "Create(" + template + ")";

        case Invoke(Code target, Function function, Code[] arguments, int pos):
            StringBuffer buffer = new StringBuffer();
            buffer.append("Invoke(" + target + "," + function + "," + "[\n");
            for (int i = 0; i < arguments.length; i++) {
                if (i > 0) buffer.append(",\n");
                buffer.append(arguments[i]);
            }
            buffer.append("])");
            return buffer.toString();

        case Load(Code target, Variable variable):
            return "Load(" +  target + "," + variable + ")";

        case Store(Code target, Variable variable, Code expression):
            return "Store(" + target + "," + variable + "," + expression + ")";

        case Box(Code value):
            return "Box(" + value + ")";

        case If(Code cond, Code thenp, Code elsep):
            return "If(" + cond + "," + thenp + "," + elsep + ")";

        case Or(Code lf, Code rg):
            return "Or(" + lf + "," + rg + ")";

        case And(Code lf, Code rg):
            return "And(" + lf + "," + rg + ")";

        case IsScala(Code target, Symbol symbol):
            return "IsScala(" + target + "," +  symbol + ")";

        case IsJava(Code target, Class clasz):
            return "IsJava(" + target + "," + clasz + ")";

        case Literal(Object value):
            return "Literal(" + value + ")";

        case Self:
            return "Self";

        case Null:
            return "Null";

        default:
            throw Debug.abort("illegal code", this);
        }
    }

    //########################################################################
}
