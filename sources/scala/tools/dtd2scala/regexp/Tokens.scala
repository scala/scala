package scala.tools.dtd2scala.regexp ;

class Tokens {

        // Tokens

                val PCD = 0;
                val NODE = 1;
                val EMPTY = 2;
                val LPAREN = 3;
                val RPAREN = 4;
                val COMMA = 5;
                val STAR = 6;
                val PLUS = 7;
                val OPT = 8;
                val CHOICE = 9;
                val END = 10;

        def token2string( i:int ):String = i.match {
                case 0 => "#PCD";
                case 1 => "NODE";
                case 2 => "EMPTY";
                case 3 => "(";
                case 4 => ")";
                case 5 => ",";
                case 6 => "*";
                case 7 => "+";
                case 8 => "?";
                case 9 => "|";
                case 10 => "END";
        }
}
