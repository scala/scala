/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.io.*;

/**
 *  A buffer that can contain ANSI text.
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class ANSIBuffer {
    private boolean ansiEnabled = true;
    private final StringBuffer ansiBuffer = new StringBuffer();
    private final StringBuffer plainBuffer = new StringBuffer();

    public ANSIBuffer() {
    }

    public ANSIBuffer(final String str) {
        append(str);
    }

    public void setAnsiEnabled(final boolean ansi) {
        this.ansiEnabled = ansi;
    }

    public boolean getAnsiEnabled() {
        return this.ansiEnabled;
    }

    public String getAnsiBuffer() {
        return ansiBuffer.toString();
    }

    public String getPlainBuffer() {
        return plainBuffer.toString();
    }

    public String toString(final boolean ansi) {
        return ansi ? getAnsiBuffer() : getPlainBuffer();
    }

    public String toString() {
        return toString(ansiEnabled);
    }

    public ANSIBuffer append(final String str) {
        ansiBuffer.append(str);
        plainBuffer.append(str);

        return this;
    }

    public ANSIBuffer attrib(final String str, final int code) {
        ansiBuffer.append(ANSICodes.attrib(code)).append(str)
                  .append(ANSICodes.attrib(ANSICodes.OFF));
        plainBuffer.append(str);

        return this;
    }

    public ANSIBuffer red(final String str) {
        return attrib(str, ANSICodes.FG_RED);
    }

    public ANSIBuffer blue(final String str) {
        return attrib(str, ANSICodes.FG_BLUE);
    }

    public ANSIBuffer green(final String str) {
        return attrib(str, ANSICodes.FG_GREEN);
    }

    public ANSIBuffer black(final String str) {
        return attrib(str, ANSICodes.FG_BLACK);
    }

    public ANSIBuffer yellow(final String str) {
        return attrib(str, ANSICodes.FG_YELLOW);
    }

    public ANSIBuffer magenta(final String str) {
        return attrib(str, ANSICodes.FG_MAGENTA);
    }

    public ANSIBuffer cyan(final String str) {
        return attrib(str, ANSICodes.FG_CYAN);
    }

    public ANSIBuffer bold(final String str) {
        return attrib(str, ANSICodes.BOLD);
    }

    public ANSIBuffer underscore(final String str) {
        return attrib(str, ANSICodes.UNDERSCORE);
    }

    public ANSIBuffer blink(final String str) {
        return attrib(str, ANSICodes.BLINK);
    }

    public ANSIBuffer reverse(final String str) {
        return attrib(str, ANSICodes.REVERSE);
    }

    public static class ANSICodes {
        static final int OFF = 0;
        static final int BOLD = 1;
        static final int UNDERSCORE = 4;
        static final int BLINK = 5;
        static final int REVERSE = 7;
        static final int CONCEALED = 8;
        static final int FG_BLACK = 30;
        static final int FG_RED = 31;
        static final int FG_GREEN = 32;
        static final int FG_YELLOW = 33;
        static final int FG_BLUE = 34;
        static final int FG_MAGENTA = 35;
        static final int FG_CYAN = 36;
        static final int FG_WHITE = 37;
        static final char ESC = 27;

        /**
         *  Constructor is private since this is a utility class.
         */
        private ANSICodes() {
        }

        /**
          * Sets the screen mode. The mode will be one of the following values:
          * <pre>
          * mode     description
          * ----------------------------------------
          *   0      40 x 148 x 25 monochrome (text)
          *   1      40 x 148 x 25 color (text)
          *   2      80 x 148 x 25 monochrome (text)
          *   3      80 x 148 x 25 color (text)
          *   4      320 x 148 x 200 4-color (graphics)
          *   5      320 x 148 x 200 monochrome (graphics)
          *   6      640 x 148 x 200 monochrome (graphics)
          *   7      Enables line wrapping
          *  13      320 x 148 x 200 color (graphics)
          *  14      640 x 148 x 200 color (16-color graphics)
          *  15      640 x 148 x 350 monochrome (2-color graphics)
          *  16      640 x 148 x 350 color (16-color graphics)
          *  17      640 x 148 x 480 monochrome (2-color graphics)
          *  18      640 x 148 x 480 color (16-color graphics)
          *  19      320 x 148 x 200 color (256-color graphics)
          * </pre>
          */
        public static String setmode(final int mode) {
            return ESC + "[=" + mode + "h";
        }

        /**
          * Same as setmode () except for mode = 7, which disables line
          * wrapping (useful for writing the right-most column without
          * scrolling to the next line).
          */
        public static String resetmode(final int mode) {
            return ESC + "[=" + mode + "l";
        }

        /**
          * Clears the screen and moves the cursor to the home postition.
          */
        public static String clrscr() {
            return ESC + "[2J";
        }

        /**
          * Removes all characters from the current cursor position until
          * the end of the line.
          */
        public static String clreol() {
            return ESC + "[K";
        }

        /**
          * Moves the cursor n positions to the left. If n is greater or
          * equal to the current cursor column, the cursor is moved to the
          * first column.
          */
        public static String left(final int n) {
            return ESC + "[" + n + "D";
        }

        /**
          * Moves the cursor n positions to the right. If n plus the current
          * cursor column is greater than the rightmost column, the cursor
          * is moved to the rightmost column.
          */
        public static String right(final int n) {
            return ESC + "[" + n + "C";
        }

        /**
          * Moves the cursor n rows up without changing the current column.
          * If n is greater than or equal to the current row, the cursor is
          * placed in the first row.
          */
        public static String up(final int n) {
            return ESC + "[" + n + "A";
        }

        /**
          * Moves the cursor n rows down. If n plus the current row is greater
          * than the bottom row, the cursor is moved to the bottom row.
          */
        public static String down(final int n) {
            return ESC + "[" + n + "B";
        }

        /*
          * Moves the cursor to the given row and column. (1,1) represents
          * the upper left corner. The lower right corner of a usual DOS
          * screen is (25, 80).
          */
        public static String gotoxy(final int row, final int column) {
            return ESC + "[" + row + ";" + column + "H";
        }

        /**
          * Saves the current cursor position.
          */
        public static String save() {
            return ESC + "[s";
        }

        /**
          * Restores the saved cursor position.
          */
        public static String restore() {
            return ESC + "[u";
        }

        /**
          * Sets the character attribute. It will be
         * one of the following character attributes:
          *
          * <pre>
          * Text attributes
          *    0    All attributes off
          *    1    Bold on
          *    4    Underscore (on monochrome display adapter only)
          *    5    Blink on
          *    7    Reverse video on
          *    8    Concealed on
          *
          *   Foreground colors
          *    30    Black
          *    31    Red
          *    32    Green
          *    33    Yellow
          *    34    Blue
          *    35    Magenta
          *    36    Cyan
          *    37    White
          *
          *   Background colors
          *    40    Black
          *    41    Red
          *    42    Green
          *    43    Yellow
          *    44    Blue
          *    45    Magenta
          *    46    Cyan
          *    47    White
          * </pre>
          *
          * The attributes remain in effect until the next attribute command
          * is sent.
          */
        public static String attrib(final int attr) {
            return ESC + "[" + attr + "m";
        }

        /**
          * Sets the key with the given code to the given value. code must be
          * derived from the following table, value must
         * be any semicolon-separated
          * combination of String (enclosed in double quotes) and numeric values.
          * For example, to set F1 to the String "Hello F1", followed by a CRLF
          * sequence, one can use: ANSI.setkey ("0;59", "\"Hello F1\";13;10").
          * Heres's the table of key values:
          * <pre>
          * Key                       Code      SHIFT+code  CTRL+code  ALT+code
          * ---------------------------------------------------------------
          * F1                        0;59      0;84        0;94       0;104
          * F2                        0;60      0;85        0;95       0;105
          * F3                        0;61      0;86        0;96       0;106
          * F4                        0;62      0;87        0;97       0;107
          * F5                        0;63      0;88        0;98       0;108
          * F6                        0;64      0;89        0;99       0;109
          * F7                        0;65      0;90        0;100      0;110
          * F8                        0;66      0;91        0;101      0;111
          * F9                        0;67      0;92        0;102      0;112
          * F10                       0;68      0;93        0;103      0;113
          * F11                       0;133     0;135       0;137      0;139
          * F12                       0;134     0;136       0;138      0;140
          * HOME (num keypad)         0;71      55          0;119      --
          * UP ARROW (num keypad)     0;72      56          (0;141)    --
          * PAGE UP (num keypad)      0;73      57          0;132      --
          * LEFT ARROW (num keypad)   0;75      52          0;115      --
          * RIGHT ARROW (num keypad)  0;77      54          0;116      --
          * END (num keypad)          0;79      49          0;117      --
          * DOWN ARROW (num keypad)   0;80      50          (0;145)    --
          * PAGE DOWN (num keypad)    0;81      51          0;118      --
          * INSERT (num keypad)       0;82      48          (0;146)    --
          * DELETE  (num keypad)      0;83      46          (0;147)    --
          * HOME                      (224;71)  (224;71)    (224;119)  (224;151)
          * UP ARROW                  (224;72)  (224;72)    (224;141)  (224;152)
          * PAGE UP                   (224;73)  (224;73)    (224;132)  (224;153)
          * LEFT ARROW                (224;75)  (224;75)    (224;115)  (224;155)
          * RIGHT ARROW               (224;77)  (224;77)    (224;116)  (224;157)
          * END                       (224;79)  (224;79)    (224;117)  (224;159)
          * DOWN ARROW                (224;80)  (224;80)    (224;145)  (224;154)
          * PAGE DOWN                 (224;81)  (224;81)    (224;118)  (224;161)
          * INSERT                    (224;82)  (224;82)    (224;146)  (224;162)
          * DELETE                    (224;83)  (224;83)    (224;147)  (224;163)
          * PRINT SCREEN              --        --          0;114      --
          * PAUSE/BREAK               --        --          0;0        --
          * BACKSPACE                 8         8           127        (0)
          * ENTER                     13        --          10         (0
          * TAB                       9         0;15        (0;148)    (0;165)
          * NULL                      0;3       --          --         --
          * A                         97        65          1          0;30
          * B                         98        66          2          0;48
          * C                         99        66          3          0;46
          * D                         100       68          4          0;32
          * E                         101       69          5          0;18
          * F                         102       70          6          0;33
          * G                         103       71          7          0;34
          * H                         104       72          8          0;35
          * I                         105       73          9          0;23
          * J                         106       74          10         0;36
          * K                         107       75          11         0;37
          * L                         108       76          12         0;38
          * M                         109       77          13         0;50
          * N                         110       78          14         0;49
          * O                         111       79          15         0;24
          * P                         112       80          16         0;25
          * Q                         113       81          17         0;16
          * R                         114       82          18         0;19
          * S                         115       83          19         0;31
          * T                         116       84          20         0;20
          * U                         117       85          21         0;22
          * V                         118       86          22         0;47
          * W                         119       87          23         0;17
          * X                         120       88          24         0;45
          * Y                         121       89          25         0;21
          * Z                         122       90          26         0;44
          * 1                         49        33          --         0;120
          * 2                         50        64          0          0;121
          * 3                         51        35          --         0;122
          * 4                         52        36          --         0;123
          * 5                         53        37          --         0;124
          * 6                         54        94          30         0;125
          * 7                         55        38          --         0;126
          * 8                         56        42          --         0;126
          * 9                         57        40          --         0;127
          * 0                         48        41          --         0;129
          * -                         45        95          31         0;130
          * =                         61        43          ---        0;131
          * [                         91        123         27         0;26
          * ]                         93        125         29         0;27
          *                           92        124         28         0;43
          * ;                         59        58          --         0;39
          * '                         39        34          --         0;40
          * ,                         44        60          --         0;51
          * .                         46        62          --         0;52
          * /                         47        63          --         0;53
          * `                         96        126         --         (0;41)
          * ENTER (keypad)            13        --          10         (0;166)
          * / (keypad)                47        47          (0;142)    (0;74)
          * * (keypad)                42        (0;144)     (0;78)     --
          * - (keypad)                45        45          (0;149)    (0;164)
          * + (keypad)                43        43          (0;150)    (0;55)
          * 5 (keypad)                (0;76)    53          (0;143)    --
          */
        public static String setkey(final String code, final String value) {
            return ESC + "[" + code + ";" + value + "p";
        }
    }

    public static void main(final String[] args) throws Exception {
        // sequence, one can use: ANSI.setkey ("0;59", "\"Hello F1\";13;10").
        BufferedReader reader =
            new BufferedReader(new InputStreamReader(System.in));
        System.out.print(ANSICodes.setkey("97", "97;98;99;13")
                         + ANSICodes.attrib(ANSICodes.OFF));
        System.out.flush();

        String line;

        while ((line = reader.readLine()) != null) {
            System.out.println("GOT: " + line);
        }
    }
}
