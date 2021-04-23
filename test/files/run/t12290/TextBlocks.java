// javaVersion: 15+
class TextBlocks {

    final static String aText = """
      A text
      """;

    final static String html1 = """
                                <html>
                                    <body>
                                        <p>Hello, world</p>
                                    </body>
                                </html>
                                """;

    // quote characters are unescaped
    final static String query = """
                                SELECT "EMP_ID", "LAST_NAME" FROM "EMPLOYEE_TB"
                                WHERE "CITY" = 'INDIANAPOLIS'
                                ORDER BY "EMP_ID", "LAST_NAME";
                                """;

    // incidental trailing spaces
    final static String html2 = """
                                <html>   
                                    <body>
                                        <p>Hello, world</p>    
                                    </body> 
                                </html>   
                                """;

    // trailing delimiter influences
    final static String html3 = """
                                <html>
                                    <body>
                                        <p>Hello, world</p>
                                    </body>
                                </html>
    """;

    // blank line does not affect 
    final static String html4 = """
                                <html>
                                    <body>
                                        <p>Hello, world</p>
                                    </body>

                                </html>
                                    """;

    // escape sequences
    final static String html5 = """
                                <html>\n
                                    <body>\
                                        <p>Hello\s,\tworld</p>
                                    </body>
                                </html>
                                """;

    // mixed indentation
		final static String mixedIndents = """
				\s  this line has 4 tabs before it
     this line has 5 spaces before it and space after it \u0020 \u000C\u0020 \u001E
  		 this line has 2 tabs and 3 spaces before it
\u0020 \u000C\u0020 \u001E this line has 6 spaces before it
								""";

    final static String code =
        """
        String text = \"""
            A text block inside a text block
        \""";
        """;

    final static String simpleString = "foo\tbar\nbaz";

    final static String emptyString = "";
}
