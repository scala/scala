package ch.epfl.lamp.util;

interface AbstractSourceFile {
    /** Returns the name of this source file. */
    String name() ;

    /** Returns the short name (name without path) of this source file. */
    String getShortName() ;

    /**
     * Returns an instance of Position representing the given line and
     * column of this source file.
     */
    Position getPosition( int line, int column ) ;

    /** Returns the content of the given line. */
    String getLine(int line) ;

    /** Returns the name of this source file. */
    String toString() ;

}
