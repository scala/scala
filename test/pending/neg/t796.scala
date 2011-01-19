case class CaseClass( value: Int );

object PatternMatchBug {
    def matcher( a: AnyRef, b: Any ) {
        (a, b) match {
            case ( instance: CaseClass, instance.value ) =>
                System.out.println( "Match succeeded!" );
            case _ =>
                System.out.println( "Match failed!" );
        }
    }

    def main( args : Array[String] ) {
        val caseClassInstance = CaseClass( 42 )

        matcher( caseClassInstance, 13 )
        matcher( caseClassInstance, 42 )
    }
}

