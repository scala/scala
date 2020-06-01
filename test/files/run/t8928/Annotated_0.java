package test;

// This should stay in sync with Annotated_1 to test annotations defined in the same compilation run

@NoArgs_0
@Simple_0(_byte = 1, _char = '2', _short = Simple_0.THREE, _int = 4, _long = 5, _float = 6.7f, _double = 8.9, _string = "ten", _class = Object.class)
@Nested_0(
    inner = @Nested_0.Inner("turkey")
)
@Array_0.Repeated({
        @Array_0({8, 6, 7, 5, 3, 0, Annotated_0.NINE}),
        @Array_0(6)
})
@Enum_0(choice = Enum_0.Enum.ONE)
@Empty_0()
public class Annotated_0 {
    public static final int NINE = 9;
}

