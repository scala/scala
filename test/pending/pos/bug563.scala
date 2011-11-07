object Test {
    def map[A,R](a : List[A], f : A => R) : List[R] = a.map(f);
    
    def split(sn : Iterable[List[Cell[int]]]) : unit =
        for (val n <- sn)
            map(n,ptr => new Cell(ptr.elem));
}
