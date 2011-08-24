object ParserBug {
  1 match {
          case 1 =>
            <div>
                { 1 match { case 1 => "1"; case 2 => "2" }
            </div>
          case 2 =>
            <div/>
        }
}