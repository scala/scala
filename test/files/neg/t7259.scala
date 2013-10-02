@xxxxx // error: not found: type xxxx
class Ok

//
// This had the wrong error message in 2.9 and 2.10.
//

@annotation.xxxxx // error: not found: type scala
class WrongErrorMessage
