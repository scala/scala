class Bippy extends AnyVal // fail

@inline class NotOkDingus2 extends Immutable with AnyVal // fail

@inline object NotOkDingus3 extends AnyVal // fail

class NotOkBippy1 extends Bippy // fail

class NotOkBippy2 extends Bippy with Immutable //fail

@inline class NotOkBippy3 extends Bippy with Immutable //fail


@inline class OkBippy extends AnyVal // ok 
