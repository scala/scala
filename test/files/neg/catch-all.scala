object CatchAll {
	try { "warn" } catch { case _ => }

	try { "warn" } catch { case x => }

	try { "warn" } catch { case _: RuntimeException => ; case x => }

	try { "okay" } catch { case _: Throwable => }

	try { "okay" } catch { case _: Exception => }

	try { "okay" } catch { case okay: Throwable => }

	try { "okay" } catch { case okay: Exception => }

	try { "okay" } catch { case _ if "".isEmpty => }

	"okay" match { case _ => "" }
}
