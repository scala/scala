package xsbtpamflet

import xsbti.ConsoleResult

case class ConsoleResponse(result: ConsoleResult, output: String) extends xsbti.ConsoleResponse
