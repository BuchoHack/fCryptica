package fCryptica.util

import tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings

class MyIMain(settings:Settings) extends IMain(settings:Settings) {
	def lastRequest = prevRequestList.last
}
