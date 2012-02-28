package fCryptica.util

import java.io.OutputStream

object ScriptSourceRunner {
	def apply(args: Array[String]): Unit = {
		args.headOption match {
		case None =>
			println("""|Required One or more Arguments
				|  Usage: [ScriptSourcePath]*""".stripMargin)
			sys.exit(1)
		case _ =>
			val compiler = new DynamicCompiler
			args foreach (compiler.runScriptFromFile(_))
		}
	}
	def apply(args: Array[String], outputstream: OutputStream): Unit = {
		args.headOption match {
		case None =>
			println("""|Required One or more Arguments
				|  Usage: [ScriptSourcePath]*""".stripMargin)
			sys.exit(1)
		case _ =>
			val compiler = new DynamicCompiler(outputstream)
			args foreach (compiler.runScriptFromFile(_))
		}
	}
}