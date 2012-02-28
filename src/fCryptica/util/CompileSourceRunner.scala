package fCryptica.util

import java.io.OutputStream

object CompileSourceRunner {
	def apply(args: Array[String]): Unit = {
		args.headOption match {
			case None =>
				println("""|Required One or more Arguments
						|  Usage: [SourcePath]:[ClassName]*""".stripMargin)
				sys.exit(1)
			case _ =>
				val compiler = new DynamicCompiler
				args foreach { pair =>
					val (path, className) = (pair.split(":")(0), pair.split(":")(1))
					compiler.compileClassFromFile(path, className) match {
						case Some(c) =>
							println("Compile Success[%s]".format(c.getName))
							println("---------- Call Default Constructor [%s]----------".format(c.getName))
							c.newInstance
						case None => println("Compile Fail")
					}
				}
		}
	}
	def apply(args: Array[String], outputstream: OutputStream): Unit = {
		args.headOption match {
			case None =>
				println("""|Required One or more Arguments
						|  Usage: [SourcePath]:[ClassName]*""".stripMargin)
				sys.exit(1)
			case _ =>
				val compiler = new DynamicCompiler(outputstream)
				args foreach { pair =>
					val (path, className) = (pair.split(":")(0), pair.split(":")(1))
					compiler.compileClassFromFile(path, className) match {
						case Some(c) =>
							println("Compile Success[%s]".format(c.getName))
							println("---------- Call Default Constructor [%s]----------".format(c.getName))
							c.newInstance
						case None => println("Compile Fail")
					}
				}
		}
	}
}
