package fCryptica.util

import scala.io.Source
import scala.util.Random

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.VirtualDirectory
//import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.BatchSourceFile
import java.io.{ File, OutputStream }

class DynamicCompiler(outputstream: OutputStream) {
	def this() = this(Console.err)
	private val SOURCE_ENCODING: String = "UTF-8"

	private val virtualDirectory: VirtualDirectory = new VirtualDirectory("[memory]", None)

//	private val bootClassPath: List[String] = List("/Users/naoki/Documents/workspace/Test/fCryptica.jar")
	private val bootClassPath: List[String] = jarPathOfClass("fCryptica.GUI")

	private val settings: Settings = new Settings
	settings.deprecation.value = true						// 非推奨の警告を有効に
	settings.unchecked.value = true							// unchecked警告を有効に
	settings.outputDirs.setSingleOutput(virtualDirectory)	// 結果の出力先はメモリ上
	settings.bootclasspath.value = bootClassPath mkString (File.pathSeparator)
	settings.classpath.value = bootClassPath mkString (File.pathSeparator)

	private val global: Global =
		new Global(settings, new DynamicConsoleReporter(settings, outputstream))	// Reporterはコンソール上に出力

	private val classLoader: AbstractFileClassLoader =
		new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)
		// rootをメモリ上に、このクラスを読み込んだClassLoaderを親ClassLoaderに設定

	def compileClassFromFile(sourcePath: String, className: String): Option[Class[_]] = {
		val source = Source.fromFile(sourcePath, SOURCE_ENCODING)
		try {
			compileClass(className, source.mkString, sourcePath)
		} finally {
			if (source != null) source.close()
		}
	}

	def compileClass(className: String, source: String, sourcePath: String = "[dynamic compiler]"): Option[Class[_]] =
		try {
			val compiler = new global.Run
			compiler.compileSources(List(new BatchSourceFile(sourcePath, source)))
			Some(classLoader.findClass(className))
		} catch {
			case th: Throwable =>
				th.printStackTrace()
				None
		}

	def runScriptFromFile(sourcePath: String): Unit = {
		val source = Source.fromFile(sourcePath, SOURCE_ENCODING)
		try {
			runScript(source.mkString)
		} finally {
			if (source != null) source.close()
		}
	}

	def runScript(source: String): Unit =
		try {
			val scriptClassName = wrapScriptClassName
			val wrappedSource = wrapScript(source, scriptClassName)

			compileClass(scriptClassName, wrappedSource) foreach { clazz =>
				clazz.newInstance.asInstanceOf[() => Any].apply()
			}
		} catch {
			case th: Throwable => th.printStackTrace()
		} finally {
			virtualDirectory.clear
		}

	private def wrapScriptClassName: String = {
		val random = new Random
		"WrappedScript_" + random.nextInt(Integer.MAX_VALUE)
	}

	private def wrapScript(code: String, className: String): String = {
		"""|class %s extends (() => Any) {
		   |	def apply() = {
		   |		%s
		   |	}
		   |}
		   |""".stripMargin.format(className, code)
	}

	private def jarPathOfClass(className: String): List[String] = {
		val resource = className.split('.').mkString("/", "/", ".class")
		val path = getClass.getResource(resource).getPath
		val indexOfFileScheme = path.indexOf("file:") + 5
		val indexOfSeparator = path.lastIndexOf('!')
		List(path.substring(indexOfFileScheme, indexOfSeparator))
	}
}
