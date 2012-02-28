package fCryptica.swing

import scala.swing.{Button,Publisher}
import scala.tools.nsc.Settings
import java.io.File
import fCryptica.GUI
import fCryptica.util.MyIMain

object TestAction extends Publisher {
	private var settings: Settings = new Settings;
	settings.usejavacp.value = true
	var intp: MyIMain = null;

	var PK: Array[String] = null;
	var SK: Array[String] = null;

	private val crlf = System.getProperty("line.separator");

	val execKeyGen = new Button { text = "鍵生成テスト" };
	val execEnc = new Button { text = "暗号化テスト" };
	val execDec = new Button { text = "復号テスト" };

	listenTo(execKeyGen);
	listenTo(execEnc);
	listenTo(execDec);

	def init() {
		val bootClassPath = jarPathOfClass("fCryptica.GUI")
//		val bootClassPath = List("/Users/naoki/Documents/workspace/Test/fCryptica.jar")
		settings.classpath.append(bootClassPath mkString (File.pathSeparator))

		intp = new MyIMain(settings);
		intp.quietImport("scala.util.Random")
		intp.quietImport("fCryptica.math._")
	}

	def close() {
		intp.close()
	}

	def setStreamWriter() {
	}

	def KeyGen() {
		PK = KeyGenPanel.pkText.text.split(",");
		SK = KeyGenPanel.skText.text.split(",");

		val KeyGenClass = "object KeyGen {" + crlf + KeyGenPanel.Algorithm.text + crlf + "}";
		intp.interpret(KeyGenClass);
//		intp.interpret(KeyGenPanel.textArea.text);

		// 公開鍵の表示
		ConsolePanel.textArea.text += "[公開鍵]" + crlf;
		if (KeyGenPanel.pkText.text.isEmpty()==false) {
			for (i<-PK) {
//				intp.valueOfTerm(i);
				intp.interpret("null");
				intp.interpret("KeyGen."+i);
				val r = intp.lastRequest;
				ConsolePanel.textArea.text += i + " = " + r.lineRep.call("$result") + crlf;
			}
		} else {
			ConsolePanel.textArea.text += "公開鍵が指定されていません" + crlf;
		}

		// 秘密鍵の表示
		ConsolePanel.textArea.text += "[秘密鍵]" + crlf;
		if (KeyGenPanel.skText.text.isEmpty()==false) {
			for (i<-SK) {
//				intp.valueOfTerm(i);
				intp.interpret("null");
				intp.interpret("KeyGen."+i);
				val r = intp.lastRequest;
				ConsolePanel.textArea.text += i + " = " + r.lineRep.call("$result") + crlf;
			}
		} else {
			ConsolePanel.textArea.text += "秘密鍵が指定されていません" + crlf;
		}
	}

	def Enc() {
		ConsolePanel.textArea.text += "[平文]" + crlf;
		if (EncPanel.plainText.text.isEmpty()==false) {
			ConsolePanel.textArea.text += "M = " + EncPanel.plainText.text + crlf;
			var EncClass = "object Enc {" + crlf;
			EncClass += "val M = " + EncPanel.plainText.text + crlf;
			ConsolePanel.textArea.text += "[暗号化に利用する鍵]" + crlf;
			if (GUI.encKey==0) {
				ConsolePanel.textArea.text += "公開鍵" + crlf;
				for (i<-PK) {
					EncClass += "val "+ i + " = " + "KeyGen." + i + crlf;
				}
			} else if (GUI.encKey==1) {
				ConsolePanel.textArea.text += "秘密鍵" + crlf;
				for (i<-SK) {
					EncClass += "val "+ i + " = " + "KeyGen." + i + crlf;
				}
			} else {
				ConsolePanel.textArea.text += "暗号化に利用する鍵が指定されていません" + crlf;
			}
			EncClass += EncPanel.Algorithm.text + crlf;
			EncClass += "val C = " + EncPanel.cipherText.text + crlf + "}";
			intp.interpret(EncClass);
		} else {
			ConsolePanel.textArea.text += "平文が入力されていません" + crlf;
		}

		ConsolePanel.textArea.text += "[暗号文]" + crlf;
		if (EncPanel.cipherText.text.isEmpty()==false) {
//			intp.valueOfTerm(encPanel.cipherText.text);
			intp.interpret("null"+crlf+"Enc.C");
			val r = intp.lastRequest;
			ConsolePanel.textArea.text += "C = " + r.lineRep.call("$result") + crlf;
		} else {
			ConsolePanel.textArea.text += "暗号文が指定されていません" + crlf;
		}
	}

	def Dec() {
		var DecClass = "object Dec {" + crlf;
		DecClass += "val C = " + "Enc." + EncPanel.cipherText.text + crlf;
		ConsolePanel.textArea.text += "[復号に利用する鍵]" + crlf;
		if (GUI.decKey==0) {
			ConsolePanel.textArea.text += "公開鍵" + crlf;
			for (i<-PK) {
				DecClass += "val "+ i + " = " + "KeyGen." + i + crlf;
			}
		} else if (GUI.decKey==1) {
			ConsolePanel.textArea.text += "秘密鍵" + crlf;
			for (i<-SK) {
				DecClass += "val "+ i + " = " + "KeyGen." + i + crlf;
			}
		} else {
			ConsolePanel.textArea.text += "復号に利用する鍵が指定されていません" + crlf;
		}
		DecClass += DecPanel.Algorithm.text + crlf;
		DecClass += "val D = " + DecPanel.answerText.text + crlf + "}";
		intp.interpret(DecClass);

		ConsolePanel.textArea.text += "[復号文]" + crlf;
		if (DecPanel.answerText.text.isEmpty()==false) {
//			intp.valueOfTerm(DecPanel.decText.text);
			intp.interpret("null"+crlf+"Dec.D");
			val r = intp.lastRequest;
			ConsolePanel.textArea.text += "D = " + r.lineRep.call("$result") + crlf;
		} else {
			ConsolePanel.textArea.text += "復号文が指定されていません" + crlf;
		}
	}

	private def jarPathOfClass(className: String): List[String] = {
		val resource = className.split('.').mkString("/", "/", ".class")
		val path = getClass.getResource(resource).getPath
		val indexOfFileScheme = path.indexOf("file:") + 5
		val indexOfSeparator = path.lastIndexOf('!')
		List(path.substring(indexOfFileScheme, indexOfSeparator))
	}
}
