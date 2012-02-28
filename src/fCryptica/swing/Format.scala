package fCryptica.swing

import scala.xml.{XML,PrettyPrinter}
import java.io.{StringReader,BufferedReader}
import fCryptica.GUI

object Format {
	private val crlf = System.getProperty("line.separator");

	def toXML(): scala.xml.Elem = {
		val fCryptica =
<fCryptica>
	<KeyGen>
		<Algorithm>{KeyGenPanel.Algorithm.text}</Algorithm>
		<PublicKey>{KeyGenPanel.pkText.text}</PublicKey>
		<PrivateKey>{KeyGenPanel.skText.text}</PrivateKey>
	</KeyGen>
	<Enc>
		<SelectedKey>{GUI.encKey}</SelectedKey>
		<PlainText>{EncPanel.plainText.text}</PlainText>
		<Algorithm>{EncPanel.Algorithm.text}</Algorithm>
		<CipherText>{EncPanel.cipherText.text}</CipherText>
	</Enc>
	<Dec>
		<SelectedKey>{GUI.decKey}</SelectedKey>
		<Algorithm>{DecPanel.Algorithm.text}</Algorithm>
		<DecodedText>{DecPanel.answerText.text}</DecodedText>
	</Dec>
</fCryptica>
		return fCryptica;
	}

	def Scala(name: String): String = {
		TestAction.KeyGen()
		TestAction.Enc()
		TestAction.Dec()

		var str = "";
		var reader: BufferedReader = null
		val EncText = new StringReader(EncPanel.Algorithm.text);
		val DecText = new StringReader(DecPanel.Algorithm.text);

		str += "import scala.util.Random" + crlf;
		str += "import fCryptica.math._" + crlf;
		str += "import fCryptica.security._" + crlf + crlf;

		str += "class " + name + " {" + crlf;

		// KeyGen
		str += "  def KeyGen():KeyPair = {" + crlf;
		reader = new BufferedReader(new StringReader(KeyGenPanel.Algorithm.text));
		try {
			var line = "";
			while ({ line = reader.readLine(); line ne null }) {
				str += "    " + line + crlf;
			}
		} finally {
			reader.close()
		}
		str += "    return new KeyPair(new PublicKey(" + KeyGenPanel.pkText.text + "), new PrivateKey(" + KeyGenPanel.skText.text + "))" + crlf;
		str += "  }" + crlf + crlf;

		// Enc
		val type_M = {
			TestAction.intp.interpret("Enc.M");
			TestAction.intp.typeOfTerm(TestAction.intp.mostRecentVar).get;
		}
		val type_C = {
			TestAction.intp.interpret("Enc.C");
			TestAction.intp.typeOfTerm(TestAction.intp.mostRecentVar).get;
		}
		if (GUI.encKey==0) {
			str += "  def Enc(key: PublicKey, M: "+ type_M +"): " + type_C + " = {" + crlf;
			for (i<-TestAction.PK) {
				str += "    val " + i + " = key." + i + crlf;
			}
		} else if (GUI.encKey==1) {
			str += "  def Enc(key: PrivateKey, M: "+ type_M + "): " + type_C + " = {" + crlf;
			for (i<-TestAction.SK) {
				str += "    val " + i + " = key." + i + crlf;
			}
		}
		reader = new BufferedReader(new StringReader(EncPanel.Algorithm.text));
		try {
			var line = "";
			while ({ line = reader.readLine(); line ne null }) {
				str += "    " + line + crlf;
			}
		} finally {
			reader.close()
		}
		str += "    return " + EncPanel.cipherText.text + crlf;
		str += "  }" + crlf;

		// Dec
		val type_D = {
			TestAction.intp.interpret("Dec.D");
			TestAction.intp.typeOfTerm(TestAction.intp.mostRecentVar).get;
		}
		if (GUI.decKey==0) {
			str += "  def Dec(key: PublicKey, C: "+ type_C +"): " + type_D + " = {" + crlf;
			for (i<-TestAction.PK) {
				str += "    val " + i + " = key." + i + crlf;
			}
		} else if (GUI.decKey==1) {
			str += "  def Dec(key: PrivateKey, C: "+ type_C + "): " + type_D + " = {" + crlf;
			for (i<-TestAction.SK) {
				str += "    val " + i + " = key." + i + crlf;
			}
		}
		reader = new BufferedReader(new StringReader(EncPanel.Algorithm.text));
		try {
			var line = "";
			while ({ line = reader.readLine(); line ne null }) {
				str += "    " + line + crlf;
			}
		} finally {
			reader.close()
		}
		str += "    return " + DecPanel.answerText.text + crlf;
		str += "  }" + crlf;

		str += "}" + crlf + crlf;

		// 公開鍵クラス
		str += "class PublicKey(";
		for (i<-TestAction.PK) {
			val type_key = {
				TestAction.intp.interpret("KeyGen."+i);
				TestAction.intp.typeOfTerm(TestAction.intp.mostRecentVar).get;
			}
			if (i==TestAction.PK.last) {
				str += "in_" + i + ": " + type_key + ") {" + crlf;
			} else {
				str += "in_" + i + ": " + type_key + ", ";
			}
		}
		for (i<-TestAction.PK) {
			str += "  val " + i + " = in_" + i + crlf;
		}
		str += "}" + crlf + crlf;

		// 秘密鍵クラス
		str += "class PrivateKey(";
		for (i<-TestAction.SK) {
			val type_key = {
				TestAction.intp.interpret("KeyGen."+i);
				TestAction.intp.typeOfTerm(TestAction.intp.mostRecentVar).get;
			}
			if (i==TestAction.SK.last) {
				str += "in_" + i + ": " + type_key + ") {" + crlf;
			} else {
				str += "in_" + i + ": " + type_key + ", ";
			}
		}
		for (i<-TestAction.SK) {
			str += "  val " + i + " = in_" + i + crlf;
		}
		str += "}" + crlf + crlf;

		// 鍵ペアクラス
		str += "class KeyPair(PK: PublicKey, SK: PrivateKey) {" + crlf;
		str += "  def getPK(): PublicKey = PK" + crlf;
		str += "  def getSK(): PrivateKey = SK" + crlf;
		str += "}" + crlf + crlf;

		return str;
	}
}