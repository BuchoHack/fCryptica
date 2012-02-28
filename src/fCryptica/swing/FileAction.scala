package fCryptica.swing

import scala.swing.FileChooser.Result.Approve
import scala.swing.{Dialog,FileChooser}
import scala.xml.{XML,PrettyPrinter}
import java.io.{File,IOException,FileOutputStream,OutputStreamWriter,BufferedWriter}
import javax.swing.filechooser.FileNameExtensionFilter
import org.apache.commons.lang3.SystemUtils
import org.apache.commons.io.FileUtils
import fCryptica.GUI

object FileAction {
	private val crlf = System.getProperty("line.separator");

	private def getFileChooser = {
		val dir = if (GUI.currentFile == null) SystemUtils.getUserDir else GUI.currentFile;
		new FileChooser(dir) {
			fileFilter = new FileNameExtensionFilter("XMLファイル", "xml");
		}
	}

	def execNewFile() {
		GUI.currentFile = null;
		KeyGenPanel.init();
		EncPanel.init();
		DecPanel.init();
	}

	def execOpenFile() {
		val fc = getFileChooser;
		fc.title = "ファイルを開く";
		fc.showOpenDialog(null) match {
			case FileChooser.Result.Approve =>
				try {
					if (fc.selectedFile.exists) {
						val xml = XML.loadFile(fc.selectedFile);
						// KeyGen
						for (bs <- xml \ "KeyGen") {
							for (<Algorithm>{ i }</Algorithm> <- bs.child) {
								KeyGenPanel.Algorithm.text = i.toString();
							}
							for (<PublicKey>{ i }</PublicKey> <- bs.child) {
								KeyGenPanel.pkText.text = i.toString();
							}
							for (<PrivateKey>{ i }</PrivateKey> <- bs.child) {
								KeyGenPanel.skText.text = i.toString();
							}
						}
						// Enc
						for (bs <- xml \ "Enc") {
							for (<SelectedKey>{ i }</SelectedKey> <- bs.child) {
								if(i.toString()==0.toString()) {
									EncPanel.selectKey(0);
								} else if (i==1.toString()) {
									EncPanel.selectKey(1);
								}
							}
							for (<PlainText>{ i }</PlainText> <- bs.child) {
								EncPanel.plainText.text = i.toString();
							}
							for (<Algorithm>{ i }</Algorithm> <- bs.child) {
								EncPanel.Algorithm.text = i.toString();
							}
							for (<CipherText>{ i }</CipherText> <- bs.child) {
								EncPanel.cipherText.text = i.toString();
							}
						}
						// Dec
						for (bs <- xml \ "Dec") {
							for (<SelectedKey>{ i }</SelectedKey> <- bs.child) {
								if(i.toString()==0.toString()) {
									DecPanel.selectKey(0);
								} else if (i.toString()==1.toString()) {
									DecPanel.selectKey(1);
								}
							}
							for (<Algorithm>{ i }</Algorithm> <- bs.child) {
								DecPanel.Algorithm.text = i.toString();
							}
							for (<DecodedText>{ i }</DecodedText> <- bs.child) {
								DecPanel.answerText.text = i.toString();
							}
						}

						GUI.currentFile = fc.selectedFile;
					} else {
						execNewFile();
					}
				} catch {
					case e: IOException => Dialog.showMessage(null, "ファイルを開けませんでした。\n" + e.getMessage);
				}
			case FileChooser.Result.Cancel => ;
			case FileChooser.Result.Error => ;
		}
	}

	def execSaveFile(f: File) {
		try {
			scala.xml.XML.save(f.getName(),Format.toXML())
		} catch {
			case e: IOException => Dialog.showMessage(null, "ファイルを保存できませんでした。\n" + e.getMessage)
		}
	}

	def execSaveAsFile() {
		var file: File = null;
		val name = "xml";
		val fc = getFileChooser;
		fc.title = "名前を付けて保存";
		fc.showSaveDialog(null) match {
			case FileChooser.Result.Approve =>
				val sf = fc.selectedFile
				if (sf.toString().substring(sf.toString().length() - name.length()).equals(name)) {
					file = sf;
				} else {
					file = new File(fc.selectedFile + "." + name);
				}
				val confResult = if (file.exists) {
					Dialog.showConfirmation(null, file.getName + " は既に存在します。\n上書きしますか？", "名前を付けて保存の確認", Dialog.Options.YesNo);
				} else {
					Dialog.Result.Yes
				}
				if (confResult == Dialog.Result.Yes) {
					try {
						scala.xml.XML.save(file.getName(),Format.toXML())
						GUI.currentFile = file
					} catch {
						case e: IOException => Dialog.showMessage(null, "ファイルを保存できませんでした。\n" + e.getMessage)
					}
				}
			case FileChooser.Result.Cancel => ;
			case FileChooser.Result.Error => ;
		}
	}
}
