package fCryptica.swing

import scala.swing.FileChooser.Result.Approve
import scala.swing.{Dialog,FileChooser}
import java.io.{File,IOException}
import javax.swing.filechooser.FileNameExtensionFilter
import org.apache.commons.lang3.SystemUtils
import org.apache.commons.io.FileUtils
import fCryptica.GUI

object OutputAction {
	private def getFileChooser(name: String) = {
		val dir = if (GUI.currentFile == null) SystemUtils.getUserDir else GUI.currentFile;
		new FileChooser(dir) {
			fileFilter = new FileNameExtensionFilter(name + "ファイル", name);
		}
	}

	def run(name: String) {
		var file: File = null;
		val fc = getFileChooser(name);
		fc.title = name + "プログラムの出力";
		fc.showDialog(null, "出力") match {
			case FileChooser.Result.Approve =>
				val sf = fc.selectedFile;
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
						if (name=="scala") {
							FileUtils.writeStringToFile(file, Format.Scala(file.getName()));
						}
					} catch {
						case e: IOException => Dialog.showMessage(null, "ファイルを保存できませんでした。\n" + e.getMessage)
					}
				}
			case FileChooser.Result.Cancel => ;
			case FileChooser.Result.Error => ;
		}
	}
}