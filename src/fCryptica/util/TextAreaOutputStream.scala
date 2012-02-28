package fCryptica.util

import java.io._
import javax.swing.SwingUtilities
import scala.swing._

class TextAreaOutputstream(private val textArea: TextArea, private val encode: String) extends OutputStream {
	private val os = new ByteArrayOutputStream();

	/** OutputStream#write(byte[])のオーバーライド */
	@throws(classOf[IOException])
	def write(arg: Int) {
		this.os.write(arg);
	}

	/**
	 * flush()でTextAreaに書き出す
	 */
	@throws(classOf[IOException])
	override def flush() {
		// 文字列のエンコード
		val str = new String(this.os.toByteArray(), this.encode);
		// 実際の書き出し処理
		SwingUtilities.invokeLater(new Runnable() {
			def run() {
				textArea.append(str);
			}
		})
		// 書き出した内容はクリアする
		this.os.reset();
	}
}
