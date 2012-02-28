package fCryptica.swing

import java.awt.dnd._
import java.awt.datatransfer._
import java.io._
import scala.swing._
import fCryptica.GUI

class TextAreaDropTargetAdapter(textArea: TextArea) extends DropTargetAdapter {
	def drop(e: DropTargetDropEvent) = {
		try {
			val transfer = e.getTransferable()
			if (transfer.isDataFlavorSupported(DataFlavor.stringFlavor)) {
				e.acceptDrop(DnDConstants.ACTION_COPY_OR_MOVE)
				val path = transfer.getTransferData(DataFlavor.stringFlavor).toString();
				GUI.currentFile = new File(path)
				import scala.io._
				val src = Source.fromFile(path, "UTF-8")
				textArea.text = src.mkString
			}
		} catch {
			case e: UnsupportedFlavorException	=> e.printStackTrace()
			case e: IOException					=> e.printStackTrace()
		}
	}
}