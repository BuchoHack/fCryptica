package fCryptica.swing

import scala.swing._
import java.awt.dnd._
import java.io._

object KeyGenPanel extends BorderPanel {
	import BorderPanel.Position._

	val Algorithm = new TextArea{ tabSize = 4 }
	add(new ScrollPane {
		new DropTarget(Algorithm.self, new TextAreaDropTargetAdapter(Algorithm));
		viewportView = Algorithm
		import ScrollPane.BarPolicy._
		verticalScrollBarPolicy = Always
	}, Center)

	val pkText = new TextField
	val skText = new TextField
	add(new BoxPanel(Orientation.Vertical) {
		contents +=
			new BoxPanel(Orientation.Horizontal) {
				contents += new Label("公開鍵")
				contents += pkText
			}
		contents +=
			new BoxPanel(Orientation.Horizontal) {
				contents += new Label("秘密鍵")
				contents += skText
			}
	}, South)

	def init() {
		this.Algorithm.text = "";
		this.pkText.text = "";
		this.skText.text = "";
	}
}
