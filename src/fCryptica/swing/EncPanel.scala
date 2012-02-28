package fCryptica.swing

import scala.swing._
import java.awt.dnd._
import java.io._

object EncPanel extends BorderPanel with Publisher {
	import BorderPanel.Position._

	private val group =	new ButtonGroup
	private val buttons = List(new RadioButton("公開鍵"),new RadioButton("秘密鍵"));
	buttons.map(b => group.buttons += b);
	buttons.map(b => listenTo(b));

	val plainText = new TextField
	add(new GridPanel(2, 1) {
		contents +=
			new BoxPanel(Orientation.Horizontal) {
			xLayoutAlignment = 0.5
				for (i <- 0 to (group.buttons.toList.length - 1)) contents += group.buttons.toList(i)
			}
		contents +=
			new BoxPanel(Orientation.Horizontal) {
				contents += new Label("平文 M = ")
				contents += plainText
			}
	}, North)

	val Algorithm = new TextArea{ tabSize = 4 }
	add(new ScrollPane {
		val target = new DropTarget(Algorithm.self, new TextAreaDropTargetAdapter(Algorithm));
		viewportView = Algorithm
		import ScrollPane.BarPolicy._
		verticalScrollBarPolicy = Always
	}, Center)

	val cipherText = new TextField
	add(new BoxPanel(Orientation.Horizontal) {
		contents += new Label("暗号文 C = ")
		contents += cipherText
	}, South)

	def init() {
		this.buttons.first.selected = true;
		this.plainText.text = "";
		this.Algorithm.text = "";
		this.cipherText.text = "";
	}

	def selectKey(i: Int) {
		if (i==0) {
			this.buttons.first.selected = true;
		} else {
			this.buttons.last.selected = true;
		}
	}
}
