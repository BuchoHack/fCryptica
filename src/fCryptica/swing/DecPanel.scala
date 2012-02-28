package fCryptica.swing

import scala.swing._
import java.awt.dnd._

object DecPanel extends BorderPanel {
	import BorderPanel.Position._

	private val group =	new ButtonGroup
	private val buttons = List(new RadioButton("公開鍵"),new RadioButton("秘密鍵"));
	buttons.map(b => group.buttons += b);
	buttons.map(b => listenTo(b));

	val plainText = new TextField
	add(new BoxPanel(Orientation.Horizontal) {
		for (i <- 0 to (group.buttons.toList.length - 1)) contents += group.buttons.toList(i)
	}, North)

	val Algorithm = new TextArea{ tabSize = 4 }
	add(new ScrollPane {
		val target = new DropTarget(Algorithm.self, new TextAreaDropTargetAdapter(Algorithm));
		viewportView = Algorithm
		import ScrollPane.BarPolicy._
		verticalScrollBarPolicy = Always
	}, Center)

	val answerText = new TextField
	add(new BoxPanel(Orientation.Horizontal) {
		contents += new Label("復号文 D = ")
		contents += answerText
	}, South)

	def init() {
		this.buttons.last.selected = true;
		this.plainText.text = "";
		this.Algorithm.text = "";
		this.answerText.text = "";
	}

	def selectKey(i: Byte) {
		if (i==0) {
			this.buttons.first.selected = true;
		} else {
			this.buttons.last.selected = true;
		}
	}
}