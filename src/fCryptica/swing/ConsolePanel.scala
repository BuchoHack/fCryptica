package fCryptica.swing

import scala.swing._
import java.awt.dnd._

object ConsolePanel extends BorderPanel {
	import BorderPanel.Position._

	val textArea = new TextArea { editable = false }
	add(new ScrollPane {
		viewportView = textArea
		import ScrollPane.BarPolicy._
		verticalScrollBarPolicy = Always
	}, Center)
}