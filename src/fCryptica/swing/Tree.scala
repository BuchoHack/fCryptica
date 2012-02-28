package fCryptica.swing

import scala.swing.Component
import javax.swing.JTree
import javax.swing.tree.TreeModel
import scala.swing.Scrollable

class Tree(model: TreeModel) extends Component with Scrollable.Wrapper {
	override lazy val peer: JTree = new JTree(model) with SuperMixin
	protected def scrollablePeer = peer
}