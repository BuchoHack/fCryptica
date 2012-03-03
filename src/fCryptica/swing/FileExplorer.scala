package fCryptica.swing

import java.io._
import javax.swing._
import javax.swing.tree._
import javax.swing.TransferHandler._
import java.awt.datatransfer.{StringSelection,Transferable}

object FileExplorer {
	val home_directory = System.getProperty("user.home")
	val current_directory = System.getProperty("user.dir")

	def apply(): Tree = {
//		val tree = new Tree(new DefaultTreeModel((new FileExplorer()).MakeRoot(home_directory)))
		val tree = new Tree(new DefaultTreeModel((new FileExplorer()).MakeRoot(current_directory)))
		tree.peer.setTransferHandler(new TreePathTransferHandler())
		tree.peer.setDragEnabled(true)
		return tree
	}
	def apply(str: String): Tree = {
		val tree = new Tree(new DefaultTreeModel((new FileExplorer()).MakeRoot(str)))
		tree.peer.setTransferHandler(new TreePathTransferHandler())
		tree.peer.setDragEnabled(true)
		return tree
	}
}

class FileExplorer {
	private def MakeRoot(rootdir: String): DefaultMutableTreeNode = {
		val RN = new DefaultMutableTreeNode(rootdir)
		val RootDir = (new File(rootdir)).listFiles()
		for (i <- 0 to RootDir.length - 1) {
			if (RootDir(i).isDirectory()) RN.add(SearchDir(RootDir(i)));
			else RN.add(new DefaultMutableTreeNode(RootDir(i).getName()));
		}
		return(RN);
	}

	private def SearchDir(dir: File):DefaultMutableTreeNode = {
		val TN = new DefaultMutableTreeNode(dir.getName());
		val Dir = dir.listFiles()
		for (i <- 0 to Dir.length - 1) {
			if (Dir(i).isDirectory()) TN.add(SearchDir(Dir(i)));
			else TN.add(new DefaultMutableTreeNode(Dir(i).getName()));
		}
		return(TN);
	}
}

class TreePathTransferHandler extends TransferHandler {
	override def getSourceActions(c:JComponent): Int = {
		// return NONE;
		// return MOVE;
		return COPY;
		// return COPY_OR_MOVE;
		// return COPY_OR_MOVE | LINK;
	}

	override protected def createTransferable(c: JComponent): Transferable = {
		if(!(c.isInstanceOf[JTree])) {
			return null;
		}
		val tree = c.asInstanceOf[JTree];
		val path = tree.getSelectionPath();
		return new StringSelection(path.getPath.mkString("/")); //Transferableを実装しているクラス
	}
}