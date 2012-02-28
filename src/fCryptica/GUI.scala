package fCryptica

import scala.swing._
import scala.swing.event._
import java.awt.event.{KeyEvent=>JKeyEvent,InputEvent=>JInputEvent}
import java.awt.Toolkit.getDefaultToolkit
import java.io.File
import javax.swing.{KeyStroke,UIManager}
import fCryptica.swing._
import fCryptica.util._

object GUI extends SimpleSwingApplication {
	private val crlf = System.getProperty("line.separator")
	var encKey = 0
	var decKey = 1

	var currentFile: File = null;

	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
	def top = new MainFrame { frame =>

		title = "fCryptica"
		val width = 800
		val height = 600
		val mask = getDefaultToolkit.getMenuShortcutKeyMask
		var url = ""

		/* ------------------------------------------------------------------------ *
		 * Model																	*
		 * ------------------------------------------------------------------------ */
		// ファイルエクスプローラー
		val explorerPane = new ScrollPane(FileExplorer())

		// テキストエリア
		val tabPanel = new BorderPanel {
			import BorderPanel.Position._
			add(new TabbedPane {
				pages += new TabbedPane.Page("鍵生成", KeyGenPanel)
				pages += new TabbedPane.Page("暗号化", EncPanel)
				pages += new TabbedPane.Page("復号", DecPanel)
			}, Center)
		}

		// テキストエリアとコンソール画面の分割
		val hsplitPane = new SplitPane(Orientation.Horizontal, tabPanel, ConsolePanel) {
			continuousLayout = true
			dividerSize = 5
			resizeWeight = 0.8
		}
		// ファイルエクスプローラと上記の分割
		val wsplitPane = new SplitPane(Orientation.Vertical, explorerPane, hsplitPane) {
			continuousLayout = true
			dividerLocation = width / 5
			dividerSize = 5
			resizeWeight = 0.2
		}

		/* ------------------------------------------------------------------------ *
		 * Controller																*
		 * ------------------------------------------------------------------------ */
		// メニューバー
		// ファイル操作
		val newFile = new Action("新規(N)") {def apply() {FileAction.execNewFile();}}
		val openFile = new Action("開く(O)") {def apply() {FileAction.execOpenFile();}}
		val saveFile = new Action("上書き保存(S)") {
			def apply() {
				currentFile match {
					case f: File => FileAction.execSaveFile(currentFile);
					case null => FileAction.execSaveAsFile();
				}
			}
		}
		val saveAsFile = new Action("別名で保存...") {def apply() {FileAction.execSaveAsFile();}}
		val exitAction = new Action("終了(Q)") {
			accelerator = Some(KeyStroke.getKeyStroke(JKeyEvent.VK_Q, mask))
			mnemonic = JKeyEvent.VK_Q
			def apply() { exit }
		}

		// ショートカットボタン
		// テスト実行(鍵生成,暗号化,復号)
		TestAction.init();
		TestAction.reactions += {
			case ButtonClicked(TestAction.execKeyGen) => TestAction.KeyGen();
			case ButtonClicked(TestAction.execEnc) => TestAction.Enc();
			case ButtonClicked(TestAction.execDec) => TestAction.Dec();
		}
		// ファイル出力
		val execOutput = new Action("プログラム出力") {
			accelerator = Some(KeyStroke.getKeyStroke(JKeyEvent.VK_S, (JInputEvent.SHIFT_DOWN_MASK | (mask))))
			mnemonic = JKeyEvent.VK_S
			def apply() {
				OutputAction.run("scala")
			}
		}

		EncPanel.reactions += {
			case ButtonClicked(b) => b.text match {
				case "公開鍵" => encKey = 0
				case "秘密鍵" => encKey = 1
			}
		}
		DecPanel.reactions += {
			case ButtonClicked(b) => b.text match {
				case "公開鍵" => decKey = 0
				case "秘密鍵" => decKey = 1
			}
		}

		/* ------------------------------------------------------------------------ *
		 * View																		*
		 * ------------------------------------------------------------------------ */
		// コンポーネントの配置
		contents = new BorderPanel {
			import BorderPanel.Position._
			maximumSize   = new Dimension(width, height)
			preferredSize = maximumSize
			val topPanel = new BorderPanel {
				add(new BoxPanel(Orientation.Horizontal) {
					contents += new Button(execOutput)
					contents += TestAction.execKeyGen
					contents += TestAction.execEnc
					contents += TestAction.execDec
				}, Center)
			}
			val mainPanel = new BorderPanel {
				add(wsplitPane, Center)
			}
			add(topPanel, North)
			add(mainPanel, Center)
		}

		// メニューバー
		menuBar = new MenuBar {
			contents += new Menu("fCryptica") {
				contents += new MenuItem(exitAction)
			}
			contents += new Menu("ファイル (F)") {
				mnemonic = Key.F
				contents += new MenuItem(newFile)
				contents += new MenuItem(openFile)
				contents += new MenuItem(saveFile)
				contents += new MenuItem(saveAsFile)
			}
			contents += new Menu("実行(E)") {
				mnemonic = Key.E
				contents += new MenuItem(Action("コピー(C)") {
					println("test")
				}) {
					mnemonic = Key.C
				}
				contents += new MenuItem(Action("貼付け(P)") {
					println("test")
				}) {
					mnemonic = Key.P
				}
			}
			contents += new Menu("ヘルプ(H)") {
				contents += new MenuItem(title + "について")
			}
		}

		TestAction.close()
	}
}
