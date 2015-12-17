package com.zsd.s99.bintrees

import java.awt.{Color, Dimension, Graphics2D}

import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

abstract class TreeVisualizingApp extends SimpleSwingApplication {

  val title: String

  val treeWithLayout: Tree[Char]

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(700, 500)

    import P65.TreePositioning2
    import TreeOperations._

    val tree = if (treeWithLayout.inOrder.exists { case NodeWithPosition(_, _, _, _, _) => true }) treeWithLayout
    else treeWithLayout.layoutBinaryTree2

    val visualizer = new TreeVisualizer(tree)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)

      visualizer.paint(g)
    }
  }

  override def top = new MainFrame {
    title = TreeVisualizingApp.this.title
    contents = ui
  }
}
