package com.zsd

import java.awt.{Color, Dimension, Graphics2D}

import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

abstract class TreeVisualizingApp extends SimpleSwingApplication {

  val title: String

  val treeWithLayout: Tree[Char]

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(700, 500)

    val visualizer = new TreeVisualizer(treeWithLayout)

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
