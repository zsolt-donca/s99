package com.zsd.s99.bintrees

import java.awt.geom.Ellipse2D
import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}

class TreeVisualizer(val treeNode: Tree[Char]) {

  import TreeOperations._

  val gridSize = 50

  val nodes: Seq[Tree[Any]] = treeNode.inOrder
  val treeNodes: Seq[(TreeNode[Any], Position)] = nodes.collect { case treeNode: TreeNode[Any] if treeNode.hasPosition => (treeNode, treeNode.position.get) }
  val xCoordinates: Seq[Int] = treeNodes.map(_._2.x)
  val minX = xCoordinates.min
  val maxX = xCoordinates.max

  val yCoordinates: Seq[Int] = treeNodes.map(_._2.y)
  val minY = yCoordinates.min
  val maxY = yCoordinates.max

  def paint(g: Graphics2D) = {

    g.setColor(Color.BLACK)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    drawGrid()
    drawLabels()
    drawConnections()
    drawNodes()

    def drawGrid(): Unit = {
      for (x <- minX to maxX) {
        val xPos = x.toScreenX
        g.drawLine(xPos, gridSize, xPos, maxY.toScreenY)
      }
      for (y <- minY to maxY) {
        val yPos = y.toScreenY
        g.drawLine(gridSize, yPos, maxX.toScreenX, yPos)
      }
    }

    def drawLabels(): Unit = {
      for (x <- minX to maxX) {
        val str = x.toString
        val xPos = x.toScreenX - 4
        val yPos = (gridSize * 0.75).asInstanceOf[Int]
        g.drawString(str, xPos, yPos)
      }
      for (y <- minY to maxY) {
        val str = y.toString
        val xPos = (gridSize * 0.60).asInstanceOf[Int]
        val yPos = y.toScreenY + 4
        g.drawString(str, xPos, yPos)
      }
    }

    def drawConnections(): Unit = {
      def drawConnections(node: Tree[_], parent: Option[Tree[_]]): Unit = {
        node match {
          case NodeWithPosition(_, left, right, x, y) =>
            parent match {
              case Some(NodeWithPosition(_, _, _, px, py)) =>
                g.drawLine(x.toScreenX, y.toScreenY, px.toScreenX, py.toScreenY)
              case _ =>
            }
            drawConnections(left, Some(node))
            drawConnections(right, Some(node))
          case _ =>
        }
      }
      g.setStroke(new BasicStroke(2))
      drawConnections(treeNode, None)
    }

    def drawNodes(): Unit = {
      g.setStroke(new BasicStroke(1))
      for ((node, position) <- treeNodes) {
        val xPos = position.x.toScreenX
        val yPos = position.y.toScreenY
        val radius = 16
        val halfRadius = radius / 2

        val circle = new Ellipse2D.Double(xPos - halfRadius, yPos - halfRadius, radius, radius)
        g.setColor(Color.BLACK)
        g.fill(circle)

        val str = node.value.toString
        g.setColor(Color.WHITE)
        g.drawString(str, xPos - 4, yPos + 4)
      }
    }
  }

  implicit class ToScreenCoordinates(val value: Int) {
    val toScreenX = (value - minX + 1) * gridSize
    val toScreenY = (value - minY + 1) * gridSize
  }

}
