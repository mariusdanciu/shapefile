package net.shapefile

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Paths

import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event._
import scala.util.Try

object SimpleGUI extends SimpleSwingApplication {

  def loadShape = {
    val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get("./data/Judete.shp")))
    ShapeFile.parse(data)
  }

  def top = {
    new MainFrame {
      val map = new MapCanvas(loadShape) {
        preferredSize = new Dimension(1400, 800)
      }
      title = "First Swing App"
      contents = map
    }
  }
}

class MapCanvas(shapeFile: => Try[ShapeFile]) extends Panel {

  var xFactor = 800
  var yFactor = 800

  var startPoint = (0, 0)
  var endPoint = (0, 0)

  var origin: (Int, Int) = null

  listenTo(mouse.wheel, mouse.clicks, mouse.moves)

  reactions += {
    case m: MouseWheelMoved =>
      if (m.rotation > 0) {
        xFactor += 50
        yFactor += 50
      } else if (xFactor > 50) {
        xFactor -= 50
        yFactor -= 50
      }
      repaint()
    case MouseDragged(src, point, mods) =>
      endPoint = (point.x, point.y)

    case MouseReleased(src, point, mods, clicks, triggers) =>
      repaint()

    case MousePressed(src, point, i1, i2, b) =>
      startPoint = (point.x, point.y)

  }

  def drawPolygon(g: Graphics2D, origin: (Int, Int), polygon: Polygon, box: BoundingBox) {

    val split = polygon.points.zipWithIndex.groupBy {
      case (point, index) =>
        val pos = for { part <- polygon.partsIndex if index >= part } yield {
          part
        }
        pos.last
    }

    for { points <- split.values } {
      points.reduceLeft {
        (p1, p2) =>
          (p1, p2) match {
            case ((Point(x1, y1), idx1), (Point(x2, y2), idx2)) =>
              g.drawLine(origin._1 + (x1 - box.xMin).toInt / xFactor,
                origin._2 - (y1 - box.yMin).toInt / yFactor,
                origin._1 + (x2 - box.xMin).toInt / xFactor,
                origin._2 - (y2 - box.yMin).toInt / yFactor)
              p2
          }
      }
    }
  }

  override def paintComponent(g: Graphics2D) {
    if (origin == null) {
      origin = (0, size.height)
    }
    val deltaX = endPoint._1 - startPoint._1
    val deltaY = endPoint._2 - startPoint._2
    origin = (origin._1 + deltaX, origin._2 + deltaY)

    g.clearRect(0, 0, size.width, size.height)
    g.setColor(Color.black)
    for { ShapeFile(header, shapes) <- shapeFile } {
      shapes.map {
        case IndexedShape(idx, p @ Polygon(box, parts, points)) => drawPolygon(g, origin, p, header.box)
      }
    }
  }
}