package net.shapefile.swing

import scala.swing.event.MousePressed
import net.shapefile.BoundingBox
import scala.swing.event.MouseDragged
import scala.swing.event.MouseReleased
import scala.swing.event.MouseWheelMoved
import java.awt.Graphics2D
import net.shapefile.Polygon
import scala.swing.Panel
import scala.util.Try
import net.shapefile.ShapeFile
import java.awt.Color
import net.shapefile.Point
import net.shapefile.IndexedShape

class MapCanvas(shapeFile: => Try[ShapeFile]) extends Panel {

  var xFactor: Double = 1000
  var yFactor: Double = 1000

  var startPoint = (0, 0)
  var endPoint = (0, 0)

  var origin: (Int, Int) = null

  listenTo(mouse.wheel, mouse.clicks, mouse.moves)

  reactions += {
    case m: MouseWheelMoved =>
      if (m.rotation > 0) {
        xFactor += 20
        yFactor += 20
      } else if (xFactor > 20) {
        xFactor -= 20
        yFactor -= 20
      }
      repaint()
    case MouseDragged(src, point, mods) =>
      val deltaX = point.x - endPoint._1
      val deltaY = point.y - endPoint._2
      origin = (origin._1 + deltaX, origin._2 + deltaY)
      endPoint = (point.x, point.y)
      repaint

    case MouseReleased(src, point, mods, clicks, triggers) =>

    case MousePressed(src, point, i1, i2, b) =>
      endPoint = (point.x, point.y)

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
              g.drawLine((origin._1 + (x1 - box.xMin) / xFactor) toInt,
                (origin._2 - (y1 - box.yMin) / yFactor) toInt,
                (origin._1 + (x2 - box.xMin) / xFactor) toInt,
                (origin._2 - (y2 - box.yMin) / yFactor) toInt)
              p2
          }
      }
    }
  }

  override def paintComponent(g: Graphics2D) {
    if (origin == null) {
      origin = (0, size.height)
    }
    g.clearRect(0, 0, size.width, size.height)
    g.setColor(Color.black)
    for { ShapeFile(header, shapes) <- shapeFile } {
      shapes.map {
        case IndexedShape(idx, p @ Polygon(box, parts, points)) => drawPolygon(g, origin, p, header.box)
        case s => println(s)
      }
    }
  }
}