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
import scala.util.Random

class MapCanvas(shapeFile: ShapeFile) extends Panel {

  var dragPoint = (0, 0)
  var origin: (Int, Int) = null
  var scaleRatio = 0.0
  var scaleStep = 0.0

  listenTo(mouse.wheel, mouse.clicks, mouse.moves)

  reactions += {
    case m: MouseWheelMoved =>
      if (m.rotation >= 0) {
        scaleRatio += scaleStep
      } else if (scaleRatio > scaleStep) {
        scaleRatio -= scaleStep
      }
      repaint

    case MouseDragged(src, point, mods) =>
      val deltaX = point.x - dragPoint._1
      val deltaY = point.y - dragPoint._2
      origin = (origin._1 + deltaX, origin._2 + deltaY)
      dragPoint = (point.x, point.y)
      repaint

    case MouseReleased(src, point, mods, clicks, triggers) =>

    case MousePressed(src, point, i1, i2, b) =>
      dragPoint = (point.x, point.y)

  }

  def drawPolygon(g: Graphics2D, polygon: Polygon) {

    val split = polygon.points.zipWithIndex.groupBy {
      case (point, index) =>
        val pos = for { part <- polygon.partsIndex if index >= part } yield {
          part
        }
        pos.last
    }
    val box = shapeFile.header.box

    for { points <- split.values } {

      val (xl, yl) = ((Nil: List[Int], Nil: List[Int]) /: points) {
        case ((xs, ys), (p, _)) =>
          (xs ++ List((origin._1 + (p.x - box.xMin) / scaleRatio).toInt),
            ys ++ List((origin._2 - (p.y - box.yMin) / scaleRatio).toInt))
      }
      val (xs, ys) = (xl toArray, yl toArray)
      val color = new Color(Random.nextInt(255), Random.nextInt(255), Random.nextInt(255))

      g.setColor(color)
      g.fillPolygon(xs, ys, xs.length)
      g.setColor(Color.black)
      g.drawPolygon(xs, ys, xs.length)
    }
  }

  override def paintComponent(g: Graphics2D) {
    if (origin == null) {
      origin = (0, size.height)
      val dy = shapeFile.header.box.yMax - shapeFile.header.box.yMin
      scaleRatio = dy / 800
      scaleStep = scaleRatio * 0.1
    }
    g.clearRect(0, 0, size.width, size.height)
    g.setColor(Color.black)
    shapeFile.shapes.map {
      case IndexedShape(idx, p @ Polygon(box, parts, points)) => drawPolygon(g, p)
    }
  }
}