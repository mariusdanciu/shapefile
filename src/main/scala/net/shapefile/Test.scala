package net.shapefile

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.ByteBuffer

object Test extends App {

  val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get("./data/Judete.shp")))

  for { ShapeFile(header, shapes) <- ShapeFile.parse(data) } {
    println(shapes.size)
    println(header.box)
    println(normalizePolygon(header.box, shapes.head.shape.asInstanceOf[Polygon]).points.head)
    println(shapes.head.shape.asInstanceOf[Polygon].points.head)
  }

  def normalizePolygon(box: BoundingBox, polygon: Polygon) = {
    Polygon(polygon.box, polygon.partsIndex, for { Point(x, y) <- polygon.points } yield {
      Point(x - box.xMin, y - box.yMin)
    })
  }
}