package net.shapefile

import scala.util.Try
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.util.Success
import java.nio.file.Paths
import java.nio.file.Files

object ShapeFile {
  def parse(path: String): Try[ShapeFile] = {
    Try {
      val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get(path)))
      ShapeFile(path, ShapeFileHeader.parse(data), Shapes.parseShapes(data, Nil))
    }
  }
}

case class ShapeFile(path: String, header: ShapeFileHeader, shapes: List[IndexedShape])

object ShapeFileHeader {
  def parse(data: ByteBuffer): ShapeFileHeader = {
    data.order(ByteOrder.BIG_ENDIAN)
    val magic = data.getInt
    data.position(24)
    val len = data.getInt
    data.order(ByteOrder.LITTLE_ENDIAN)
    val version = data.getInt
    val shapeType = data.getInt
    ShapeFileHeader(len, version, ShapeType.fromInt(shapeType), BoundingBox.parse(data))
  }
}

case class ShapeFileHeader(size: Int,
                           version: Int,
                           shapeType: ShapeType,
                           box: BoundingBox)

object BoundingBox {
  def parse(data: ByteBuffer): BoundingBox = {
    BoundingBox(data.getDouble, data.getDouble,
      data.getDouble, data.getDouble,
      data.getDouble, data.getDouble,
      data.getDouble, data.getDouble)
  }
}

case class BoundingBox(xMin: Double, yMin: Double,
                       xMax: Double, yMax: Double,
                       zMin: Double, zMax: Double,
                       mMin: Double, mMax: Double)

