package net.shapefile

import scala.util.Try
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.util.Success

object ShapeFile {
  def parse(data: ByteBuffer): Try[ShapeFile] = {
    Try {
      ShapeFile(ShapeFileHeader.parse(data), Shapes.parseShapes(data, Nil))
    }
  }

}

case class ShapeFile(header: ShapeFileHeader, records: List[IndexedShape])

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

case class ShapeFileHeader(langth: Int,
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


