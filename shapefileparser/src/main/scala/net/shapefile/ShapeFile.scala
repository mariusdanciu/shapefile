package net.shapefile

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.nio.file.Paths

import scala.util.Try
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object ShapeFile {
  def parse(path: String, loadProps: Boolean): Try[ShapeFile] = {
    Try {
      val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get(path + ".shp")))

      val header = ShapeFileHeader.parse(data)
      val shapes = Shapes.parseShapes(data, Nil)

      val (s, schema) = if (loadProps) {
        val dbf = DBFFile.parse(path).get
        (for ((shape, row) <- shapes zip dbf.rows) yield {
          Geo(shape, Props((dbf.meta zip row.values) map { case (f, v) => (f.name, v) } toMap))
        }, Some(PropsSchema(dbf.meta)))
      } else {
        (shapes.map { Geo(_, Props(Map.empty)) }, None)
      }
      ShapeFile(header, s, schema)
    }
  }
}

case class Geo(shape: Shape, props: Props)
case class PropsSchema(fields: List[Field])

case class Props(props: Map[String, String]) {
  def int(key: String) = Try(props(key).toInt)
  def long(key: String) = Try(props(key).toLong)
  def float(key: String) = Try(props(key).toFloat)
  def double(key: String) = Try(props(key).toDouble)
  def boolean(key: String) = Try(props(key).toBoolean)
  def string(key: String) = Try(props(key))
}

case class ShapeFile(header: ShapeFileHeader, shapes: List[Geo], schema: Option[PropsSchema])

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

