package net.shapefile

import java.nio.ByteBuffer
import scala.util.Try
import java.nio.ByteOrder
import scala.annotation.tailrec

sealed trait Shape {
  def shapeType: ShapeType
}

case object NullShape extends Shape {
  def shapeType = NullShapeType
}

case class Point(x: Double, y: Double) extends Shape {
  def shapeType = PointType
}

case class PointM(x: Double, y: Double, m: Double) extends Shape {
  def shapeType = PointMType
}

case class PointZ(x: Double, y: Double, z: Double, m: Double) extends Shape {
  def shapeType = PointZType
}

case class MultiPoint(box: Box, points: List[Point]) extends Shape {
  def shapeType = MultiPointType
}

case class MultiPointM(box: Box, points: List[Point],
                       measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = MultiPointMType
}
case class MultiPointZ(box: Box, points: List[Point],
                       zRange: (Double, Double), zmeasures: List[Double],
                       measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = MultiPointZType
}

case class PolyLine(box: Box, partsIndex: List[Int], points: List[Point]) extends Shape {
  def shapeType = PolyLineType
}

case class PolyLineM(box: Box, partsIndex: List[Int], points: List[Point],
                     measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = PolyLineMType
}

case class PolyLineZ(box: Box, partsIndex: List[Int], points: List[Point],
                     zRange: (Double, Double), zMeasures: List[Double],
                     measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = PolyLineZType
}

case class Polygon(box: Box, partsIndex: List[Int], points: List[Point]) extends Shape {
  def shapeType = PolygonType
}

case class PolygonM(box: Box, partsIndex: List[Int], points: List[Point],
                    measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = PolygonMType
}

case class PolygonZ(box: Box, partsIndex: List[Int], points: List[Point],
                    zRange: (Double, Double), zMeasures: List[Double],
                    measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = PolygonZType
}

case class MultiPatch(box: Box, partsIndex: List[Int], partTypes: List[Int], points: List[Point],
                      zRange: (Double, Double), zMeasures: List[Double],
                      measureRange: (Double, Double), measures: List[Double]) extends Shape {
  def shapeType = MultiPatchType
}

object Box {
  def parse(data: ByteBuffer): Box = {
    Box(data.getDouble, data.getDouble,
      data.getDouble, data.getDouble)
  }
}
case class Box(xMin: Double, yMin: Double,
               xMax: Double, yMax: Double)

object Shapes {

  @tailrec
  def parseShapes(data: ByteBuffer, acc: List[Shape]): List[Shape] = {
    if (data.hasRemaining()) {
      val RecordHeader(idx, size) = RecordHeader.parse(data)
      data.order(ByteOrder.LITTLE_ENDIAN)

      val shapeType = data.getInt
      val shape = ShapeType.fromInt(shapeType).parse(data)

      parseShapes(data, acc ++ List(shape))
    } else {
      acc
    }
  }
}

object RecordHeader {
  def parse(data: ByteBuffer): RecordHeader = {
    data.order(ByteOrder.BIG_ENDIAN)
    RecordHeader(data.getInt, data.getInt)
  }

}
case class RecordHeader(num: Int, contentLength: Int)
