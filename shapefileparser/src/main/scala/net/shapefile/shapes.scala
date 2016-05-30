package net.shapefile

import java.nio.ByteBuffer
import scala.util.Try
import java.nio.ByteOrder
import scala.annotation.tailrec

sealed trait Shape {
  def shapeType: ShapeType
}

trait PolyShape[T <: PolyShape[T]] {
  def split: Seq[T] = {
    val a = points.zipWithIndex.groupBy {
      case (point, index) =>
        val pos = for { part <- partsIndex if index >= part } yield {
          part
        }
        pos.last
    }

    val b = a map { case (k, v) => make(v.map { _._1 }) } toSeq

    b
  }
  def points: List[Point]
  def partsIndex: List[Int]
  def box: Box
  def make(points: List[Point]): T
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

case class PolyLine(box: Box, partsIndex: List[Int], points: List[Point]) extends Shape with PolyShape[PolyLine] {
  def shapeType = PolyLineType

  def make(points: List[Point]): PolyLine = PolyLine(box, List(0), points)
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

case class Polygon(box: Box, partsIndex: List[Int], points: List[Point]) extends Shape with PolyShape[Polygon] {
  def shapeType = PolygonType

  def make(points: List[Point]): Polygon = Polygon(box, List(0), points)

  def pointInside(x: Double, y: Double): Boolean = {

    var i = 0
    var j = points.size - 1
    var c = false
    while (i < points.size) {
      val pyi = points(i).y
      val pyj = points(j).y

      val pxi = points(i).x
      val pxj = points(j).x

      if (((pyi > y) != (pyj > y)) &&
        (x < (pxj - pxi) * (y - pyi) / (pyj - pyi) + pxi))
        c = !c;

      j = i
      i += 1
    }
    c
  }

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
