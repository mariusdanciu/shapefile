package net.shapefile

import java.nio.ByteBuffer

object ShapeType {
  def fromInt(value: Int): ShapeType = value match {
    case 0  => NullShapeType
    case 1  => PointType
    case 3  => PolyLineType
    case 5  => PolygonType
    case 8  => MultiPointType
    case 11 => PointZType
    case 13 => PolyLineZType
    case 15 => PolygonZType
    case 18 => MultiPointZType
    case 21 => PointMType
    case 23 => PolyLineMType
    case 25 => PolygonMType
    case 28 => MultiPointMType
    case 31 => MultiPatchType
  }

}

sealed trait ShapeType {
  def value: Int

  def parse(data: ByteBuffer): Shape

  protected def readPoints(num: Int, data: ByteBuffer): List[Point] = {
    (for { _ <- 0 until num } yield {
      PointType.parse(data)
    }) toList
  }
}

object NullShapeType extends ShapeType {
  def value = 0
  def parse(data: ByteBuffer) = NullShape
}

object PointType extends ShapeType {
  def value = 1
  def parse(data: ByteBuffer) = {
    Point(data.getDouble, data.getDouble)
  }
}

trait Poly { self: ShapeType =>
  def readPoly(data: ByteBuffer) = {
    val box = Box.parse(data)
    val numParts = data.getInt
    val numPoints = data.getInt
    val parts = for { _ <- 0 until numParts } yield {
      data.getInt
    }
    val points = readPoints(numPoints, data)
    (box, parts toList, points)
  }

  def readMeasures(numPoints: Int, data: ByteBuffer) = {
    val mMin = data.getDouble
    val mMax = data.getDouble
    val mArray = for (_ <- 0 until numPoints) yield {
      data.getDouble
    }
    (mMin, mMax, mArray toList)
  }

  def readZMeasures(numPoints: Int, data: ByteBuffer) = {
    val zMin = data.getDouble
    val zMax = data.getDouble
    val zArray = for (_ <- 0 until numPoints) yield {
      data.getDouble
    }
    val mMin = data.getDouble
    val mMax = data.getDouble
    val mArray = for (_ <- 0 until numPoints) yield {
      data.getDouble
    }
    (zMin, zMax, zArray toList, mMin, mMax, mArray toList)
  }
}

object PolyLineType extends ShapeType with Poly {
  def value = 3
  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    PolyLine(box, parts toList, points)
  }
}

object PolygonType extends ShapeType with Poly {
  def value = 5
  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    Polygon(box, parts toList, points)
  }
}

object MultiPointType extends ShapeType {
  def value = 8
  def parse(data: ByteBuffer) = {
    val box = Box.parse(data)
    val num = data.getInt
    val points = readPoints(num, data)
    MultiPoint(box, points toList)
  }
}
object PointZType extends ShapeType {
  def value = 11

  def parse(data: ByteBuffer) = {
    PointZ(data.getDouble, data.getDouble, data.getDouble, data.getDouble)
  }
}

object PolyLineZType extends ShapeType with Poly {
  def value = 13

  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    val (zMin, zMax, zMeasures, min, max, measures) = readZMeasures(points.size, data)

    PolyLineZ(box, parts toList, points, (zMin, zMax), zMeasures, (min, max), measures)
  }
}

object PolygonZType extends ShapeType with Poly {
  def value = 15

  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    val (zMin, zMax, zMeasures, min, max, measures) = readZMeasures(points.size, data)
    PolygonZ(box, parts toList, points, (zMin, zMax), zMeasures, (min, max), measures)
  }
}

object MultiPointZType extends ShapeType with Poly {
  def value = 18

  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    val (zMin, zMax, zMeasures, min, max, measures) = readZMeasures(points.size, data)

    MultiPointZ(box, points toList, (zMin, zMax), zMeasures toList, (min, max), measures toList)
  }
}
object PointMType extends ShapeType {
  def value = 21
  def parse(data: ByteBuffer) = {
    PointM(data.getDouble, data.getDouble, data.getDouble)
  }
}
object PolyLineMType extends ShapeType with Poly {
  def value = 23

  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    val (min, max, measures) = readMeasures(points.size, data)

    PolyLineM(box, parts toList, points, (min, max), measures)
  }
}

object PolygonMType extends ShapeType with Poly {

  def value = 25

  def parse(data: ByteBuffer) = {
    val (box, parts, points) = readPoly(data)
    val (min, max, measures) = readMeasures(points.size, data)
    PolygonM(box, parts toList, points, (min, max), measures)
  }
}

object MultiPointMType extends ShapeType with Poly {
  def value = 28
  def parse(data: ByteBuffer) = {
    val shapeType = data.getInt
    val box = Box.parse(data)
    val numPoints = data.getInt
    val points = readPoints(numPoints, data)
    val (min, max, measures) = readMeasures(points.size, data)
    MultiPointM(box, points toList, (min, max), measures)
  }
}

object MultiPatchType extends ShapeType with Poly {
  def value = 31

  def parse(data: ByteBuffer) = {
    val shapeType = data.getInt
    val box = Box.parse(data)
    val numParts = data.getInt
    val numPoints = data.getInt
    val parts = for { _ <- 0 until numParts } yield {
      data.getInt
    }
    val partTypes = for { _ <- 0 until numParts } yield {
      data.getInt
    }
    val points = readPoints(numPoints, data)

    val (zMin, zMax, zMeasures, min, max, measures) = readZMeasures(points.size, data)
    MultiPatch(box, parts toList, partTypes toList, points toList, (zMin, zMax), zMeasures, (min, max), measures)
  }
}
