package net.shapefile

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.nio.file.Paths

import scala.util.Try

object DBFFile {
  def parse(file: String): Try[DBFFile] = {

    def parseSchema(data: ByteBuffer, num: Int) = ((Nil: List[Field]) /: (0 until num)) {
      case (acc, i) =>
        val fieldName = asString(11, data)
        val tpe = data.get.toChar
        IO.skip(4, data)
        val len = data.get
        IO.skip(15, data)
        acc ++ List(Field(fieldName, DBFFieldType.fromChar(tpe), len))
    }

    def parseRecords(data: ByteBuffer, numRecords: Int, fields: List[Field]) = for {
      i <- 0 until numRecords
    } yield {
      data.get
      val row = fields.map { f =>
        asString(f.len, data)
      }
      Row(row)
    }

    Try {
      val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get(file)))

      data.order(ByteOrder.LITTLE_ENDIAN)
      val version = data.get
      data.position(4)
      val numRecords: Int = data.getInt
      val bytesInHeader: Int = data.getShort

      data.position(32)

      val n = (bytesInHeader - 33) / 32 toInt

      val schema = parseSchema(data, n)
      data.get
      val records = parseRecords(data, numRecords, schema)

      DBFFile(version, numRecords, schema, records)
    }
  }

  private def asString(len: Int, data: ByteBuffer) = {
    val arr = new Array[Byte](len)
    data.get(arr)
    (arr.filter(_ > 0).map { _ toChar }).mkString.trim
  }

}

case class DBFFile(version: Byte, numRecords: Int, meta: List[Field], rows: Seq[Row]) {
  def show {
    println(meta mkString "\n")
    println("===================================")
    println(rows mkString "\n")
  }
}

object DBFFieldType {
  def fromChar(c: Char): DBFFieldType = c match {
    case 'C' => CHAR
    case 'N' => NUMERIC
    case 'M' => MEMO
    case 'L' => LOGICAL
    case 'D' => DATE
    case 'F' => FLOAT
  }
}

sealed trait DBFFieldType
case object CHAR extends DBFFieldType
case object DATE extends DBFFieldType
case object NUMERIC extends DBFFieldType
case object LOGICAL extends DBFFieldType
case object MEMO extends DBFFieldType
case object FLOAT extends DBFFieldType

case class Field(name: String, fType: DBFFieldType, len: Byte)
case class Row(values: List[String])