package net.shapefile

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.nio.file.Paths

import scala.util.Try

object DBFFile {
  def parse(file: String): Try[DBFFile] = {
    Try {
      val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get(file)))
      parse(data)
    } flatten
  }

  def parse(data: ByteBuffer): Try[DBFFile] = Try {
    data.order(ByteOrder.LITTLE_ENDIAN)
    val version = data.get
    data.position(4)
    val numRecords: Int = data.getInt
    val bytesInHeader: Int = data.getShort
    val bytesInRecord: Int = data.getShort

    data.position(32)

    val n = (bytesInHeader - 33) / 32 toInt

    val fields = ((Nil: List[Field]) /: (0 until n)) {
      case (acc, i) =>
        val fieldName = asString(11, data)
        val tpe = data.get.toChar
        skip(4, data)
        val len = data.get
        skip(15, data)
        acc ++ List(Field(fieldName, DBFFieldType.fromChar(tpe), len))
    }

    data.get
    val records = for {
      i <- 0 until numRecords
    } yield {
      data.get
      val row = fields.map { f =>
        asString(f.len, data)
      }
      Row(row)
    }

    DBFFile(version, numRecords, fields, records)
  }

  private def asString(len: Int, data: ByteBuffer) = {
    val arr = new Array[Byte](len)
    data.get(arr)
    (arr.filter(_ > 0).map { _ toChar }).mkString.trim
  }

  private def skip(n: Int, data: ByteBuffer) = {
    for { _ <- 0 until n } {
      data.get
    }
  }
}

case class DBFFile(version: Byte, numRecords: Int, meta: List[Field], rows: Seq[Row]) {
  def show {
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