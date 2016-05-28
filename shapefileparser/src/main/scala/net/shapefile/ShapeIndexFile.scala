package net.shapefile

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.util.Try
import java.nio.file.Paths
import java.nio.file.Files
import scala.annotation.tailrec

object ShapeIndexFile {
  def parse(path: String): Try[ShapeIndexFile] = Try {

    @tailrec
    def parseIndexes(data: ByteBuffer, acc: List[Index]): List[Index] = {
      if (data.hasRemaining()) {
        val idx = Index(data.getInt, data.getInt)
        parseIndexes(data, acc ++ List(idx))
      } else {
        acc
      }
    }

    val idx = ByteBuffer.wrap(Files.readAllBytes(Paths.get(path + ".shx")))
    IO.skip(100, idx)

    ShapeIndexFile(parseIndexes(idx, Nil))
  }
}

case class ShapeIndexFile(indexes: List[Index])

case class Index(offset: Int, size: Int)
