package net.shapefile

import java.nio.ByteBuffer

object IO {
  def skip(n: Int, data: ByteBuffer) = {
    data.position(data.position + n)
  }
}