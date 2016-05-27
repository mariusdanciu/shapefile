package net.shapefile.swing.test

import net.shapefile.ShapeFile
import scala.swing.MainFrame
import java.nio.file.Files
import net.shapefile.swing.MapCanvas
import scala.swing.SimpleSwingApplication
import java.nio.file.Paths
import java.awt.Dimension
import java.nio.ByteBuffer

object Demo extends SimpleSwingApplication {

  def loadShape = {
    val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get("../data/world/TM_WORLD_BORDERS-0.3.shp")))
    //val data = ByteBuffer.wrap(Files.readAllBytes(Paths.get("../data/romania/Judete.shp")))
    ShapeFile.parse(data)
  }

  def top = {
    new MainFrame {
      val map = new MapCanvas(loadShape) {
        preferredSize = new Dimension(1400, 800)
      }
      title = "First Swing App"
      contents = map
    }
  }
}