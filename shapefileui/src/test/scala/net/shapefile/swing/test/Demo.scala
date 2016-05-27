package net.shapefile.swing.test

import net.shapefile.ShapeFile
import scala.swing.MainFrame
import java.nio.file.Files
import net.shapefile.swing.MapCanvas
import scala.swing.SimpleSwingApplication
import java.nio.file.Paths
import java.awt.Dimension
import java.nio.ByteBuffer
import scala.swing.Label
import net.shapefile.DBFFile

object Demo extends SimpleSwingApplication {
  //val path = "../data/world/TM_WORLD_BORDERS_SIMPL-0.3.shp"
  val path = "../data/romania/Judete.shp"

  def loadShape = {

    val dbf = DBFFile.parse("../data/romania/Judete.dbf")
    dbf map { _ show }
    ShapeFile.parse(path)
  }

  def top = {
    new MainFrame {
      val map =
        (loadShape map { s =>
          new MapCanvas(s) {
            preferredSize = new Dimension(1400, 800)
          }
        } recover { case t => new Label(t toString) }).get
      title = path
      contents = map
    }
  }
}