package sgit.io

import java.nio.file.{Files, Paths}

import better.files._
import sgit.objects.StagedLine

object ReadFile {

  /**
   * Retrieve the name of the file
   * @param f : file sha key
   * @return the name of the file
   */
  def readName(f :String) :String= {
    val currentFolder = (".sgit/objects/"+ f)
    if (Files.exists(Paths.get(currentFolder)) && f != "") {
      val content :String = currentFolder.toFile.contentAsString
      content.substring(0, content.indexOf('\n'))
    } else ""
  }

  /**
   * Read the content of the stages file.
   * @return : the list of the added files.
   */
  def readStaged() :Option[List[StagedLine]]  = {
    val line :List[String] = ".sgit/staged".toFile.contentAsString
      .replace("\r", "")
      .split("\n").toList
    if (line != List("")) {
      Some(line.map(l => {
        val ids = l.split(" ")
       StagedLine(ids(0), ids(1))
      }))
    } else None
  }
}
