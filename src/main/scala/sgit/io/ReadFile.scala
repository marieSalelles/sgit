package sgit.io

import java.nio.file.{Files, Paths}

import better.files._

object ReadFile {

  /**
   * Retrieve the name of the file
   * @param f : file sha key
   * @return the name of the file
   */
  def readName(f :String) :String= {
    val currentFolder = (".sgit/objects/"+ f)
    if (Files.exists(Paths.get(currentFolder)) && f != "") {
      val content = currentFolder.toFile.contentAsString
      content.substring(0, content.indexOf('\n'))
    } else ""
  }
}
