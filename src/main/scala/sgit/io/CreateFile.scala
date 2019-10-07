package sgit.io

import java.nio.file.Paths
import better.files._

object CreateFile {

  /**
   * Create the file in the objects folder corresponding to the file staged.
   * @param files : files lists
   * @return : sequence of sha keys
   */
  def createObjectBlob(files :Seq[File]) = {
    val root = ".sgit/".toFile.parent
    files.map((file :File) => {
      val path = Paths.get(".sgit/objects/")
      val _: File=  (path + "/" + file.sha1)
        .toFile
        .createIfNotExists()
        .appendLine(root.relativize(file).toString)
        .appendText(file.contentAsString)
      file.sha1
    })
  }

  /**
   * Create the file in the objects folder corresponding to the folder modified
   * @param folders : folder list
   * @return : sequence of sha keys
   */
  /*def createObjectTree(folders :Seq[File]) :Seq[String] = {

  }*/

}
