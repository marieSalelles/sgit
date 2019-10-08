package sgit.io

import java.nio.file.Paths

import better.files._
import sgit.objects.Blob

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
         .appendLine(root.relativize(file).toString)
        .appendText(file.contentAsString)
      Blob(file.sha1,file.contentAsString, root.relativize(file).toString)
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
