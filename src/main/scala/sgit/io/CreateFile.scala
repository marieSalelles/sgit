package sgit.io

import better.files._

object CreateFile {

  /**
   * Create the file in the objects folder corresponding to the file staged.
   * @param files : files lists
   * @return : sequence of sha keys
   */
  def createObjectBlob(files :Seq[File]) = {
    files.map((file :File) => {
      val _: File= ".sgit/objects/"+file.sha1
        .toFile
        .appendLine(file.name)
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
