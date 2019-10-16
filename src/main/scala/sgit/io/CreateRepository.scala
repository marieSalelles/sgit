package sgit.io

import better.files._
import java.nio.file.{Files, Paths}

import sgit.objects.Blob

object CreateRepository {

  /**
   * Create the tree view of the sgit repository if .sgit not already exist.
   * @return the state of the repository creation
   */
  def initialisation() :Boolean ={
    try {
      if (!Files.exists(Paths.get(".sgit"))) {
        createDirectory(true, ".sgit")
        createDirectory(false, ".sgit/HEAD")
        createDirectory(true, ".sgit/objects")
        createDirectory(true, ".sgit/refs")
        createDirectory(true, ".sgit/refs/heads")
        createDirectory(true, ".sgit/refs/tags")
        createDirectory(false, ".sgit/staged")
        true
      } else {
        false
      }
    }catch {
        case e :Exception => false
    }
  }

  /**
   * Create a file or a folder.
   * @param isFolder : Boolean which define if we have to create a file or a folder
   * @param nameF : name of the new file/folder
   */
  def createDirectory(isFolder :Boolean, nameF: String): Boolean= {
    try {
      val dir: File = nameF
        .toFile
        .createIfNotExists(isFolder,createParents = false)
      true
    }
    catch {
      case e :Exception => println("Error, please write the command again.")
        false
    }
  }

  /**
   * Create in the working directory the file and its parents folders
   * @param blob : file
   */
  def builtRepository(blob: Blob): Unit = {
    val _: File =  blob.path.toFile
      .createIfNotExists( asDirectory = false, createParents = true)
      .overwrite(blob.content)
  }

}
