package sgit.io

import better.files._
import scala.util.matching.Regex

object RepoSearching {

  /**
   * Search all the files and folders in the current directory.
   * @return : the sequence of all files/folders
   */
  def searchAllDirectoryFile (folder: String):Seq[File] = {
    val currentFolder = folder.toFile.parent
    val files :Seq[File] = currentFolder.listRecursively.toSeq
    val filesWithoutSgit :Seq[File] = files.filterNot(f => f.path.toString.contains(".sgit"))
    val filesName :Seq[File]= filesWithoutSgit.map((f :File) => f)
    filesName
  }

  /**
   * Search all files which match with the regex
   * @param regex : regular expression
   * @return the sequence of all file/folder
   */
  def searchDirectoryFile (regex :String):Seq[File] = {
    val currentFolder: File = ".".toFile
    //val files :Seq[File] = currentFolder.listRecursively.filter(f => regex.matches(f.name)).toSeq
    val files :Seq[File] = currentFolder.glob(regex).toSeq
    val filesWithoutSgit = files.filterNot(f => f.path.toString.contains(".sgit"))
    val filesName = filesWithoutSgit.map((f :File) => f)
    filesName
  }

  /**
   * Search files corresponding to the name given in the arguments
   * @param files : file name list
   * @return : file list
   */
  def searchDirectoryFile(files : Seq[String]) :Seq[File]= {
    val list = files.iterator.map((f: String) => if (f.toFile.exists) f.toFile else null).toSeq
    list.filterNot(f => f == null)
  }

}
