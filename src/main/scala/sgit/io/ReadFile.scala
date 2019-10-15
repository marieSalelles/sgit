package sgit.io

import java.nio.file.{Files, Paths}

import better.files._
import sgit.objects.{Commit, StagedLine}

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
   * Retrieve the content of the file
   * @param f : file sha key
   * @return the name of the file
   */
  def readBlobContent(f :String) :String= {
    val currentFolder = (".sgit/objects/"+ f)
    if (Files.exists(Paths.get(currentFolder)) && f != "") {
      val content :String = currentFolder.toFile.contentAsString
      content.substring(content.indexOf("\n"))
    } else ""
  }

  /**
   * Read the content of the stages file.
   * @return the list of the added files.
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

  /**
   * Read the content of a commit file.
   * @param commit : the sha key of a commit file
   * @return the list of the files commit
   */
  def readCommit(commit :String) :List[StagedLine] = {
    val line :List[String] = (".sgit/objects/"+commit).toFile.contentAsString
      .replace("\r","")
      .split("\n").toList
    val contentWithoutParents = line.tail
    contentWithoutParents.tail.map(l => {
      val indice = l.split(" ")
      StagedLine(indice(0),indice(1))
    })
  }

  /**
   * Read the current branch in the HEAD file
   * @return the current branch
   */
  def readHEAD() :String = {
    (".sgit/HEAD").toFile.contentAsString
  }

  /**
   * Read the last commit of a branch
   * @param branch the current branch
   * @return the name of the last commit
   */
  def readHeads(branch: String) :Option[String] = {
    if ((".sgit/"+branch).toFile.exists) Some((".sgit/"+ branch).toFile.contentAsString)
    else None
  }

  /**
   * Rerieve all the commit properties and create a commit object.
   * @param commit commit sha key
   * @return a commit object
   */
  def readCommitProperties(commit: Option[String]) :Option[Commit]= {
    if(commit.isEmpty) return None
    val commitFile = (".sgit/objects/" + commit.get).toFile
    if(commitFile.exists) {
      val content: Seq[String] = commitFile.contentAsString.replace("\r","").split("\n").toList

      val parents: Seq[String] = content.head.split(" ").toList
      val contentWithoutParents = content.tail
      val message: String = contentWithoutParents.head
      val allFileElement: Seq[StagedLine] = contentWithoutParents.tail.map(f => {
        val line = f.split(" ")
        StagedLine(line(0), line(1))
      })
      Some(Commit(commit.get, commitFile.lastModifiedTime, parents, allFileElement, message))
    } else None
  }
}
