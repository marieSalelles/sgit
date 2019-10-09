package sgit.io

import sgit.objects.{Blob, Commit, Tree, TreeElement}
import better.files._

import scala.annotation.tailrec

object TreeMapulation {
/*
  def buildRepoTree(origin :TreeElement) :TreeElement = {
    val root :File = ".sgit/".toFile.parent
    val childrenList :List[File] = origin.sha.toFile.children.toList
    val folders :List[File] = childrenList.filter(c => c.isDirectory)
    val l1 :List[Tree] = folders
      .map( (c :File) =>
        Tree(c.sha1, buildRepoTree(TreeElement(root.relativize(c).toString, c.sha1)),root.relativize(c).toString ))

    val files = childrenList.filterNot(c => c.isDirectory)
    val l2 = files.map(c => if((".sgit/objects:"+c.sha1).toFile.exists) c.sha1)
    val contentChildren = l1.concat(l2)
    Tree(origin.sha,origin.name, contentChildren)
  }
  */
  /**
   * Adds children recursively to the tree.
   * @param files the files to add
   * @param res the result
   * @return the sequence of children
   */
  @tailrec
  def addChildren(files: Seq[File], res: Seq[TreeElement]): Seq[TreeElement] = {
    if(files.isEmpty) res
    else {
      val firstFile: File = files.head
      if(firstFile.isDirectory) addChildren(files.tail, res:+ buildTree(firstFile).orNull)
      else {
        val root :File = ".sgit/".toFile.parent
        val blob = Blob(firstFile.sha1, firstFile.contentAsString,root.relativize(firstFile).toString)
        addChildren(files.tail, res:+blob)
      }
    }
  }

  /**
   * Builds the main tree from an origin.
   * @param origin the origin folder
   * @return the tree.
   */
  def buildTree(origin: File): Option[TreeElement] = {
    if(!origin.isDirectory) None
    val root :File = ".sgit/".toFile.parent
    val children: Seq[File]= origin.children.toIndexedSeq
    Some(Tree(origin.sha1, addChildren(children, Seq()).filterNot(elt => elt ==null), root.relativize(origin).toString))
  }

  /**
   * Build a tree starting to the commit tree.
   * @param commit : commit
   * @return the whole commit tree
   */
  def readCommitTree(commit: Commit): TreeElement = {
    val rootFile: String = commit.tree.sha
    buildCommitTree(rootFile)
  }


  /**
   * Addthe children of the tree
   * @param origin sha key of a file/folder
   * @return a blob or a tree
   */
  def buildCommitTree(origin: String) : TreeElement = {
      val root :File = ".sgit/".toFile.parent

      //search file corresponding to sha key
      val file: File = (".sgit/objects/" + origin).toFile
      //retrieve the content of the file
      val fileContent: String = file.contentAsString

      //check if the file is a folder or a file
      if (fileContent.startsWith("Tree")) {
        //retrieve all the properties of the tree
        val treeProperties: Seq[String] = fileContent
            .replace("\r", "")
            .split("\n").toList
        //children list
        val listChildren :Seq[String] = treeProperties(2).split(" ").toList
        //folder name (path in the repo)
        val pathProperty :String = treeProperties(3)

        //built the tree object
        Tree(origin, listChildren.map (child => buildCommitTree(child)),pathProperty)

      } else {
        Blob(origin,fileContent, root.relativize(file).toString)
      }
    }
}
