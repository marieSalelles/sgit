package sgit.objects

import java.sql.Date

case class Commit (sha: String, timestamp: Date, message: String, parents: Seq[String], tree: Tree) {
}
