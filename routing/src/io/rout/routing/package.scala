package io.rout

import io.rout.routing.{Assets, Routing}
import com.twitter.conversions.storage._
import com.twitter.util.{Future, StorageUnit}

package object file {

 /*
  def withCacheSize(size: StorageUnit = 50.megabytes) = FileOps(size)
  lazy val fileCache = withCacheSize()

  implicit class File(val routing: Routing) extends AnyVal{

    def cacheSize(size: StorageUnit) =
      io.rout.file.withCacheSize(size)

    def asset1(prefix: String) =
      routing :+ Assets(prefix,fileCache).asset1

    def asset2(prefix: String) =
      routing :+ Assets(prefix,fileCache).asset2

    def asset3(prefix: String) =
      routing :+ Assets(prefix,fileCache).asset3

    def asset4(prefix: String) =
      routing :+ Assets(prefix,fileCache).asset5

    def asset5(prefix: String) =
      routing :+ Assets(prefix,fileCache).asset5

    def asset(prefix: String,depth: Int) =
      routing :+ Assets(prefix,fileCache).assetDepth(depth)

    def debugAssets = routing :+ Assets.addDebug(fileCache)

  }
  */

}
