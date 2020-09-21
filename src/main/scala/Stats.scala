/**
 * Satalyzer
 *
 * Copyright (c) 2020 Matthias Nickles
 *
 * matthiasDOTnicklesATgmxDOTnet
 *
 * This code is licensed under MIT License (see file LICENSE for details)
 *
 */

package serialData

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.time.Instant
import java.util
import java.util.UUID

import visualization.Satalyzer

final case class StatsEntry( // Any modification of this class (e.g., new fields or field names) need to be reflected in the
                             // deserialization method further below (can't be don't automatically)
                             // >>> AND of course also in class StatsEntry in the serialization code (SatalyzerUse.scala, e.g., in delSAT) <<<

                             messageTimeStamp: Long = -1l, // in nano secs from program start (don't use as a unique entry or message key)

                             threadNo: Int = 0,

                             key: String = null,

                             valueStr: String = null,

                             @transient runIDTransient: String = null

                           ) {

}


final case class Stats( // Any modification of this class (e.g., new fields or field names) need to be reflected in the
                        // deserialization method further below (can't be don't automatically)
                        // AND of course also in class Stats in the serialization code (SatalyzerUse.scala, e.g., in delSAT).

                        problemFile: String,

                        runID: String = UUID.randomUUID().toString,  // a type-4 UUID represented as a string

                        runStartTimeMs: Long = Instant.now.toEpochMilli(),

                        // any other information, including solving duration, should be written as StatsEntry entries

                        entries: util.List[StatsEntry] /*doesn't serialize properly: ArrayBuffer[StatsEntry]*/ =
                        new util.ArrayList[StatsEntry](),

                        @transient var outFile: File = null,

                        @transient var statsFileStream: OutputStream = null,

                        @transient var lastFlush: Long = 0l

                      ) {


  @inline def initializeStatsFileStream(): Unit = {

    assert(outFile != null)

    statsFileStream = new BufferedOutputStream(
      new FileOutputStream(outFile))

  }

  def getFirstOpt(key: String): Option[StatsEntry] = {

    var i = 0

    while (i < entries.size()) {

      val existingEntry = entries.get(i)

      if (existingEntry.key == key)
        return Some(existingEntry)

      i += 1

    }

    None

  }

  def countEntriesWithKey(key: String, maxCount: Int = Int.MaxValue): Int = {

    var count = 0

    var i = 0

    while (i < entries.size()) {

      if (entries.get(i).key == key) {

        count += 1

        if (count >= maxCount)
          return count

      }

      i += 1

    }

    count

  }

  override def toString(): String = {

    val runStartTimeMsStr = Instant.ofEpochMilli(runStartTimeMs).toString

    val problemName = Satalyzer.fileNameFromFullPath(problemFile)

    problemName + " [" + runStartTimeMsStr + "]"

  }


}
