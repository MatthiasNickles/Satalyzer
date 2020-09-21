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

package commandline

object Main {

  var writeStatsDirectory = "C:/Users/Owner/workspaceScala211/DelSAT/history/"

  /**
   * Command line processing
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {

    if (args.length > 0) {

      if (args(0) == "--help" || args(0) == "/?" || args(0) == "-h") {

        println(
          """

      Satalyzer 0.1

      Copyright (c) 2020 Matthias Nickles
      License: MIT

      Usage: java -jar satalyzer.jar <folder path for log files>

      Includes libraries

         jsoniter (https://jsoniter.com/)
         Copyright (c) 2016 Tao Wen
         License: https://github.com/json-iterator/java/blob/master/LICENSE

         EvilPlot (https://cibotech.github.io/evilplot/)
         Copyright (c) 2018 CiBO Technologies, Inc.
         License: https://github.com/cibotech/evilplot/blob/master/LICENSE
      """)

        sys.exit(0)

      }

      writeStatsDirectory = args(0)

    }

    println("Launching Satalyzer Dashboard...\nReading event log files in directory " + writeStatsDirectory)

    visualization.Satalyzer.show

  }

}
