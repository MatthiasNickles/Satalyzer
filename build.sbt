/**
 *
 * Satalyzer
 *
 * Copyright (c) 2020 Matthias Nickles
 *
 * matthiasDOTnicklesATgmxDOTnet
 *
 * Licensed under MIT License (see file LICENSE for details)
 *
 */

name := "Satalyzer"

version := "0.1"

scalaVersion := "2.13.2"

mainClass in (Compile, run) := Some("commandline.Main")

libraryDependencies += "com.jsoniter" % "jsoniter" % "0.9.23"

resolvers += "cibo artifacts" at "https://dl.bintray.com/cibotech/public/"

libraryDependencies += "com.cibo" %% "evilplot" % "0.8.0"


