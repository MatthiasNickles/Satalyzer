**Satalyzer** is a small tool for the asynchronous visualization and analysis of event logs generated by supported logic and constraint solvers, such
as SAT, CP, SMT and Answer Set solvers.

Satalyzer is written in Scala and runs on the Java VM (version 8 or higher).  

It is not a general purpose logging or log analytics tool, it is mainly meant for solver debugging and benchmarking purposes.

##### Usage
  
`java -jar satalyzer.jar <path to directory with event log files>`

(A ready-to-run JAR file is available from Releases.)

On the dashboard, select one or more event log files in the upper left section. 
Events (singleton or grouped) appear in the upper middle pane. Select
one or multiple entries there to see a graph depicting the event values. 

![Dashboard](images/satalyzer_img2.png?raw=true)

##### Event log files

Satalyzer reads JSON files consisting of event lists.

Generally, the entries in a Satalyzer log file are (event, value) pairs, where
an event consists of a type and a timestamp, and possibly further information such 
as the solver thread which generated the event. An "event" can be any kind of message or
measurement - it's entirely up to the solver what kinds of events it generates.
For example, an event might be "number of conflicts at time 0:34:23, generated by thread $5" and its value
might be 100637.

Events can also be used to write meta-information into the log file, e.g., the 
name, version and runtime settings of the solver.
 
Multiple events of the same kind can be grouped - this is useful for events which 
are all instances of the same type and are generated over the course of time of the same
solver run, such as the number of conflicts or assignments (in DPLL/CDCL-style solvers),
but also if multiple log files are selected and singleton events in those solver
runs should be compared with each other, e.g., the overall run times of give solver runs (useful for benchmarking).

Event grouping happens automatically. If a group of events is selected in the upper middle pane,
a graph visualization of the grouped event values is shown in the upper right pane.
It is also possible to select multiple groups, to compare their graphs.

##### Generating event log files in your own solver

To let your solver generate Satalyzer log files, you currentl need to include source code file `SatalyzerUse.scala`
into your solver project. (Currently, this short file is available only in Scala code, but translating it
to Java shouldn't be too difficult.) 

Modify line `import [...].stats` as explained in SatalyzerUse.scala.
  
Initialize the logger by assigning variable `stats` a value and calling `initializeStatsFile()`.  
E.g.,  
` stats = new Stats(problemFile = "eventLog.json")  // various other parameters exist, see method stats.initializeStatsFile() `
  
To emit an event, use `stats.writeEntry()`. Examples:  
` stats.writeEntry(key = "numberUnassigned", value = noOfUnassignedVars, solverThreadNo = 5)  
  stats.writeEntry(key = "overallTimeMs", value = timeMs, solverThreadNo = 0) `

To write the collected events to the JSON file, use   
	`stats.writeToFile()`

An existing SAT solver which can generate Satalyzer logs is the probabilistic SAT solver [delSAT](https://github.com/MatthiasNickles/delSAT).

##### Contact

Author: [Matthias Nickles](https://www.researchgate.net/profile/Matthias_Nickles)

`matthiasDOTnicklesATgmxDOTnet`

Feedback and bug reports are welcome!

##### Copyright & License

Copyright (c) 2020 by Matthias Nickles  
License: MIT

##### Dependencies

_jsoniter_ (https://jsoniter.com/)  
Copyright (c) 2016 Tao Wen  
License: https://github.com/json-iterator/java/blob/master/LICENSE  

_EvilPlot_ (https://cibotech.github.io/evilplot/)  
Copyright (c) 2018 CiBO Technologies, Inc.  
License: https://github.com/cibotech/evilplot/blob/master/LICENSE
