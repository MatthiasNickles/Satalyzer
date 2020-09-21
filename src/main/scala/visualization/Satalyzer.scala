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

package visualization

import serialData._
import java.awt.event.{ActionEvent, ActionListener, MouseAdapter, MouseEvent, MouseListener, MouseMotionListener}
import java.awt.image.BufferedImage
import java.awt.{Color, Component, Dimension, Graphics, GridLayout, Robot}
import java.io.{File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util
import java.util.{Date, Random}

import com.cibo.evilplot.geometry.{Align, BorderRect, Extent, Rect, Text}
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.renderers.{BarRenderer, PointRenderer}
import com.cibo.evilplot.plot.{Bar, BarChart, Histogram, LinePlot, ScatterPlot}
import com.jsoniter.{JsonIterator, any}
import javax.swing.event.{DocumentEvent, DocumentListener, ListSelectionEvent, ListSelectionListener}
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing.text.DefaultHighlighter
import javax.swing.text.html.{HTML, HTMLDocument}
import javax.swing._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{JavaConverters, mutable}

final object Satalyzer {

  import java.awt.BorderLayout

  import com.cibo.evilplot.colors.HTMLNamedColors
  import com.cibo.evilplot.geometry.{Drawable, Graphics2DRenderContext, LineStyle, fit}
  import com.cibo.evilplot.plot.renderers.PathRenderer
  import com.cibo.evilplot.plot.{Overlay, Plot}
  import javax.swing.{JFrame, JPanel, JScrollPane}

  val prng = new Random()

  val compHeight = 450

  val compWidth = 700

  val compWidthAdaptWest = -210

  var groupSingletonKeyAcrossFiles = true

  var useAbsoluteTimesForSingletonEntitiesGroupedAcrossFiles = true // with true (default), for singelton entries, the
  // UTC time stamp (approx. delSAT run start date/time in millisec since 1970) of the respective formula file is used to
  // index the grouped entries (but displayed on the x-axis of curves converted to local timezone!).
  // With false, even if the entries stem from different files, the relative times (duration of the respective run when
  // the entry was written to the json file) is used, which normally doesn't make much sense if there are multiple files (runs) involved.

  val allEntriesFromAllSelectedJsonFiles = ArrayBuffer[StatsEntry]()
  // ^ (re-)populated in fileListSelectionModel. Also see refreshFileAndEntryLists

  val allEntriesFromAllSelectedJsonFilesGrouped = ArrayBuffer[ArrayBuffer[StatsEntry]]()
  // ^ (re-)populated in the selection listener of fileListSelectionModel. Also see refreshFileAndEntryLists

  val runIDToStats = mutable.HashMap[String, Stats]()

  var fileList: mutable.Buffer[File] = null // all files, before any filtering

  var fileStatsList: Array[(File, Stats)] = null // with all files, before any filtering

  def initializeFileListFromDir(dir: String): Unit = {

    fileList = new File(dir).listFiles().toBuffer

    if (fileList.isEmpty)
      error("Directory " + dir + "\nis empty")

  }

  def initialize(): Unit = {

    fileStatsList = fileList.map(file => (file, readStatsFromFile(file))).toArray

    runIDToStats.clear()

    for (fileAndStats <- fileStatsList) {

      // val statsFromFile: Stats = readStatsFromFile(file)

      runIDToStats.put(fileAndStats._2.runID, fileAndStats._2)

    }

  }

  initializeFileListFromDir(commandline.Main.writeStatsDirectory)

  initialize()

  def error(message: String) = {

    JOptionPane.showMessageDialog(null, message)

  }

  def fileNameFromFullPath(fileStr: String): String = {

    java.nio.file.Paths.get(fileStr).getFileName().toString()

  }


  def parseDoubleOrElse(str: String, e: Double = -1d): Double = {

    val dOpt = str.toDoubleOption

    dOpt.getOrElse(e)

  }

  def round(n: Double, digits: Int): Double = {

    val p = Math.pow(10d, digits)

    Math.round(n * p) / p

  }

  def instantMsToLocalZoneDateTimeFormatted(instantMsUTC: Long): String = {

    val startTimeUTCInstant = Instant.ofEpochMilli(instantMsUTC)

    val runStartTimeZonedSystem = ZonedDateTime.ofInstant(startTimeUTCInstant, ZoneId.systemDefault())

    runStartTimeZonedSystem.format(DateTimeFormatter.ofPattern("H:m:s 'on' yyyy-MM-dd (O)"))

  }

  def getRectifiedTimeStampOfEntryInMsOrSec(entry: StatsEntry, convertToSec: Boolean, enforceUTC: Boolean): (Double /*ms*/ , Boolean /*UTC yes/no*/ ) = {

    val statsWithThisEntry: Stats = runIDToStats(entry.runIDTransient)

    val divider = if (convertToSec) 1000d else 1d

    val r = if (enforceUTC /*|| groupSingletonKeyAcrossFiles && useAbsoluteTimesForSingletonEntitiesGroupedAcrossFiles &&
      statsWithThisEntry.countEntriesWithKey(entry.key, 2) == 1 /*"singleton" key, i.e., occurs only once per json-file */ */ ) {

      // In this case, we use the "global" time stamp (start time of the delSAT run which generated the entry)
      // instead of the (in this case typically meaningless) relative entry message time stamp. This way, we can compare the values of
      // multiple singleton keys (e.g., plot multiple SAT solver duration times as a curve).

      (statsWithThisEntry.runStartTimeMs.toDouble / divider, true)

    } else
    (entry.messageTimeStamp.toDouble / 1000000d / divider, false) // we use Double because in case we later write the
    // data to a csv or gnuplot file, we need to convert seconds back to ms, so we don't want to round here.

    r

  }

  def readStatsFromFile(inFile: File): Stats = {

    val jsonStr = new String(Files.readAllBytes(inFile.toPath), StandardCharsets.UTF_8)

    val newStatsAny: com.jsoniter.any.Any = try {

      JsonIterator.deserialize(jsonStr)

    } catch {

      case e: com.jsoniter.spi.JsonException => {

        error("" + inFile.toPath + " " + e.getMessage)

        return new Stats(problemFile = "<Error>")

        null.asInstanceOf[com.jsoniter.any.Any]

      }

    }

    val problemFile = newStatsAny.toString("problemFile") //

    val runID = newStatsAny.toString("runID") // string representation of a UUID

    val runStartTimeMs = newStatsAny.toLong("runStartTimeMs")

    val entriesJ = newStatsAny.get("entries")

    val entriesAny: util.List[any.Any] = entriesJ.asList

    val entries = JavaConverters.asJava(JavaConverters.asScalaBuffer(entriesAny).map((anyJson: com.jsoniter.any.Any) => {

      val messageTimeStamp = anyJson.toLong("messageTimeStamp")

      val threadNo = anyJson.toInt("threadNo")

      val key = anyJson.toString("key")

      val valueStr = anyJson.toString("valueStr")

      new StatsEntry(threadNo = threadNo, key = key, valueStr = valueStr, runIDTransient = runID, messageTimeStamp = messageTimeStamp)

    }))

    val newStats = new Stats(problemFile = problemFile, runID = runID, runStartTimeMs = runStartTimeMs,
      entries = entries)

    newStats

  }

  class FramePanel extends JPanel {

    import javax.swing.JTable

    class EntriesJTableModel extends AbstractTableModel {

      override def getRowCount: Int = {

        allEntriesFromAllSelectedJsonFilesGrouped.length

      }

      override def getColumnCount: Int = {

        5

      }

      override def getColumnName(column: Int): String = {

        Seq("formula@UTC", "thread", "@time (sec)", "key", "value(s)")(column)

      }

      def problemDescription(runID: String) = {

        val stats = runIDToStats.getOrElse(runID, null)

        val runStartTimeMsStr = Instant.ofEpochMilli(stats.runStartTimeMs).toString

        val problemName = fileNameFromFullPath(stats.problemFile)

        problemName + "[" + runStartTimeMsStr + "]"

      }

      override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {

        val entriesAtRow: mutable.Seq[StatsEntry] = allEntriesFromAllSelectedJsonFilesGrouped(rowIndex)

        if (entriesAtRow.size == 1) {

          columnIndex match {

            case 0 => problemDescription(entriesAtRow(0).runIDTransient) //.asInstanceOf[AnyRef]

            case 1 => if (entriesAtRow(0).threadNo == 0) "" else "$" + entriesAtRow(0).threadNo

            case 2 => (entriesAtRow(0).messageTimeStamp / 1000000000l).asInstanceOf[AnyRef]

            case 3 => entriesAtRow(0).key.asInstanceOf[AnyRef]

            case 4 => entriesAtRow(0).valueStr

          }

        } else if (entriesAtRow.size > 1) {

          val minMessageTimeSec = round(getRectifiedTimeStampOfEntryInMsOrSec(entriesAtRow.minBy(entry => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = true, enforceUTC = false)._1), convertToSec = true, enforceUTC = false)._1.toDouble, 1)

          val maxMessageTimeSec = round(getRectifiedTimeStampOfEntryInMsOrSec(entriesAtRow.maxBy(entry => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = true, enforceUTC = false)._1), convertToSec = true, enforceUTC = false)._1.toDouble, 1)

          val firstValue = entriesAtRow.head.valueStr

          val lastValue = entriesAtRow.last.valueStr

          val noOfFiles = entriesAtRow.distinctBy((entry: StatsEntry) => entry.runIDTransient).size

          columnIndex match {

            case 0 => {

              if (noOfFiles > 1) // (i.e., we have a group of singleton entries created with groupSingletonKeyAcrossFiles=true)
              fileNameFromFullPath(runIDToStats(entriesAtRow(0).runIDTransient).problemFile) + " (" + noOfFiles + " files)"
                else
                problemDescription(entriesAtRow(0).runIDTransient)

            }

            case 1 => if (entriesAtRow(0).threadNo == 0) "" else "$" + entriesAtRow(0).threadNo

            case 2 => {

              if (noOfFiles > 1)
                ""
              else
                minMessageTimeSec + " .. " + maxMessageTimeSec

            }

            case 3 => entriesAtRow(0).key.asInstanceOf[AnyRef]

            case 4 => firstValue + " .." + lastValue + " (" + entriesAtRow.size + " values)"

          }

        } else
          "???"

      }

    }

    var colorIndex = -1

    val lineColors = Seq(HTMLNamedColors.indianRed, HTMLNamedColors.cornflowerBlue, HTMLNamedColors.darkOrange, HTMLNamedColors.forestGreen,
      HTMLNamedColors.blueViolet, HTMLNamedColors.peru, HTMLNamedColors.slateGray,
      HTMLNamedColors.darkSalmon, HTMLNamedColors.yellowGreen, HTMLNamedColors.crimson, HTMLNamedColors.mediumAquaMarine,
      HTMLNamedColors.mediumPurple) // length of this sequence is also maximum number of x-values for which a bar chart is used.

    val lineStyles = Seq(LineStyle.Solid, LineStyle.Solid /* LineStyle.DashDot, LineStyle.Dashed, LineStyle.Dotted*/)
    // ^To work with the mouse hover feature for selecting files, a solid curve with a distinctive color is required.

    var lineStyleIndex = 0

    val fileCurveColorForHover = mutable.HashMap[Color, String /*runID as string(!)*/ ]() // for curve selection by mouse hover

    var runIDHover: String = null // UUID-as-a-string for curve selection by mouse hover

    var exportCurveDataToFileOpt: Option[(File, Int /*0:Gnuplot, 1:CSV*/ )] = None

    setLayout(new BorderLayout())

    // The list with the fields and values in the JSON stats files (goes in the centre of the frame panel):

    val entriesJTableModel = new EntriesJTableModel()

    val entriesJTable = new JTable(entriesJTableModel)

    entriesJTable.getColumnModel().getColumn(0).setPreferredWidth(compWidth / 5 /*330*/)

    entriesJTable.getColumnModel().getColumn(1).setPreferredWidth(compWidth / 5 / 5 /*60*/)

    entriesJTable.getColumnModel().getColumn(2).setPreferredWidth(compWidth / 5 / 3 /*100*/)

    entriesJTable.getColumnModel().getColumn(3).setPreferredWidth(compWidth / 5 /*200*/)

    entriesJTable.getColumnModel().getColumn(4).setPreferredWidth(compWidth / 5 /*220*/)

    //entriesJTable.setPreferredSize(new Dimension(compWidth, compHeight))  // messes up vertical scroll bar

    entriesJTable.setDefaultRenderer(classOf[AnyRef], new DefaultTableCellRenderer() {

      override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {

        val component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

        if (entriesJTable.getModel.getValueAt(row, 0).toString.contains(" files)")) // TODO: replace this quick hack
        component.setForeground(Color.BLUE)
          else if (entriesJTable.getModel.getValueAt(row, 3).toString.toLowerCase.equals("timedout") &&
          entriesJTable.getModel.getValueAt(row, 4).toString.toLowerCase.equals("true") ||
          entriesJTable.getModel.getValueAt(row, 3).toString.toLowerCase.equals("targetcostreached") &&
            entriesJTable.getModel.getValueAt(row, 4).toString.toLowerCase.equals("false") ||
          entriesJTable.getModel.getValueAt(row, 3).toString.toLowerCase.equals("targetnoofmodelsreached") &&
            entriesJTable.getModel.getValueAt(row, 4).toString.toLowerCase.equals("false"))
          component.setForeground(Color.RED)
        else
          component.setForeground(Color.BLACK)

        component

      }

    })

    var plotImage = new BufferedImage(compWidth, compHeight, BufferedImage.TYPE_INT_RGB)

    val g2 = plotImage.createGraphics

    g2.setBackground(java.awt.Color.WHITE)

    g2.clearRect(0, 0, plotImage.getWidth, plotImage.getHeight)

    // In the east, we put the graph plot area (top) combined with an option panel with switches etc:

    val plotAndOptionsPanel = new JPanel()

    plotAndOptionsPanel.setLayout(new GridLayout(2, 1))

    val plotPanel = new JPanel() {

      override def paintComponent(g: Graphics) = {

        super.paintComponent(g)

        g.drawImage(plotImage, 0, 0, this)

      }

      override def getPreferredSize() = new Dimension(compWidth, compHeight)

    }

    plotPanel.addMouseMotionListener(new MouseMotionListener {

      override def mouseDragged(e: MouseEvent): Unit = ()

      override def mouseMoved(e: MouseEvent): Unit = { // hovering over a curve updates the info boxes

        if (updateFileInfoOnCurveMouseHover.isSelected) { // works only well enough if curve lines are solid and colors are distinctive enough:

          val x = e.getLocationOnScreen.x

          val y = e.getLocationOnScreen.y

          val colorsAroundXY = (y - 5 to y + 5).map(yy => new Robot().getPixelColor(x, yy)).map(c => (c.getRed, c.getGreen, c.getBlue))

          val mostDistinctiveFromWhite = colorsAroundXY.sortBy((c: (Int, Int, Int)) =>
            Math.sqrt(Math.pow(255 - c._1, 2) + Math.pow(255 - c._2, 2) + Math.pow(255 - c._3, 2))
          ).last

          val pixelColor = new Color(mostDistinctiveFromWhite._1, mostDistinctiveFromWhite._2, mostDistinctiveFromWhite._3) //new Robot().getPixelColor(x, y)

          if (pixelColor.getRed + pixelColor.getBlue + pixelColor.getGreen < 250 * 3 &&
            pixelColor.getRed + pixelColor.getBlue + pixelColor.getGreen > 30) {

            val runID: String = fileCurveColorForHover.toSeq.sortBy((colorRunId: (Color, String)) => {

              val distance: Double = Math.sqrt(Math.pow(pixelColor.getRed - colorRunId._1.getRed, 2) + Math.pow(pixelColor.getGreen - colorRunId._1.getGreen, 2) + Math.pow(pixelColor.getBlue - colorRunId._1.getBlue, 2))

              distance

            }).headOption.getOrElse((null, null))._2

            //println(runIDToStats(runID).problemFile)

            if (runIDHover == null || !runID.equals(runIDHover)) {

              runIDHover = runID

              entriesTableSelectionListener.valueChanged_(new ListSelectionEvent(entriesTableSelectionModel,
                0, 0, false), runID)

              updateFileInfoArea(runIDToStats(runID), runIDToStats(runID).problemFile)

            }

          }

        }

      }
    })

    plotPanel.setMinimumSize(new Dimension(compWidth, compHeight))

    plotAndOptionsPanel.add(plotPanel)

    val optionsPanel = new JPanel()

    optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS))

    val useAbsoluteTimesForSingletonEntitiesGroupedAcrossFilesSwitch = new JCheckBox("Use global dates/times on x-axis for groups of entries with same key but from different files",
      useAbsoluteTimesForSingletonEntitiesGroupedAcrossFiles)

    val groupSingletonKeyAcrossFilesSwitch = new JCheckBox("Group singleton entries with same key across different selected files",
      groupSingletonKeyAcrossFiles)

    val spreadCurveTimes = new JCheckBox("Replace curve x-values with evenly distributed values from 0 to max x-value of all selected curves", false)

    val showDataPointsHistogram = new JCheckBox("Show histogram of x data points distribution",
      true)

    val enforceScatterPlot = new JCheckBox("Replace line plots with scatter plots",
      false)

    val useBarChartForSmallDatasets = new JCheckBox("Use a bar chart if the number of data points is small",
      true)

    val updateFileInfoOnCurveMouseHover = new JCheckBox("Update file/run/entries info when hovering mouse over curve or bar",
      true)

    val exportPlotDataToGnuplotFile = new JButton("Export plot data for Gnuplot")

    exportPlotDataToGnuplotFile.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        import javax.swing.JFileChooser
        import javax.swing.filechooser.FileNameExtensionFilter

        val chooser = new JFileChooser

        val filter = new FileNameExtensionFilter("Gnuplot format", "dat")

        chooser.setFileFilter(filter)

        val returnVal = chooser.showOpenDialog(null)

        if (returnVal == JFileChooser.APPROVE_OPTION) {

          refreshFileAndEntryLists(reselect = true, writePlotToFileOpt = Some((chooser.getSelectedFile, 0)))

        }

      }

    })

    val exportPlotDataToCSVFile = new JButton("Export plot data as CSV")

    exportPlotDataToCSVFile.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        import javax.swing.JFileChooser
        import javax.swing.filechooser.FileNameExtensionFilter

        val chooser = new JFileChooser

        val filter = new FileNameExtensionFilter("CSV", "csv")

        chooser.setFileFilter(filter)

        val returnVal = chooser.showOpenDialog(null)

        if (returnVal == JFileChooser.APPROVE_OPTION) {

          refreshFileAndEntryLists(reselect = true, writePlotToFileOpt = Some((chooser.getSelectedFile, 1)))

        }

      }

    })

    val refreshFromDir = new JButton("Refresh from directory")

    refreshFromDir.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        initializeFileListFromDir(commandline.Main.writeStatsDirectory)

        initialize()

        refreshFileAndEntryLists(reselect = false)

      }

    })

    val openDirectory = new JButton("Add files")

    openDirectory.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        import javax.swing.JFileChooser
        val f = new JFileChooser

        f.setMultiSelectionEnabled(true)

        f.showSaveDialog(null)

        val filesToAdd = f.getSelectedFiles

        fileList.appendAll(filesToAdd)

        initialize()

        refreshFileAndEntryLists(reselect = false)

      }

    })

    def refreshFileAndEntryLists(selectFilteredContent: Boolean = false, reselect: Boolean = false,
                                 writePlotToFileOpt: Option[(File, Int)] = None) = {

      //populateFileListModel()

      val existingSelectionsFile = (0 until fileTableModel.getRowCount).filter(fileTableSelectionModel
        .isSelectedIndex(_))

      val existingSelectionsEntries: Seq[Int] = (0 until entriesJTableModel.getRowCount).filter(entriesTableSelectionModel.isSelectedIndex(_))

      fileTableSelectionModel.clearSelection()

      entriesJTable.clearSelection()

      jTableFiles.getModel.asInstanceOf[AbstractTableModel].fireTableDataChanged()

      entriesJTable.getModel.asInstanceOf[AbstractTableModel].fireTableDataChanged()

      fileInfoArea.setText("")

      entryInfoArea.setText("")

      if (selectFilteredContent)
        fileTableSelectionModel.addSelectionInterval(0, jTableFiles.getModel.getRowCount - 1)
      else if (reselect) {

        existingSelectionsFile.foreach(selectionIndex => fileTableSelectionModel.addSelectionInterval(selectionIndex, selectionIndex))

        writePlotToFileOpt.foreach { case (file, format) => {

          exportCurveDataToFileOpt = Some((file, format))

          entriesTableSelectionModel.setValueIsAdjusting(true) // // so that the following series of selection additions is treated
          // as a single group of selection changes which triggers the plot data file writing only once (i.e., including all curves)


        }
        }

        existingSelectionsEntries.foreach(selectionIndex => entriesTableSelectionModel.addSelectionInterval(selectionIndex, selectionIndex))

        entriesTableSelectionModel.setValueIsAdjusting(false)

      }

    }

    useAbsoluteTimesForSingletonEntitiesGroupedAcrossFilesSwitch.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        useAbsoluteTimesForSingletonEntitiesGroupedAcrossFiles = e.getSource().asInstanceOf[JCheckBox].isSelected

        refreshFileAndEntryLists(reselect = true)

      }

    })

    groupSingletonKeyAcrossFilesSwitch.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        groupSingletonKeyAcrossFiles = e.getSource().asInstanceOf[JCheckBox].isSelected

        refreshFileAndEntryLists()

      }

    })

    spreadCurveTimes.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        refreshFileAndEntryLists(reselect = true)

      }

    })

    showDataPointsHistogram.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        refreshFileAndEntryLists(reselect = true)

      }

    })

    enforceScatterPlot.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        refreshFileAndEntryLists(reselect = true)

      }

    })

    useBarChartForSmallDatasets.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        refreshFileAndEntryLists(reselect = true)

      }

    })

    updateFileInfoOnCurveMouseHover.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent): Unit = {

        if (!updateFileInfoOnCurveMouseHover.isSelected)
          runIDHover = null

        refreshFileAndEntryLists(reselect = true)

      }

    })


    optionsPanel.add(groupSingletonKeyAcrossFilesSwitch)

    optionsPanel.add(useAbsoluteTimesForSingletonEntitiesGroupedAcrossFilesSwitch)

    optionsPanel.add(spreadCurveTimes)

    optionsPanel.add(showDataPointsHistogram)

    optionsPanel.add(enforceScatterPlot)

    optionsPanel.add(useBarChartForSmallDatasets)

    optionsPanel.add(updateFileInfoOnCurveMouseHover)

    optionsPanel.add(refreshFromDir)

    optionsPanel.add(openDirectory)

    optionsPanel.add(exportPlotDataToGnuplotFile)

    optionsPanel.add(exportPlotDataToCSVFile)


    val fileFilterPanel = new JPanel() {

      override def getMaximumSize = {

        val dim = super.getMaximumSize

        dim.width = getPreferredSize.width

        dim.height = getPreferredSize.height

        dim

      }
    }

    fileFilterPanel.setLayout(new BoxLayout(fileFilterPanel, BoxLayout.LINE_AXIS))

    fileFilterPanel.setAlignmentX(Component.LEFT_ALIGNMENT)

    fileFilterPanel.add(new JLabel("Filter files: "))

    val fileFilterInputTextField = new JTextField(30)

    fileFilterPanel.add(fileFilterInputTextField)

    optionsPanel.add(fileFilterPanel)

    fileFilterInputTextField.getDocument().addDocumentListener(new DocumentListener() {

      override def insertUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = false /*true*/)

      override def removeUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = false /*true*/)

      override def changedUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = false /*true*/)

    })

    val entriesFilterPanel = new JPanel() {

      override def getMaximumSize = {
        val dim = super.getMaximumSize

        dim.width = getPreferredSize.width

        dim.height = getPreferredSize.height

        dim

      }
    }

    entriesFilterPanel.setLayout(new BoxLayout(entriesFilterPanel, BoxLayout.LINE_AXIS))

    entriesFilterPanel.setAlignmentX(Component.LEFT_ALIGNMENT)

    entriesFilterPanel.add(new JLabel("Filter entries by key: "))

    val entriesFilterInputTextField = new JTextField(30)

    entriesFilterInputTextField.getDocument().addDocumentListener(new DocumentListener() {

      override def insertUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = true)

      override def removeUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = true)

      override def changedUpdate(e: DocumentEvent): Unit = refreshFileAndEntryLists(selectFilteredContent = true)

    })

    entriesFilterPanel.add(entriesFilterInputTextField)

    optionsPanel.add(entriesFilterPanel)

    plotAndOptionsPanel.add(optionsPanel)

    add(BorderLayout.EAST, plotAndOptionsPanel)

    // A text area which shows the content of the "settings" entry which is contained in all stats files:

    import javax.swing.JTextPane

    val fileInfoArea = new JTextPane()

    fileInfoArea.setContentType("text/html")

    fileInfoArea.setEditable(false)

    val scrollPaneFileInfoArea: JScrollPane = new JScrollPane(fileInfoArea)

    scrollPaneFileInfoArea.setPreferredSize(new Dimension(compWidth + compWidthAdaptWest, compHeight))

    import javax.swing.event.{HyperlinkEvent, HyperlinkListener}

    fileInfoArea.addHyperlinkListener(new HyperlinkListener() {

      override def hyperlinkUpdate(e: HyperlinkEvent): Unit = {

        if (HyperlinkEvent.EventType.ACTIVATED == e.getEventType) {

          val description = e.getDescription

          val docHTML = fileInfoArea.getDocument.asInstanceOf[HTMLDocument]

          // TODO: There must be a simpler and more accurate way to scroll to a local anchor than the following:

          val posMax = docHTML.getEndPosition.getOffset

          var posCheck = 1

          var anchorFound = false

          while (!anchorFound && posCheck <= posMax) {

            val selText = docHTML.getCharacterElement(posCheck).getAttributes.getAttribute(HTML.Tag.A)

            anchorFound = selText != null && selText.toString.contains("name=" + description.trim.stripPrefix("#").trim)

            posCheck += 1

          }

          if (anchorFound) // TODO: make this precise, also if text pane size has changed:
          fileInfoArea.setCaretPosition((posCheck + 500).
            min(docHTML.getLength - 1)) // an approximation...

        }

      }

    })

    // A text area which shows information about a (group of) entries selected in the entries area:

    val entryInfoArea = new JTextArea(5, 80)

    entryInfoArea.addMouseListener(new MouseAdapter() {

      override def mouseClicked(e: MouseEvent) = {

        val line = entryInfoArea.getLineOfOffset(entryInfoArea.getCaretPosition)

        val start = entryInfoArea.getLineStartOffset(line)

        val end = entryInfoArea.getLineEndOffset(line)

        val text = entryInfoArea.getDocument.getText(start, end - start)

        val dateTimeIndices: Option[(Int, Int)] = raw"@\[[^\]]+\]".r.findFirstMatchIn(text).map(m => (m.start, m.end))

        if (dateTimeIndices.isDefined) { // user has clicked on a highlighted file date/time in the text area

          val localDateTimeClickedStr = text.substring(dateTimeIndices.get._1, dateTimeIndices.get._2).stripPrefix("@[").stripSuffix("]")

          /*for (jsonFile <- fileList)*/ for ((runID, _) <- getAllSelectedFileRunIDsWithSelectionIndex) {

            val statsFromFile: Stats = runIDToStats(runID) //readStatsFromFile(jsonFile)

            val localDateTimeInFileStr = instantMsToLocalZoneDateTimeFormatted(statsFromFile.runStartTimeMs)

            if (localDateTimeInFileStr == localDateTimeClickedStr) {

              updateFileInfoArea(statsFromFile, statsFromFile.problemFile)

            }

          }


        }

      }

    })

    val entriesTableSelectionModel = entriesJTable.getSelectionModel

    val entriesTableSelectionListener = new ListSelectionListener {

      override def valueChanged(event: ListSelectionEvent): Unit = {

        runIDHover = null

        valueChanged_(event, overrideShowInfoOnRunID = null)

      }

      def valueChanged_(event: ListSelectionEvent, overrideShowInfoOnRunID: String = null): Unit = {

        val entriesTableSelectionModel: ListSelectionModel = event.getSource().asInstanceOf[ListSelectionModel]

        val selectedEntrySets = ArrayBuffer[(ArrayBuffer[StatsEntry], Int)]()

        if (!entriesTableSelectionModel.isSelectionEmpty) {

          selectedEntrySets.addAll(allEntriesFromAllSelectedJsonFilesGrouped.zipWithIndex.filter((tuple: (ArrayBuffer[StatsEntry], Int)) =>
            tuple._2 >= entriesTableSelectionModel.getMinSelectionIndex() &&
              tuple._2 <= entriesTableSelectionModel.getMaxSelectionIndex() &&
              entriesTableSelectionModel.isSelectedIndex(tuple._2)))

        }


        val recentSelectedIndex: Int = if (overrideShowInfoOnRunID == null) entriesTableSelectionModel.getLeadSelectionIndex
        else selectedEntrySets.find((tuple: (ArrayBuffer[StatsEntry], Int)) => tuple._1.head.runIDTransient.equals(overrideShowInfoOnRunID)).getOrElse(null, 0)._2

        val entriesComeFromMultipleFiles = selectedEntrySets.exists(_._1.groupBy(_.runIDTransient).toSeq.length > 1)

        val entriesAllWithSameKey = selectedEntrySets.map(_._1).flatten.groupBy(_.key).toSeq.length == 1

        val useUTCtimesOnXaxis: Boolean = if (entriesTableSelectionModel.isSelectionEmpty) false else
          useAbsoluteTimesForSingletonEntitiesGroupedAcrossFiles && entriesComeFromMultipleFiles &&
            entriesAllWithSameKey /*runIDToStats(selectedEntrySets.head._1.head.runIDTransient).countEntriesWithKey(selectedEntrySets.head._1.head.key, 2) == 1*/
        // ^ we need to decide this for all curves, as otherwise we'd get a meaningless mix of relative and global
        // times on the x-axis. So even if a entry group doesn't consist of data from different files, if useUTCtimesOnXaxis=true,
        // its x-values will be replaced with the UTC time of the run start.

        val showMillisecInsteadSecOnX: Boolean = selectedEntrySets.foldLeft(false) { case (ub, (entryGroup: ArrayBuffer[StatsEntry], _: Int)) => {

          val minMessageTimeMs = getRectifiedTimeStampOfEntryInMsOrSec(entryGroup.minBy(entry => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = false, enforceUTC = useUTCtimesOnXaxis)._1), convertToSec = false, enforceUTC = useUTCtimesOnXaxis)._1.toDouble

          val maxMessageTimeMs = getRectifiedTimeStampOfEntryInMsOrSec(entryGroup.maxBy(entry => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = false, enforceUTC = useUTCtimesOnXaxis)._1), convertToSec = false, enforceUTC = useUTCtimesOnXaxis)._1.toDouble

          ub || (maxMessageTimeMs - minMessageTimeMs < 10l * 1000l)

        }
        }


        val curvesOriginal: mutable.Seq[(Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])] = selectedEntrySets.map { case (entryGroup: ArrayBuffer[StatsEntry], index: Int) => {

          val pointsOnCurve: Seq[Point] = entryGroup.map((entry: StatsEntry) => {

            Point((getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = !showMillisecInsteadSecOnX,
              enforceUTC = useUTCtimesOnXaxis)._1).toDouble, {

              parseDoubleOrElse(entry.valueStr)

            })

          }).toSeq

          val minXTimeMsOrSec = pointsOnCurve.minBy(_._1)._1

          val maxXTimeMsOrSec = pointsOnCurve.maxBy(_._1)._1

          //getRectifiedTimeStampOfEntryInMsOrSec(entryGroup.minBy(entry => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = !useMillisecOnX, enforceUTC = useUTCtimesOnXaxis)._1), convertToSec = !useMillisecOnX, enforceUTC = useUTCtimesOnXaxis)._1.toDouble

          val minYValue = parseDoubleOrElse(entryGroup.minBy(entry => parseDoubleOrElse(entry.valueStr)).valueStr)

          val maxYValue = parseDoubleOrElse(entryGroup.maxBy(entry => parseDoubleOrElse(entry.valueStr)).valueStr)

          if (index == recentSelectedIndex && entriesTableSelectionModel.isSelectedIndex(recentSelectedIndex)) {

            val highlighter = entryInfoArea.getHighlighter

            val painter = new DefaultHighlighter.DefaultHighlightPainter(Color.pink)

            def compHeader(entry: StatsEntry, useEventTime: Boolean = true) = {

              "Entry group information" + (if (true || useUTCtimesOnXaxis) " (click in highlighted areas for file/run information):" else "") + "\n\nFormula: " + runIDToStats(entry.runIDTransient).problemFile +
                (if (entriesComeFromMultipleFiles /*useUTCtimesOnXaxis*/ )
                "\n(" + selectedEntrySets.head._1.size + " singleton key entries from " + selectedEntrySets.head._1.size + " files)"
                else
                "\nRun start: @[" + instantMsToLocalZoneDateTimeFormatted(runIDToStats(entry.runIDTransient).runStartTimeMs) + "]") +
                (if (entry.threadNo == 0) "" else "\nThread: $" + entry.threadNo) +
                "\nKey: " + entry.key +
                (if (useEventTime) " created at @ms: " + entry.messageTimeStamp / 1000000l + " after run start" else "")
            }

            val (infoStr, header) = if (entryGroup.size == 1) ("Value: " + entryGroup.head.valueStr,
              compHeader(entryGroup.head)) else {

              (entryGroup.sortBy((entry: StatsEntry) => getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = false, enforceUTC = useUTCtimesOnXaxis)).map((entry: StatsEntry) => {

                val utc: (Double /*ms or sec*/ , Boolean) = getRectifiedTimeStampOfEntryInMsOrSec(entry, convertToSec = false, enforceUTC = useUTCtimesOnXaxis)

                //assert(useUTCtimesOnXaxis == utc._2)

                if (!utc._2)
                  "@" + (entry.messageTimeStamp / 1000000l) + "ms after run start: value " + entry.valueStr
                else
                  "@[" + instantMsToLocalZoneDateTimeFormatted(utc._1.toLong) + "]: value " + entry.valueStr

              }).mkString("\n"), compHeader(entryGroup.head, useEventTime = false))

            }

            val newText = header + "\n\n" + infoStr

            entryInfoArea.setText(newText)

            entryInfoArea.setCaretPosition(0)

            if (true || useUTCtimesOnXaxis) { // TODO: hack, might use JList or JTree instead. On the other hand, this could easily be
              // extended for further clickable words/subareas:

              val fileDateIndices: Seq[(Int, Int)] = raw"@\[[^\]]+\]".r.findAllMatchIn(newText).map(m => (m.start, m.end)).toList

              val r = fileDateIndices.foreach(fromTo =>
                highlighter.addHighlight(fromTo._1, fromTo._2, painter))


            }

          }

          (pointsOnCurve, (minXTimeMsOrSec, maxXTimeMsOrSec), (minYValue, maxYValue), entryGroup)

        }
        }

        if (!curvesOriginal.isEmpty) {

          val maxXmultiCurvesOriginal = curvesOriginal.maxBy((tuple: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => tuple._2._2)._2._2

          val curvesR: mutable.Seq[(Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])] = if (spreadCurveTimes.isSelected) {

            curvesOriginal.map((curve: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => {

              val numberOfXvalues = curve._1.length

              if (numberOfXvalues > 1) {

                val xValueDistanceEqualized = maxXmultiCurvesOriginal / (numberOfXvalues - 1).toDouble

                val spreadCurvePoints: Seq[Point] = curve._1.zipWithIndex.map((tuple: (Point, Int)) =>
                  new Point(xValueDistanceEqualized * tuple._2, tuple._1._2))

                assert(spreadCurvePoints.head._1 == 0d)

                //roughly: assert(spreadCurvePoints.last._1 == maxXmultiCurvesOriginal)

                val r: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry]) = (spreadCurvePoints, (spreadCurvePoints.head._1, spreadCurvePoints.last._1), curve._3, curve._4)

                r

              } else { // if there is only one data point and spreadCurveTimes is selected, we make the curve a constant line

                val r: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry]) = (scala.Seq(Point(0d, curve._1.head._2), Point(maxXmultiCurvesOriginal, curve._1.head._2)), (0d, maxXmultiCurvesOriginal), curve._3, curve._4)

                //println(r)

                r

              }

            })

          } else
          // we need to sort by x-value as this is how evilplot expects the curves (otherwise the plot would be messed up if there are any out-of-order points)
            curvesOriginal.map((curve: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => {

              val sortedCurvePointsAndEntries: (Seq[Point], Seq[StatsEntry]) = curve._1.zip(curve._4).sortBy { case (point, entry) => point._1 }.unzip

              (sortedCurvePointsAndEntries._1,
                (sortedCurvePointsAndEntries._1.head._1, sortedCurvePointsAndEntries._1.last._1),
                curve._3,
                sortedCurvePointsAndEntries._2.to(ArrayBuffer))

            })

          val xTickCountOpt: Option[Int] = None

          val minXmultiCurves: Double = curvesR.minBy((tuple: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => tuple._2._1)._2._1

          val maxXmultiCurves: Double = curvesR.maxBy((tuple: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => tuple._2._2)._2._2

          val minYmultiCurves: Double = curvesR.minBy((tuple: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => tuple._3._1)._3._1

          val minYmultiCurvesIncluding0: Double = 0d.min(minYmultiCurves)

          val maxYmultiCurves: Double = curvesR.maxBy((tuple: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) => tuple._3._2)._3._2

          fileCurveColorForHover.clear()

          lazy val curvesFlattened: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry]) =
            curvesR.reduce((value: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry]),
                            value1: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])) =>
              (value._1 ++ value1._1,
                (value._2._1.min(value1._2._1), value._2._2.max(value1._2._2)),
                (value._3._1.min(value1._3._1), value._3._2.max(value1._3._2)),
                value._4 ++ value1._4))

          val useBarChart = useBarChartForSmallDatasets.isSelected &&
            curvesFlattened._1.length >= 2 && curvesFlattened._1.length < lineColors.size

          val curves = if (useBarChart) mutable.Seq(curvesFlattened) else curvesR
          // Observe that at this point the x-values of all curves are either in ms or sec, and either relative times (from delSAT run start)
          // or global UTC times (ms or sec since 1/1/1970). Mixing UTC and relative times isn't possible, since useUTCtimesOnXaxis
          // enforces the same mode for all curves.

          if (!event.getValueIsAdjusting)
            exportCurveDataToFileOpt.foreach { case (outFile: File, format: Int) => {

              exportCurveDataToFileOpt = None

              val csv = format == 1

              val gnuplot = format == 0

              import java.io.BufferedWriter
              import java.io.FileWriter
              import java.io.IOException
              import java.io.PrintWriter

              try {

                val fw = new FileWriter(outFile, false)

                val bw = new BufferedWriter(fw)

                val out = new PrintWriter(bw)

                try {

                  // Plot using Gnuplot like this: plot 'demo.dat' using 5:6 with lines

                  curves.zipWithIndex.foreach { case ((curve: (Seq[Point], (Double, Double), (Double, Double), ArrayBuffer[StatsEntry])), index) => {

                    val unit = "ms" //if(showMillisecInsteadSecOnX) "ms" else "sec"

                    val sep = if (csv) "," else " "

                    if (index == 0 || gnuplot) {

                      if (useUTCtimesOnXaxis)
                        out.println("#source(runID)" + sep + "thread" + sep + "problemFile" + sep + "key" + sep + "UTC" + unit + sep + "value")
                      else
                        out.println("#source(runID)" + sep + "thread" + sep + "problemFile" + sep + "key" + sep + unit + "FromRunStart" + sep + "entryValue")

                    }

                    curve._1.zip(curve._4).foreach { case (point, entry) => {

                      val x = if (!showMillisecInsteadSecOnX) (point._1 * 1000d).toLong else point._1.toLong
                      // x = either ms since run start or UTC[ms] (ms since 1/1/1970), depending on useUTCtimesOnXaxis

                      val y = point._2

                      val stats = runIDToStats(entry.runIDTransient)

                      out.println("\"" + entry.runIDTransient + "\"" + sep + entry.threadNo + sep + "\"" + stats.problemFile + "\"" + sep + entry.key + sep + x + sep + y)

                    }
                    }

                    if (gnuplot) {

                      out.println

                      out.println

                    }

                  }
                  }

                  println("Curve data written to file " + outFile.getAbsolutePath)

                } catch {

                  case e: IOException => System.err.println(e)

                } finally {

                  if (out != null) out.close()

                  if (bw != null) bw.close()

                  if (fw != null) fw.close()

                }

              }

            }
            }

          val plots: List[Plot] = curves.zipWithIndex.flatMap { case (curveMinMaxEntryGroup: ((Seq[Point]), _, _, ArrayBuffer[StatsEntry]), index: Int) => {

            import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

            val pathRendererOpt: Option[PathRenderer[Point]] = {

              colorIndex = index

              val color = lineColors(colorIndex % (lineColors.length - 1))

              val ls = lineStyles(lineStyleIndex % (lineStyles.length - 1))

              lineStyleIndex = index

              val firstEntryInGroup = curveMinMaxEntryGroup._4.head

              fileCurveColorForHover.put(new Color(color.rgba._1, color.rgba._2, color.rgba._3), firstEntryInGroup.runIDTransient) // (for mouse hover file info)

              val stats = runIDToStats(firstEntryInGroup.runIDTransient)

              val fileName = fileNameFromFullPath(stats.problemFile)

              val curveLabel = firstEntryInGroup.key + ", " + (if (firstEntryInGroup.threadNo == 0) "" else "$" + firstEntryInGroup.threadNo + " in ") + fileName +
                (if (useUTCtimesOnXaxis) "" /*<- in this case the global date+time doesn't correspond to the entire curve but to
                values on x-axis*/ else ", " +
                  instantMsToLocalZoneDateTimeFormatted(stats.runStartTimeMs) /*<- relative times from run start, not UTC times*/)

              Some(PathRenderer.named(curveLabel, color,
                Some(if (firstEntryInGroup.runIDTransient == runIDHover) 2d else 1.2d /*1.1d*/), Some(ls)))

            }

            val p =
              if (enforceScatterPlot.isSelected)
                ScatterPlot(curveMinMaxEntryGroup._1,

                  pointRenderer = Some(PointRenderer.default[Point](
                    color = Some(lineColors(index % (lineColors.length - 1))),
                    pointSize = Some(1.1d)
                  ))

                ) //.trend(1, 0, color = RGB(45, 45, 45), lineStyle = LineStyle.DashDot)
              else if (useBarChart) {

                val theme = DefaultTheme /*.copy(
                  fonts = DefaultFonts
                    .copy(tickLabelSize = 15, legendLabelSize = 13, fontFace = "'Lato', sans-serif")
                )*/

                val labels: Seq[String] = curveMinMaxEntryGroup._1.zipWithIndex.map { case (x, index) =>
                  if (index % 3 == 0) {
                    if (!useUTCtimesOnXaxis) "@" + (if (showMillisecInsteadSecOnX) (x._1.toLong + "ms") else x._1.toLong + "sec") else xToTimeZonedDateTimeTickLabel(showMillisecInsteadSecOnX, x._1)
                  } else
                    ""
                }

                val barRenderer = new BarRenderer {

                  def render(plot: Plot, extent: Extent, category: Bar): Drawable = {

                    val barLabel: String = category.labels.head.asInstanceOf[Text].msg

                    val runId = barLabel.dropWhile(_ != '#').stripPrefix("#").trim

                    val rect = Rect(extent)

                    val color = category.colors.head

                    fileCurveColorForHover.put(new Color(color.rgba._1, color.rgba._2, color.rgba._3),
                      runId)

                    val sd: Seq[Drawable] = if (runId == runIDHover) Seq(BorderRect(extent.width, extent.height).weighted(2d)) else Seq()

                    val d: Drawable = Align.centerSeq(sd.+:(rect.filled(color)).:+(
                      Text(barLabel.takeWhile(_ != '#'), size = 11)
                        .filled(HTMLNamedColors.red /*theme.colors.label*/))
                    ).group

                    d

                  }

                }

                BarChart
                  .custom(curveMinMaxEntryGroup._1.zipWithIndex.map { case (xy: Point, index: Int) => {

                    val xTime = xy._1

                    val statsForBar: Stats = runIDToStats(curveMinMaxEntryGroup._4(index).runIDTransient)

                    val timePartLabel = if (!useUTCtimesOnXaxis) xTime.toLong.toString else xToTimeZonedDateTimeTickLabel(showMillisecInsteadSecOnX, xTime)

                    val barLabel = (if (useUTCtimesOnXaxis) fileNameFromFullPath(statsForBar.problemFile) else statsForBar.runID /*<- .outFile==null?!*/) +
                      "@" + timePartLabel + "#" + statsForBar.runID

                    val y: Double = xy._2

                    val color = lineColors(index % (lineColors.length - 1))

                    val bar = new Bar(Seq(y), 0, Seq(color),
                      labels = Seq(Text(barLabel)))

                    bar

                  }
                  }, spacing = Some(5),
                    barRenderer = Some(barRenderer)
                  ).ybounds(minYmultiCurvesIncluding0, maxYmultiCurves)
                  .xbounds(minXmultiCurves, maxXmultiCurves)
                  .standard(xLabels = labels) // for unknown reasons (probably because bars are somehow determined by labels on the axes?) .
                // .standard(xLabels=...) is required or the bars won't show up (so we cannot use the overlay axis further below)


              } else
                LinePlot(curveMinMaxEntryGroup._1,

                  pathRenderer = pathRendererOpt

                )

            if (showDataPointsHistogram.isSelected && curveMinMaxEntryGroup._1.size > 2) {

              val xhist = Histogram(curveMinMaxEntryGroup._1.map(_.x), bins = 40)

              Some(p.topPlot(xhist))

            } else
              Some(p)

          }
          }.toList

          import com.cibo.evilplot.plot.aesthetics.ClassicTheme._ //DefaultTheme._

          val rawYvaluesAreMs = selectedEntrySets.head._1.head.key.endsWith("Ms")

          val translateYvaluesFromMsToSec = rawYvaluesAreMs && maxYmultiCurves > 1000d

          var ticks = 0

          val r: Drawable = if (useBarChart) plots.head.render(new Extent(width = compWidth - 20, height = compHeight)) /*see
           above for why we don't use Overlay... with bar charts*/ else Overlay.fromSeq(plots).xAxis(

            tickCount = xTickCountOpt, // doesn't work properly; we need to reduce labels on X here since otherwise plot would attempt to
            // "interpolate" UTC times on the x-axis. Actually, this still happens occasionally even with
            // given tickCount (i.e., there might be "ghost file dates/times" on the x-axis). <- TODO

            labelFormatter = Some((xTime: Double) => {

              if (spreadCurveTimes.isSelected)
                "" // in this case showing the x-values wouldn't make sense, since multiple y-values for the same x-value could stem from different times.
              else if (!useUTCtimesOnXaxis)
                xTime.toLong.toString
              else {

                ticks += 1

                if (ticks % 2 == 0) {

                  xToTimeZonedDateTimeTickLabel(showMillisecInsteadSecOnX, xTime)

                } else
                  ""

              }

            })
          )
            .title("")
            .yAxis(labelFormatter = Some(yValue => {

              if (!translateYvaluesFromMsToSec)
                yValue.toString
              else {

                (yValue / 1000d).toString

              }

            }))
            .frame().
            ybounds(minYmultiCurvesIncluding0, maxYmultiCurves).
            xbounds(minXmultiCurves, maxXmultiCurves)
            //.rightLegend()
            .overlayLegend(x = 1d /*0.2*/ ,
              y = if (minYmultiCurves > (maxYmultiCurves - minYmultiCurvesIncluding0) / 2) 0.8 else 0.2)
            .xLabel(if (spreadCurveTimes.isSelected) "(Uniformly replaced x-values)" else if (useUTCtimesOnXaxis) "Date/time of program run (might comprise interpolations)" else (if (showMillisecInsteadSecOnX) "@ms" else "@sec"), size = Some(13d /*px*/))
            .yLabel((if (translateYvaluesFromMsToSec) "value (sec)" else if (rawYvaluesAreMs) "value (ms)" else "value"), size = Some(13d /*px*/)).render(Extent(compWidth, compHeight))


          implicit class DrawableAdditionalMethods(drawable: Drawable) {

            def myAsBufferedImage: BufferedImage = {

              val scale = 1.0 //4.0

              val paddingHack = 20

              val bufImage = new BufferedImage((r.extent.width * scale.toInt).toInt.max(1),
                (r.extent.height * scale).toInt.max(1),
                BufferedImage.TYPE_INT_RGB)

              val gfx = bufImage.createGraphics()

              gfx.scale(scale, scale)

              gfx.setBackground(java.awt.Color.white);

              gfx.clearRect(0, 0, bufImage.getWidth, bufImage.getHeight);

              val padded = r.padAll(paddingHack / 2)

              fit(padded, r.extent).draw(Graphics2DRenderContext(gfx))

              gfx.dispose()

              bufImage

            }

          }

          plotImage = r.myAsBufferedImage

        } else {

          plotImage = new BufferedImage(compWidth, compHeight, BufferedImage.TYPE_INT_RGB)

          val g2 = plotImage.createGraphics

          g2.setBackground(java.awt.Color.WHITE)

          g2.clearRect(0, 0, plotImage.getWidth, plotImage.getHeight)

        }

        plotPanel.repaint()

      }

    }

    def xToTimeZonedDateTimeTickLabel(useMillisecOnX: Boolean, xTime: Double): String = {

      val xTimeMs = (if (useMillisecOnX) xTime else xTime * 1000l).toLong

      val td = instantMsToLocalZoneDateTimeFormatted(xTimeMs) // (so while xTimeMs is in UTC-millisec,
      // we actually show the date/time in the *local* time zone on the x-axis)

      val timeRegex = raw"(\d{1,2}):(\d{1,2}):(\d{1,2})".r

      val dateRegex = raw"(\d{4})-(\d{2})-(\d{2})".r

      val t = timeRegex.findFirstMatchIn(td).map(m => m.group(1) + ":" + m.group(2))

      val d = dateRegex.findFirstMatchIn(td).map(m => m.group(1) + "-" + m.group(2) + "-" + m.group(3))

      t.getOrElse("?") + "/" + d.getOrElse("?")

    }

    entriesTableSelectionModel.addListSelectionListener(entriesTableSelectionListener)

    val scrollPaneEntryTable: JScrollPane = new JScrollPane(entriesJTable)

    scrollPaneEntryTable.setPreferredSize(new Dimension(compWidth, compHeight))

    val entriesPlusEntryInfoPanel = new JPanel()

    entriesPlusEntryInfoPanel.setLayout(new GridLayout(2, 1))

    val scrollPaneEntryInfoArea: JScrollPane = new JScrollPane(entryInfoArea)

    scrollPaneEntryInfoArea.setPreferredSize(new Dimension(compWidth, compHeight))

    entriesPlusEntryInfoPanel.add(scrollPaneEntryTable)

    entriesPlusEntryInfoPanel.add(scrollPaneEntryInfoArea)

    add(BorderLayout.CENTER, entriesPlusEntryInfoPanel)

    // The table with the JSON stats files:

    class FileJTableModel extends AbstractTableModel {

      override def getRowCount: Int = {

        val rc = getAllFileWithStatsFiltered.length //fileList.filter(_.getName.contains(fileFilterInputTextField.getText)).length

        //println("rc = " + rc)

        rc
      }

      override def getColumnCount: Int = {

        4

      }

      override def getColumnName(column: Int): String = {

        Seq("runID" /*<-- this MUST be present as column 0, as we use this also to obtains the stats objects from these files*/ ,
          "File", "Date/Time", "Sampling (sec)")(column)

      }

      def problemDescription(runID: String) = {

        val stats = runIDToStats.getOrElse(runID, null)

        val runStartTimeMsStr = Instant.ofEpochMilli(stats.runStartTimeMs).toString

        val problemName = fileNameFromFullPath(stats.problemFile)

        problemName + "[" + runStartTimeMsStr + "]"

      }

      override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {

        //(NB: if we call getValueAt for a row obtained from a click or selection into the JTable,
        // we need to translate the view row index in the model index using jTableFiles.convertRowIndexToModel(selectionRowIndex).)

        val fileAndStatsAtRow = getAllFileWithStatsFiltered(rowIndex)

        //println(" rowIndex: " +rowIndex + ", " + fileAndStatsAtRow._1)

        columnIndex match {

          case 0 => fileAndStatsAtRow._2.runID.asInstanceOf[AnyRef]

          case 1 => fileAndStatsAtRow._1.getName

          case 2 => new SimpleDateFormat("dd-MM-yyyy HH-mm-ss").format(
            new Date(fileAndStatsAtRow._1.lastModified())
          )

          case 3 => round(fileAndStatsAtRow._2.getFirstOpt("samplingTimeMs").map(statsEntry => statsEntry.valueStr.toDouble / 1000l).getOrElse(-1d), 2).toString

        }

      }

    }


    val fileTableModel = new FileJTableModel()

    val jTableFiles = new JTable(fileTableModel)

    jTableFiles.getColumnModel().getColumn(0).setPreferredWidth(20)

    jTableFiles.getColumnModel().getColumn(1).setPreferredWidth(200)

    jTableFiles.getColumnModel().getColumn(2).setPreferredWidth(100)

    jTableFiles.getColumnModel().getColumn(3).setPreferredWidth(70)

    jTableFiles.setAutoCreateRowSorter(true)

    jTableFiles.setDefaultRenderer(classOf[AnyRef], new DefaultTableCellRenderer() {

      override def getTableCellRendererComponent(table: JTable, valueR: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {

        val component = super.getTableCellRendererComponent(table, valueR, isSelected, hasFocus, row, column)

        val runID = fileTableModel.getValueAt(jTableFiles.convertRowIndexToModel(row), 0).toString.trim //.toLong

        runIDToStats.get(runID).foreach(stats => {

          if (stats.getFirstOpt("timedout").getOrElse(new StatsEntry()).valueStr == "true" ||
            stats.getFirstOpt("runCompleted").getOrElse(new StatsEntry()).valueStr == "false")
            component.setForeground(Color.RED)
          else if (stats.getFirstOpt("noOfMinibenchmarkTrials").map(_.valueStr).getOrElse("1") != "1") {

            if (stats.getFirstOpt("sampledModels").map(_.valueStr).getOrElse("0") != "1")
              component.setForeground(new Color(130, 10, 120))
            else
              component.setForeground(new Color(140, 10, 220))

          } else if (stats.getFirstOpt("sampledModels").map(_.valueStr).getOrElse("0") != "1")
            component.setForeground(new Color(150, 10, 70))
          else
            component.setForeground(Color.BLACK)

        })

        component

      }

    });

    val fileTableSelectionModel = jTableFiles.getSelectionModel

    def getAllFileWithStatsFiltered: Array[(File, Stats)] = {

      fileStatsList.filter { case (_, stats) => stats.problemFile.toLowerCase.contains(fileFilterInputTextField.getText.toLowerCase) }

    }

    def getAllSelectedFileRunIDsWithSelectionIndex: Seq[(String /*runID*/ , Int)] = {

      (fileTableSelectionModel.getMinSelectionIndex() to fileTableSelectionModel.getMaxSelectionIndex()).
        filter(fileTableSelectionModel.isSelectedIndex(_)).map(rowIndex =>
        (fileTableModel.getValueAt(jTableFiles.convertRowIndexToModel(rowIndex), 0 /*i.e., we fetch the
        runID from column 0 of the file table*/).toString.trim /*.toLong*/ , rowIndex))

      // observe that the returned index in ._2 is a view index (a selection index) whereas the
      // index passed into .getValueAt is translated to the model to account for sorting columns.
    }

    class FileTableSelectionListener extends ListSelectionListener {

      override def valueChanged(event: ListSelectionEvent): Unit = {

        val recentSelectedIndex = fileTableSelectionModel.getLeadSelectionIndex

        val selectedJsonFiles = ArrayBuffer[(String /*runID*/ , Int)]()

        entryInfoArea.setText("") // (why?)

        if (!fileTableSelectionModel.isSelectionEmpty) {

          selectedJsonFiles.addAll(getAllSelectedFileRunIDsWithSelectionIndex)
          // ^ observe that the returned indices are view indices (selection indices), not
          // model row indices (these may be different if the rows were sorted by clicking on column headers).

        }

        allEntriesFromAllSelectedJsonFiles.clear()

        fileInfoArea.setText("")

        allEntriesFromAllSelectedJsonFiles.addAll(selectedJsonFiles.flatMap { case (runID: String, selectedRowIndex: Int) => {

          val statsFromFile: Stats = runIDToStats(runID) //readStatsFromFile(file)

          //runIDToStats.put(statsFromFile.runID, statsFromFile)

          if (selectedRowIndex == recentSelectedIndex && fileTableSelectionModel.isSelectedIndex(recentSelectedIndex)) {

            updateFileInfoArea(statsFromFile, statsFromFile.problemFile)

          }

          JavaConverters.asScalaBuffer(statsFromFile.entries).filter(_.key.toLowerCase.contains(entriesFilterInputTextField.getText.toLowerCase))

        }
        })

        entriesJTableModel.fireTableDataChanged()

        allEntriesFromAllSelectedJsonFilesGrouped.clear()

        val grouped: Map[Int, ArrayBuffer[StatsEntry]] = allEntriesFromAllSelectedJsonFiles.groupBy((entry: StatsEntry) => {

          if (groupSingletonKeyAcrossFiles && runIDToStats(entry.runIDTransient).countEntriesWithKey(entry.key, maxCount = 2) == 1)
            (entry.key + fileNameFromFullPath(runIDToStats(entry.runIDTransient).problemFile)).hashCode
          else
            (entry.runIDTransient.toString + "," + entry.threadNo + "," + entry.key).hashCode

        })

        allEntriesFromAllSelectedJsonFilesGrouped.addAll(grouped.map(_._2).toSeq.sortBy((value: ArrayBuffer[StatsEntry]) => value.head.key))


      }

    }

    val fileTableSelectionListener = new FileTableSelectionListener()

    fileTableSelectionModel.addListSelectionListener(fileTableSelectionListener)

    def updateFileInfoArea(statsFromFile: Stats, fileName: String) = {

      val infoOpt = statsFromFile.getFirstOpt("settings")

      infoOpt.foreach(info => {

        val infoHTML = info.valueStr

        val localDateTimeStr = instantMsToLocalZoneDateTimeFormatted(statsFromFile.runStartTimeMs)

        val textPaneFontFamily = fileInfoArea.getFont().getFamily()

        val benchmarkTrials: Int = statsFromFile.getFirstOpt("noOfMinibenchmarkTrials").map(_.valueStr).getOrElse("1").toInt

        val header = "<html><body style=\"font-family: " + textPaneFontFamily +
          "\">File/run info for runID=" + statsFromFile.runID + " (in set of " + getAllSelectedFileRunIDsWithSelectionIndex.size + " selected files)" +
          "<p>Formula:<br>" + fileName +
          "<br>Solver: " + statsFromFile.getFirstOpt("solver").map(_.valueStr).getOrElse("?") + ", run start: " + localDateTimeStr +
          (if (statsFromFile.getFirstOpt("runCompleted").getOrElse(new StatsEntry()).valueStr == "false") "<br><span style=\"color:red\">Run aborted or timed out or maximum number of trials exceeded</span>" else "") +
          (if (benchmarkTrials > 1) "<br><span style=\"color:green\">Number of sampled models per trial: " +
            statsFromFile.getFirstOpt("sampledModels").map(_.valueStr).getOrElse("?") + "</span>" else "<br><span style=\"color:green\">Sampling time: " + statsFromFile.getFirstOpt("samplingTimeMs").map(v => v.valueStr + "ms (" + round(v.valueStr.toDouble / 1000d, 3) + " sec) (models: " + statsFromFile.getFirstOpt("sampledModels").map(_.valueStr).getOrElse("?") + ")").getOrElse("?") + "</span>") +
          (if (benchmarkTrials <= 1) "" else "<br><span style=\"color:#BA55D3\">Average benchmark trial time: " + statsFromFile.getFirstOpt("avgBenchmarkTrialTimeMs").map(v => v.valueStr + "ms (" + round(v.valueStr.toDouble / 1000d, 3) + " sec)").getOrElse("?") + " (number of trials: " + benchmarkTrials + ")</span>") +
          "<br>Overall time: " + statsFromFile.getFirstOpt("overallTimeMs").map(v => v.valueStr + "ms (" + round(v.valueStr.toDouble / 1000d, 3) + " sec)").getOrElse("?") +
          "<br><span style=\"color:green\">Successful thread: $" + statsFromFile.getFirstOpt("successfulThread").map(_.valueStr).getOrElse("?") + " out of " + statsFromFile.getFirstOpt("noOfThreads").map(_.valueStr).getOrElse("?") + " threads</span><br><b>Settings:</b><br>"

        val footer = "</body></html>"

        import javax.swing.text.html.HTMLEditorKit

        val doc = fileInfoArea.getDocument.asInstanceOf[HTMLDocument]

        val editorKit = fileInfoArea.getEditorKit.asInstanceOf[HTMLEditorKit]

        fileInfoArea.setText("")

        editorKit.insertHTML(doc, doc.getLength, header + infoHTML + footer, 0, 0, null)

      })

      fileInfoArea.setCaretPosition(0)

    }

    val filesPlusInfoPanel = new JPanel()

    filesPlusInfoPanel.setLayout(new GridLayout(2, 1))


    val scrollPaneFileList: JScrollPane = new JScrollPane(jTableFiles)

    scrollPaneFileList.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)

    scrollPaneFileList.setPreferredSize(new Dimension(compWidth + compWidthAdaptWest, compHeight))

    filesPlusInfoPanel.add(scrollPaneFileList)

    filesPlusInfoPanel.add(scrollPaneFileInfoArea)

    add(BorderLayout.WEST, filesPlusInfoPanel)


  }

  def show: Unit = {

    val frame: JFrame = new JFrame("Solver runtime stats dashboard")

    frame.setSize(compWidth * 3 - compWidthAdaptWest, compHeight * 2)

    frame.setVisible(true)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    frame.getContentPane.add(new FramePanel())

    frame.pack()

  }

}
