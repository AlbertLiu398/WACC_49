package wacc

import scala.collection.mutable
import instruction._
import constant._

object Labels {

    // HashMap to store all string data stored in program
    val allDataMsgs: mutable.LinkedHashMap[String, DataMsg] = mutable.LinkedHashMap.empty

    // Counter for naming data messages labels
    var dataMsgCounter = 0

    // counter for naming if labels
    var ifCounter = 0

    // counter for naming while labels
    var whileCounter = 0

    case class DataMsg(s: String, labelIndex: Int, actualSize: Int, name: String){
        val label: String = s".L.str$labelIndex"
        var instruction: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
            if (labelIndex == -1) {
                // Use customised label name
                instruction = mutable.ListBuffer(
                    I_Directive(s"word $actualSize"),
                    I_Label(s".L.$name"),
                    I_Directive(s"asciz " + "\"" + s + "\"")
                )
            } else {
                // Use counter to generate label name
                instruction = mutable.ListBuffer(
                    I_Directive(s"word $actualSize"),
                    I_Label(s".L.str$labelIndex"),
                    I_Directive(s"asciz " + "\"" + s + "\"")
                ) 
            }
    }

    // Generate a data message from a string and add to allDataMsgs
    def addDataMsg(s:String): String = {
        val result = addDataMsgWithLabel(s, dataMsgCounter, "")
        dataMsgCounter = dataMsgCounter + 1
        result
    }

    def addCustomisedDataMsg(s: String, labelName: String): String = {
        // If use customised label name, set label index to -1 (No need to generate name using counter)
        val result = addDataMsgWithLabel(s, -1 ,labelName)
        result
    }


    // Helper function for addDataMsg()
    def addDataMsgWithLabel(s: String, labelCounter: Int, customisedLabelName: String): String = {
        val len = s.length

        val msg = DataMsg(s, labelCounter, len, customisedLabelName)
        // Check whether msg already existed
        allDataMsgs.get(s) match {
            case None =>
                allDataMsgs.put(s, msg)
                // return the new created label string
                msg.label
            case Some(elem) =>
                // return existed label string
                elem.label
        }
    }

    def addIfLabel(): (String, String) ={
        ifCounter += 1
        (s"if_then_$ifCounter", s"if_end_$ifCounter")
    }

    def addWhileLabel(): (String, String) ={
        whileCounter += 1
        (s"w_condition_$whileCounter", s"w_body_$whileCounter")
    }
}