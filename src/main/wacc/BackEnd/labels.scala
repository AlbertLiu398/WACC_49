package wacc

import scala.collection.mutable
import instruction._
import constant._

object Labels {

    case class DataMsg(s: String, labelIndex: String, actualSize: Int) {
        val label: String = s".L.str_$labelIndex"
        val instruction: mutable.ListBuffer[Instruction] = 
            mutable.ListBuffer(
                I_Directive(s"word $actualSize"),
                I_Label(s".L.str_$labelIndex"),
                I_Directive(s"asciz " + "\"" + s + "\"")
            )
    }


    // HashMap to store all string data stored in program
    val allDataMsgs: mutable.LinkedHashMap[String, DataMsg] = mutable.LinkedHashMap.empty
    // Counter for naming data messages labels
    var dataMsgCounter = 0


    // Generate a data message from a string and add to allDataMsgs
    def addDataMsg(s:String): String = {
        val result = addDataMsgWithLabel(s, dataMsgCounter.toString)
        dataMsgCounter = dataMsgCounter + 1
        result
    }

    // Helper function for addDataMsg()
    def addDataMsgWithLabel(s: String, label: String): String = {
        val len = s.length

        val msg = DataMsg(s, label, len)
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
}