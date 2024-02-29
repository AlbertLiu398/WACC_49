package wacc

import scala.collection.mutable
import Instruction._
import Constant._

object Labels {

    // HashMap to store all string data stored in program
    // val allDataMsgs: mutable.Map[String, DataMsg] = mutable.HashMap.empty[String, DataMsg]
    val allDataMsgs = new mutable.HashMap[String, mutable.Set[DataMsg]].withDefault(_ => mutable.Set.empty[DataMsg])

    // Counter for naming data messages labels
    var dataMsgCounter = 0

    // counter for naming if labels
    var ifCounter = 0

    // counter for naming while labels
    var whileCounter = 0

    // print counter 
    var printbCounter = 0
    var printcCounter = 0
    var printiCounter = 0
    var printsCounter = 0
    var printpCounter = 0
    var printLinCounter = 0
    
    // read counter
    var readiCounter = 0
    var readcCounter = 0

    // freePair counter
    var freePairCounter = 0

    // error handler label counter
    var errOutOfMemoryCounter = 0
    var errNullCounter = 0
    var errDivZeroCounter = 0
    var errOutOfBoundCounter = 0
    var errOverflowCounter = 0
    var errBadCharCounter = 0
    

    
    case class DataMsg(s: String, labelIndex: Int, actualSize: Int, name: String){
        val label: String = s".L.str$labelIndex"
        var instruction: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty
            if (labelIndex == -1) {
                // Use customised label name
                instruction = mutable.ListBuffer(
                    I_Directive(s"      .word ${actualSize - 1}"),
                    I_Label(s"$name"),
                    I_Directive(s"      .asciz " + "\"" + s.substring(1) + "\"")
                )
            } else {
                // Use counter to generate label name
                instruction = mutable.ListBuffer(
                    I_Directive(s"      .word $actualSize"),
                    I_Label(label),
                    I_Directive(s"      .asciz " + "\"" + s + "\"")
                ) 
            }
    }

    // Generate a data message from a string and add to allDataMsgs
    def addDataMsg(s:String): String = {
        // print("Adding data message: " + s + "     COUNTER:  " +  dataMsgCounter + "\n")
        val result = addDataMsgWithLabel(s, dataMsgCounter, "")
        dataMsgCounter = dataMsgCounter + 1
        result
    }

    def addCustomisedDataMsg(s: String, labelName: String): String = {
        // If use customised label name, set label index to -1 (No need to generate name using counter)
        val result = addDataMsgWithLabel(s, -1 ,labelName)
        result
    }

    private def replaceEscapeCharacters(input: String): String = {
        val result = new StringBuilder
        var i = 0
        while (i < input.length) {
            val nextChar = input.charAt(i)
            nextChar match {
            case '"' => 
                result.append('\\')
                result.append(nextChar)

            // Add more cases for other escape characters as needed
            case _ => 
                result.append(nextChar)
            }
            i += 1 // Skip the next character
        }
            
        result.toString
    }

    // Helper function for addDataMsg()
    def addDataMsgWithLabel(s: String, labelCounter: Int, customisedLabelName: String): String = {
        val len = s.length
        var s_escaped = replaceEscapeCharacters(s)

        // using default to handle non-existing keys
        val dataMsgSets = allDataMsgs(s_escaped)
        val msg = DataMsg(s_escaped, labelCounter, len, customisedLabelName)
        allDataMsgs(s_escaped) = dataMsgSets.union(Set(msg))
        msg.label
    }

    def addIfLabel(): (String, String) ={
        val instr = (s".if_then_$ifCounter", s".if_end_$ifCounter")
        ifCounter += 1
        instr
    }

    def addWhileLabel(): (String, String) ={
        val instr = (s".w_condition_$whileCounter", s".w_body_$whileCounter")
        whileCounter += 1
        instr
    }

    def addPrintbLabel(): String = {
        val instr = s".L._printb_str$printbCounter"
        printbCounter += 1
        instr
        
    }
    def addPrintcLabel(): String = {
        val instr = s".L._printc_str$printcCounter"
        printcCounter += 1
        instr

        
    }
    def addPrintlnLabel(): String = {
        val instr = s".L._println_str$printLinCounter"
        printLinCounter += 1
        instr
        
    }
    def addPrintsLabel(): String = {
        val instr = s".L._prints_str$printsCounter"     
        printsCounter += 1
        instr
         
    }
   def addPrintiLabel(): String = {
        val instr = s".L._printi_str$printiCounter"
        printiCounter += 1
        instr 
    }

    def addPrintpLabel(): String = {
        val instr = s".L._printp_str$printpCounter"
        printpCounter += 1
        instr 
    }

    def addReadiLabel(): String = {
        val instr = s".L._readi_str$readiCounter"
        readiCounter += 1
        instr
    }

    def addReadcLabel(): String = {
        val instr = s".L._readc_str$readcCounter"
        readcCounter += 1
        instr
    }



    // functions to add error handlers label

    def addErrOutOfMemoryLabel() : String = {
        val instr = s".L._errOutOfMemory_str$errOutOfMemoryCounter"
        errOutOfMemoryCounter += 1
        instr
    }

    def addErrNullLabel(): String = {
        val instr = s".L._errNull_str$errNullCounter"
        errNullCounter += 1
        instr
    }

    def addErrDivZeroLabel(): String = {
        val instr = s".L._errDivZero_str$errDivZeroCounter"
        errDivZeroCounter += 1
        instr
    }
    
    def addErrOutOfBoundLabel(): String = {
        val instr = s".L._errOutOfBound_str$errOutOfBoundCounter"
        errOutOfBoundCounter += 1
        instr
    }

    def addErrOverflowLabel() : String = {
        val instr = s".L._errOverflow_str$errOverflowCounter"
        errOverflowCounter += 1
        instr
    }
    def addErrBadCharLabel() : String = {
        val instr = s".L._errBadChar_str$errBadCharCounter"
        errBadCharCounter += 1
        instr
    }

    // function to add free label
    def addFreePairLabel(): String = {
        val instr = s".L._free_str$freePairCounter"
        freePairCounter += 1
        instr
    }

}