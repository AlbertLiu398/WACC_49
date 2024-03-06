package wacc 

import scala.collection.mutable
import Instruction._

object PeepholeOptimisation {       


    def runPeeopholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        
        var instrus = instructions
        removeRedundantLoads(instrus)

        instrus
    }


    def removeRedundantLoads(instrus: List[Instruction]): Unit = {
        
        // TODO
    }

    def removeRedundantStores(instrus: List[Instruction]): Unit = {
        
        // TODO
    }

    


}