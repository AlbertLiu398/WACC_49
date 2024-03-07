package wacc 

import scala.collection.mutable
import Instruction._

object PeepholeOptimisation {       


    def runPeeopholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        
        var instrus = instructions
        removeRedundantLoads(instrus)

        instrus
    }


    // Load-Add
    // Load-Store

    // Store from a to b 
    // Load from b to c  ==> Load from a to c
    
    def removeRedundantLoads(instrus: List[Instruction]): Unit = {
        
        // TODO
    }

    def removeRedundantStores(instrus: List[Instruction]): Unit = {
        
        // TODO
    }

    


}