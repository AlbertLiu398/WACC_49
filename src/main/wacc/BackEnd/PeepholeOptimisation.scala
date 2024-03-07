package wacc 

import scala.collection.mutable
import Instruction._

object PeepholeOptimisation {       


    def runPeeopholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        
        var instrus = instructions
        removeRedundantLoads(instrus)

        instrus
    }


// -------------peephole size 2---------------

    // ---------MOVE------------

    /*
        Redundant Move-Move
        e.g.
            mov     w0, w1
            mov     w2, w0
        can be replaced with
            mov     w2, w1
    */

    /*
        Redundant Move-Store
        e.g.
            mov     w0, w1
            str     w0, [fp,#28]
        can be replaced with
            str     w1, [fp,#28]
    */


    // ---------LOAD------------

    /*
        Redundant Load-Move
        e.g.
            ldr     w0, [fp,#28]
            mov     w1, w0
        can be replaced with
            ldr     w1, [fp,#28]
    */


    /*
        Redundant Load-Load
        e.g.
            ldr     w1, [fp,#28]
            ldr     w0, [fp,#28]
        can be replaced with
            ldr     w1, [fp,#28]
            mov     w0, w1

    */

    /*
        Redundant Load-Store
        e.g.
            ldr     w0, [fp,#28]
            str     w0, [fp,#28]
        can be replaced with
            ldr     w0, [fp,#28]
        (remove second instruction)
    */

    

    // ---------STORE------------

    /*
        Redundant Store-Load
        e.g.
            str     w0, [fp,#28]
            ldr     w1, [fp,#28]
        can be replaced with
            str     w0, [fp,#28]
            mov     w1, w0
        --------------------------
        e.g. 
            str     w0, [fp,#28]
            ldr     w0, [fp,#28]
        can be replaced with
            str     w0, [fp,#28]
        (remove second instruction)
    */


    /*
        Redundant Store-Store
        e.g.
            str     w0, [fp,#28]
            str     w1, [fp,#28]
        can be replaced with
            str     w1, [fp,#28]
    */




    // ---------STACK------------



    // ----------ARITHMETIC------------




// -------------peephole size 3---------------

    /* 

        Redundant Move-Move-Move
        e.g.
            mov     w0, w1
            mov     w2, w0
            mov     w3, w2
        can be replaced with
            mov     w3, w1

    */


    


}