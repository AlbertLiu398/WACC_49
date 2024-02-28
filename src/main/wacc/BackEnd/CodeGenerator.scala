package wacc

import wacc.ast._
import scala.collection._
import Instruction._
import scala.annotation.unused
import Constant._
import Conditions._
import Labels._
import Utility._

class CodeGenerator (varList: List[Int]) {

  // identMap to store all variables and their size & pointer
  // For functions specifically: size = func.returnType's size, pointer = func.paramListType.length
  case class identMapEntry(size: Int, reg: Register)

  private val identMap = mutable.Map[Ident, identMapEntry]()
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

  private var funcProcessed = 0

  private var assemblyFcuntions = mutable.Map[String, List[Instruction]]()
  
  /* 
    generateInstructions (recursive function)
  */
  def generateInstructions(ast: ASTNode): Unit = ast match {
    /* --------------------------- generate program */
    case Program(functions, statements) =>
      

      // Start with the data section
      instructions ++= List(
        I_Directive(".align 4"),
        I_Directive(".text"),
        I_Directive(".global main"), 
        I_Label("main"))
      
      // // Add function names to identMap
      // for(func <- functions) {
      //   identMap(Ident("wacc_" + func.functionName.value)) = identMapEntry(getSize(func.returnType), func.params.paramListType.length)
      // }

      // Generate code for main body
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), true))
      pushUsedRegs(unused_ResultRegs.toList, varList.last)
      instructions.append(I_Move(fp, sp))
      generateInstructions(statements)
      instructions.append(I_Move(x0, ImmVal(0)))
      popUsedRegs(used_ResultRegs.toList.reverse, varList.last)
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)

      // Add utility function definitions
      instructions.appendAll(addUtility())

      // Generate code for functions
      for(func <- functions) {
        generateInstructions(func)
      }

      // Add all string labels at the top of program
      if (allDataMsgs.nonEmpty) {

        // Add the DataMsg.instruction of all (String, DataMsg) pairs in the hashmap before all other instructions
        allDataMsgs.flatMap(kv => kv._2.instruction) ++=: instructions

        // Add .data directive to the very top of program
        I_Directive(".data") +=: instructions

      }

      


    case Func(returnType, functionName, params, body) =>

      instructions.append(I_Label(functionName.value))
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), true))
      pushUsedRegs(unused_ResultRegs.toList, varList(funcProcessed))
      instructions.append(I_Move(fp, Content(sp, ImmVal(0))))
      generateInstructions(body)
      popUsedRegs(unused_ResultRegs.toList, varList(funcProcessed))
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)
      funcProcessed += 1

    /* --------------------------- generate expressions */
    case Add(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Add(x8, x8, ImmVal(value)))
        case _=> 
          // int x = 1- (2 - 1)
          // mov x9 x8 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Add(x8, used_TempRegs.head, x8))
      }
      revertTempRegs()

      
    case Sub(expr1, expr2) => 
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Sub(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Sub(x8, used_TempRegs.head, x8))
      }

      

    case Mul(expr1, expr2) =>
      generateInstructions(expr1)  // mov x8 expr1
      instructions.append(I_Move(unused_TempRegs.head, x8))  // mov x8 x9 
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)  //mov x8 epr2 
      
      instructions.append(I_Mul(x8, used_TempRegs.head, x8))  // mul x8 x9 x8
      revertTempRegs()


    case Div(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_UDiv(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_UDiv(x8, used_TempRegs.head, x8))
      }

    case Mod(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)
      
      // x8 = expr2, used_TempRegs.head = expr2
      
      // UDIV unused_TempRegs.head, x8, used_TempRegs.head  
      // MUL  unused_TempRegs.head, unused_TempRegs.head, used_TempRegs.head   
      // SUB  x8, x8, unused_TempRegs.head
      instructions.append(I_UDiv(unused_TempRegs.head, used_TempRegs.head, x8))
      instructions.append(I_Mul(unused_TempRegs.head, unused_TempRegs.head, x8))
      instructions.append(I_Sub(x8, used_TempRegs.head, unused_TempRegs.head))


    case LessThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LT))
      //push and pop x8
        
    case LessThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LE))

    case GreaterThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GT))

    case GreaterThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GE))

    case Eq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, EQ))
    
    case NotEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, NE))

    case And(expr1, expr2) =>
      // bool x = expr1 && (1 && 0)
      // x8 : expr1 
      // x8 
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_And(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_And(x8, used_TempRegs.head, x8))
      }

    case Or(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Orr(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Orr(x8, used_TempRegs.head, x8))
      }

    case Invert(expr) =>
      generateInstructions(expr)
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_CSet(x8, NE))
      //push and pop x8
      pushAndPopx8(16)
      


    case Negate(expr) =>
      generateInstructions(expr)
      pushAndPopx8(16)

  //   case Len(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Move(usedRegs.head, usedRegs.head))

    case Ord(expr) =>
      generateInstructions(expr)


    case Chr(expr) =>   
      generateInstructions(expr)
      //check the number of char is in ASCII table

      pushAndPopx8(16)

   // -------------------------- Generate instructions for statements
    case Read(lValue) =>
        lValue match {
        case n@Ident(value) =>
          instructions.append(I_Move(x8, getRegFromMap(n)))
          instructions.append(I_Move(x0, x8))
          readFlag = true
          branchLink(instructions, READ_LABEL)
          instructions.append(I_Move(x16, x0))
          instructions.append(I_Move(x8, x16))
          instructions.append(I_Move(getRegFromMap(n), x8))
        case _=>
        }

      
        
    case NewAssignment(identType, name, value) => 
      generateInstructions(value)
      instructions.append(I_Move(unused_ResultRegs.head, x8))
      used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      unused_ResultRegs.remove(0)
      identMap(name) = identMapEntry(getSize(value), used_ResultRegs.head)

          
      

    case Assignment(name, value) => 
      
      val reg = getRegFromMap(getIdent(name))
      name match {
        case n@ArrElem(name, v)=>
          generateInstructions(n)

        case n@FstPairElem(values) =>
          instructions.append(I_CBZ(reg, I_Label(ERR_NULL_LABEL)))
        
        case n@SndPairElem(values) => 
          instructions.append(I_CBZ(reg, I_Label(ERR_NULL_LABEL)))

          
        case _=>
          generateInstructions(name)
          
        generateInstructions(value)  
        instructions.append(I_Move(reg, x8))
        
      }
      
      
      
    
    case Free(expr) => 
    case Return(expr) => 
      generateInstructions(expr)
      instructions.append(I_Move(x0, used_ResultRegs.head))
      
    case Exit(expr) =>
      generateInstructions(expr)
      instructions.append(I_Move(unused_ParamRegs.head, x8))
      branchLink(instructions, EXIT_LABEL)

    

// 13		adrp x8, .L.str0
// 14		add x8, x8, :lo12:.L.str0
// 15		// push {x8}
// 16		stp x8, xzr, [sp, #-16]!
// 17		// pop {x8}
// 18		ldp x8, xzr, [sp], #16
// 19		mov x8, x8
// 20		mov x0, x8
// 21		// statement primitives do not return results (but will clobber r0/rax)
// 22		bl _prints
// 23		mov x0, #0
// 24		// pop {fp, lr}
// 25		ldp fp, lr, [sp], #16
// 26		ret
    case Print(expr, newline) =>
    
      generateInstructions(expr)
      val printBranch = expr.getType match {
        case "string" | "char[]" =>
          printStringFlag = true
          branchLink(instructions,PRINT_STRING_LABEL)

        case "bool" =>
          printBoolFlag = true
          branchLink(instructions,PRINT_BOOL_LABEL)

        case "char" =>
          printCharFlag = true
          branchLink(instructions,PRINT_CHAR_LABEL)

        case "int" =>
          printStringFlag = true
          branchLink(instructions,PRINT_INT_LABEL)

        case _ =>
          
      }
  
     
      if (newline) {
        printLineFlag = true
        branchLink(instructions, PRINT_LN_LABEL)
      }
      

       
    case If(condition, thenStat, elseStat) =>
      generateInstructions(condition)
      val (if_then, if_end) = addIfLabel()

      // Branch instruction, Jump to then clause if condition is EQ, otherwise continue to else clause
      instructions.append(I_Branch(I_Label(if_then), EQ)) 

      // Generate else clause
      generateInstructions(elseStat)
      // Branch instruction, (Unconditional) Jump to end after executing else clause
      instructions.append(I_Branch(I_Label(if_end)))

      // Label for then clause
      instructions.append(I_Label(if_then))
      // Generate then clause
      generateInstructions(thenStat)

      // Label for end of if
      instructions.append(I_Label(if_end))

   
    case While(condition, stat) =>
      // Generate a pair of label strings
      val (while_condition, while_body) = addWhileLabel()

      // Unconditional Jump to while condition
      instructions.append(I_Branch(I_Label(while_condition)))

      // Label for start of body of while
      instructions.append(I_Label(while_body))
      // Generate body of while
      generateInstructions(stat)

      // Label for condition of while
      instructions.append(I_Label(while_condition))
      // Generate condition of while
      generateInstructions(condition)
      
      // Jump back to body of while if condition holds
      instructions.append(I_Branch(I_Label(while_body), EQ))
      
    case Begin(stmt) =>
      generateInstructions(stmt)
    
    case SeqStmt(first, second) => 
      generateInstructions(first)
      generateInstructions(second)
        
    case Skip =>
    
    case NewPairRValue(expr1, expr2) =>
      instructions.append(I_Move(x0, ImmVal(getSize(expr1) + getSize(expr2))))

      branchLink(instructions, MALLOC_LABEL)

      instructions.append(I_Move(x16, x0))

      generateInstructions(expr1)

      instructions.append(I_Store(x8, Content(x16, ImmVal(0))))
      
      generateInstructions(expr2)

      instructions.append(I_Store(x8, Content(x16, ImmVal(getSize(expr1)))))

      instructions.append(I_Move(x8, x16))
      
    

    case IntLiter(value)=> instructions.append(I_Move(x8, ImmVal(value)))
         
    case BoolLiter(value) => 
      value match {
        case true => instructions.append(I_Move(x8, ImmVal(1)))
        case false => instructions.append(I_Move(x8, ImmVal(0)))
      }

    case CharLiter(value) => instructions.append(I_Move(x8, ImmValChar(value)))

    case StringLiter(value) => 
      // Create label string with the given string 
      val label = Labels.addDataMsg(value)
      // Convert the label string to an I_Label class and store at x8
      instructions.append(I_ADRP(x8, I_Label(label)))
    
    case PairLiter => 

    case a@ArrLiter(e, es) =>
      instructions.append(I_Move(x0, ImmVal(getSize(a))))
      branchLink(instructions, MALLOC_LABEL)
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Add(x16, x16, ImmVal(ARRAY_ELEM_SIZE)))

      // Size of array  
      var arrSize = 0
      e match {
        case StringLiter("empty") => arrSize = 0
        case _=> arrSize = es.length + 1
      }
      instructions.append(I_Move(x8, ImmVal(arrSize)))

      instructions.append(I_Store(x8, Content(x16, ImmVal(-ARRAY_ELEM_SIZE))))
      var arrPointer = 0
      for (expr <- e::es) {
        arrPointer += getSize(expr)
        generateInstructions(expr)
        // StoreByte for char and bool
        instructions.append(I_Store(x8, Content(x16, ImmVal(arrPointer))))
      }

      instructions.append(I_Move(x8, x16))
      instructions.append(I_Move(x19, x8))
    
    case CallRValue(func, args) =>
      for (i <- 0 until args.exprl.length) {
        generateInstructions(args.exprl(i))
        instructions.append(I_Move(unused_ParamRegs(i), x8))
      }
      branchLink(instructions, "wacc_" + func.value)
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Move(x8, x16))
    
    case FstPairElem(values) =>
      

    case SndPairElem(values) => 


    case _ => 
      
  }


  /* ------------------- Helper functions ------------------- */
  def getInstructions(): List[Instruction] = {
    instructions.toList
    }

  def refreshInstructions(): Unit = instructions.clear()
  
  private def getSize(ast: ASTNode): Int = {
    ast match {
      case IntLiter(_) => return INT_SIZE
         
      case BoolLiter(_) => return BOOL_SIZE

      case CharLiter(_) => return CHAR_SIZE

      case StringLiter(value) => return STRING_SIZE
      
      case PairLiter => return PAIR_SIZE

      case n@Ident(value) => return identMap.get(n).size

      case ArrLiter(e, es) => 
        
        e match {
          case StringLiter("empty") => return 0
          case _=> 
            val sizes = (e::es).map(elem => getSize(elem))
          return sizes.sum

        }
      case CallRValue(func, args) =>
        return identMap.get(func).get.size

      case FstPairElem(values) => return -1

      case SndPairElem(values) => return -1
      
      case ArrElem(name, value) =>
        //look up name in identMap
        //get size
        return identMap.get(name).size

      case BaseType(name) =>
        name match {
          case "int" => return INT_SIZE
          case "bool" => return BOOL_SIZE
          case "char" => return CHAR_SIZE
          case "string" => return STRING_SIZE
        }
      case ArrayType(elementType) => return ARRAY_TYPE_SIZE

      case PairType(first, second) => return PAIR_SIZE

      case _ => return EMPTY_SIZE
    }
  }






  def revertTempRegs():Unit = {
    used_TempRegs.clear()
    unused_TempRegs = mutable.ListBuffer() ++ unused_TempRegs_copy
    // print(used_TempRegs)
    // print(unused_TempRegs_copy)
    // print(unused_TempRegs)
  }

  def pushAndPopx8(size: Int):Unit = {
    instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-size)), true))
    instructions.append(I_LoadPair(x8, xzr, Content(sp), ImmVal(size)))
    instructions.append(I_Move(x8, x8))
  }

  def pushUsedRegs(regs: List[Register], noOfVar: Int):Unit = {
    if (noOfVar != 0 && noOfVar != 1){
      for (i <- 0 to noOfVar - 2 by 2) {
        instructions.append(I_LoadPair(regs(i), regs(i+1), Content(sp, ImmVal(0)), ImmVal(16), false))
      }
    }
    if (noOfVar%2 == 1) {
      instructions.append(I_StorePair(regs(noOfVar - 1), xzr, Content(sp, ImmVal(-16)), true))
    }
  }

  def popUsedRegs(regs: List[Register], noOfVar: Int):Unit = {
    if (noOfVar != 0 && noOfVar != 1){
      for (i <- 0 to noOfVar - 2 by 2) {
        instructions.append(I_LoadPair(regs(i), regs(i+1), Content(sp, ImmVal(0)), ImmVal(16), false))
      }
    }

    if (noOfVar%2 == 1) {
      instructions.append(I_LoadPair(regs.last, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
    }
  }

      // Helper function to append branch link instruction with a given label name
  def branchLink(instructions: mutable.ListBuffer[Instruction], s: String): Unit = {
      instructions.append(I_BranchLink(I_Label(s)))
  }

  def getRegFromMap(name: Ident): Register = {
      identMap.get(name) match {
        case Some(value) => return value.reg
        case None => "no such Ident in map"
      }
      return xzr
  }
  def getIdent(lvalue: LValue): Ident = {
    lvalue match {
      case n@Ident(value) => return n
      case n@ArrElem(name, value) => return name
      case n@FstPairElem(values) => return getIdent(values)
      case n@SndPairElem(values) => return getIdent(values)
    }
  }
}
