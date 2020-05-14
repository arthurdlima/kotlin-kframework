module KT-SYNTAX
  import DOMAINS
  syntax Program ::= "fun main()" "{" Block "}"

  syntax Block ::= "{" Statement "}"
                | Statement

  // ------ DECLARAÇÃO DE VARIÁVEIS -------

  syntax VariableType ::= "boolean" | "integer"

  // Declarando os tipos de variáveis
  //syntax DeclaredVars ::= VariableType List{Id, ","} ";"
  //                      | VariableType List{Id, ","}
  // Declarando variável única
  syntax DeclareUniqueVar ::= VariableType Id ";"
  //                        |  VariableType Id
  // Atribuindo à variável

  //syntax Assigner ::= DeclaredVars "=" AExp
                    //| DeclareUniqueVar "=" AExp



  syntax Statement ::= "if" "(" BExp ")" Block [strict(1)]
                    | BExp "?" Block ":" Block [strict(1)]
                    | "if" "(" BExp ")" Block ElseIfStatement [strict(1)]
                    | Statement Statement [left]
                    | AExp
                    | AExpMod
                    | Skip
                    | DeclareUniqueVar

  syntax ElseIfStatement ::= "else if" "(" BExp ")" Block
                          | "else if" "(" BExp ")" Block ElseIfStatement
                          | "else" Block


  syntax BExp ::= Bool
                | AExp "<" AExp [strict]
                | AExp "<=" AExp [strict]
                | AExp ">" AExp [strict]
                | AExp ">=" AExp [strict]
                | "(" BExp ")" [bracket]

  syntax AExpMod ::= Id "++"
  syntax AExp ::= Int
                | Id
                > left: AExp "*" AExp [seqstrict]
                > left: AExp "/" AExp [seqstrict]
                | AExp "-" AExp [seqstrict]
                | AExp "+" AExp [seqstrict]
                | "(" AExp ")" [bracket]

  // ---------- REGRAS ------------

  syntax Skip ::= "Skip"

  rule fun main() { B:Block } => B [structural]

  rule S1:Statement S2:Statement => S1 ~> S2 [structural]
  //rule local(I:Id, S1:Statement S2:Statement) => local(I, S1) ~> local(I, S2) [structural]

  rule Skip => .

  // ------ REGRAS PARA DECLARAÇÃO DE VARIÁVEIS EM MEMÓRIA (utiliza syntax <>) -----

  /*

  Depois de declarar uma variável ex: boolean x; retirar da pilha (ir pra próxima instrução)

  Usando o Map das configuração, renomeando de MapEnv,
  e mapeando o id I para posição de memória L

  Variáveis do tipo int quando alocadas iniciam com valor 0,
  logo, guardar na posição L (que é 1) de memória o valor 0

  */

  rule <k> T:VariableType I:Id ; => Skip </k>
        <env> MapEnv:Map => MapEnv I |-> L </env>
        <store> MapStore:Map => MapStore L |-> 0 </store>
        <loc> L:Int </loc>

  rule if (true) { S:Statement } => S
  rule if (false) { _ } => Skip
  rule if (true) { B } S:ElseIfStatement => B
  rule if (false) { _ } S:ElseIfStatement => S
  rule else if ( B:BExp ) { S1:Statement } S2:ElseIfStatement => if ( B ) { S1 } S2
  rule else if ( B:BExp ) { S:Statement } => if ( B ) { S }
  rule else { S } => S

  rule true ? B:Block : _ => B
  rule false ? _ : B:Block => B

  rule I1:Int + I2:Int => I1 +Int I2
  rule I1:Int - I2:Int => I1 -Int I2
  rule I1:Int * I2:Int => I1 *Int I2
  rule I1:Int / I2:Int => I1 /Int I2
  rule I1:Int < I2:Int => I1 <Int I2
  rule I1:Int <= I2:Int => I1 <=Int I2
  rule I1:Int > I2:Int => I1 >Int I2

  // ----------- Configurações -------------

  configuration <T>
    <k> $PGM:Program </k>
    <env> .Map </env>
    <store> .Map </store>
    <loc> 1 </loc>
  </T>

  endmodule


module KT
    import DOMAINS
    import KT-SYNTAX
    syntax KResult ::= Int | Bool
endmodule
