module KT-SYNTAX
  import DOMAINS
  syntax MainKotlinProg ::= "fun main()" "{" Block "}"

  syntax Block ::= "{" Statement "}"
                | Statement

  // ------ DECLARAÇÃO DE VARIÁVEIS -------


  syntax VariableType ::= "var" | "val"

  syntax Ids ::= List{Id, ","}

  syntax DeclaredVar ::= VariableType Ids

  syntax UniqueIdAssigner ::= Id "=" AExp  [strict(2)]


  syntax Assigner ::= DeclaredVar "=" AExp ";" [strict(2)]
                  | UniqueIdAssigner

  syntax Assigner2 ::= DeclaredVar "=" AExp [strict(2)]
                   | UniqueIdAssigner
//------------------------------------------

  syntax Statement ::= "if" "(" BExp ")" Block [strict(1)]
                    | BExp "?" Block ":" Block [strict(1)]
                    | "if" "(" BExp ")" Block ElseIfStatement [strict(1)]
                    | AExp
                    | AExpMod
                    | Skip
                    | Assigner
                    | Assigner2
                    | Statement Statement [left]

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
                  | Id "--"
  syntax AExp ::= Int
                | Id
                > left: AExp "*" AExp [seqstrict]
                > left: AExp "/" AExp [seqstrict]
                | AExp "-" AExp [seqstrict]
                | AExp "+" AExp [seqstrict]
                | "(" AExp ")" [bracket]

  syntax Skip ::= "Skip"

  // ---------- REGRAS ------------

  rule T:VariableType .Ids = _:Int ; => Skip
  rule T:VariableType .Ids = _:Int => Skip

  rule <k> var I:Id = V:Int ; => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> var I:Id = V:Int => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <loc> L:Int => L +Int 1 </loc>

  // Para: var x,y,z = n; ou var x,y,z = n

  rule <k> T:VariableType (I:Id, Is:Ids) = V:Int ; => T Is = V ; ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <loc> L:Int => L +Int 1 </loc>

    rule <k> T:VariableType (I:Id, Is:Ids) = V:Int => T Is = V ... </k>
        <env> MEnv:Map => MEnv I |-> L </env>
        <store> MStore:Map => MStore L |-> V </store>
        <loc> L:Int => L +Int 1 </loc>


  // ------ Alteração de variavies em memória abaixo-----------

  rule <k> I:Id = V:Int => Skip ... </k>
      <env> ... I |-> L ... </env>
      <store> ... L |-> (_ => V) ... </store>

  rule <k> I1:Id = I2:Id => I1 = V ... </k>
      <env> ... I2 |-> L ... </env>
      <store> ... L |-> V  ... </store>

  rule <k> I:Id => V ... </k>
      <env> ... I |-> L ... </env>
      <store> ... L |-> V ... </store>

  // Para: x++
  rule <k> I:Id ++ => Skip ... </k>
      <env> ... I |-> L ... </env>
      <store> ... L |-> (V => V +Int 1) ... </store>
  // Para: x--
  rule <k> I:Id -- => Skip ... </k>
      <env> ... I |-> L ... </env>
      <store> ... L |-> (V => V -Int 1) ... </store>




//-----------------------------------------

  rule fun main() { B:Block } => B [structural]


  // Resolve e empilha dois statements. Ex: x = 2 y = 4.
  // Separa (empilha) x=2, resolve, depois resolve y.
  rule S1:Statement S2:Statement => S1 ~> S2 [structural]

  rule Skip => .

  // ------ REGRAS PARA DECLARAÇÃO DE VARIÁVEIS EM MEMÓRIA (utiliza syntax <>) -----

  /*

  Depois de declarar uma variável ex: boolean x; retirar da pilha (ir pra próxima instrução)

  Usando o Map das configuração, renomeando de MapEnv,
  e mapeando o id I para posição de memória L

  Variáveis do tipo int quando alocadas iniciam com valor 0,
  logo, guardar na posição L (que é 1) de memória o valor 0

  */

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
    <k> $PGM:MainKotlinProg </k>
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
