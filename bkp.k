
module KT-SYNTAX
  import DOMAINS

  syntax MainKotlinProg ::= "fun main()" "{" Block "}"
                        | "fun main()" "{"  "}"

  syntax Block ::= "{" Statement "}"
                | Statement


  //----------- KOTLIN FUNCTIONS -------------
  syntax FunReturnType ::= "String"
                        | "Int"

  syntax KotlinFun ::= "fun" Id "(" ")" ":" FunReturnType Block [strict(3)]


  syntax KotlinFunCall ::= Id "(" ")"
                        //  | Id "(" ")" ";"

  /*Declarando e usando internamente ao .K, para
  para busca de funções pelo seu Id, retornando também
  seu statmente */
  syntax LocalFun ::= "local" "(" Id "," Statement ")"

// ------------------------------------------


  // ------ DECLARAÇÃO DE VARIÁVEIS -------


  syntax VariableType ::= "var"

  //KOTLIN VARIABLE ASSIGN INT : var x: Int
  syntax KotIntVarAssign ::= VariableType Id ":" "Int"
                      //    | VariableType Id ":" "Int" ";"

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
                    | "while" "(" BExp ")" Block
                  //  | "forEach" "{" Id "->" Array "}" Block
                    | "for" "(" Assigner ";" BExp ";" AExpMod ")" Block
                    | AExp
                    | AExpMod
                    | Skip
                    | Assigner
                    | Assigner2
                    | KotIntVarAssign
                    | KotlinFun
                    | KotlinFunCall
                    | "{" Statement "return" AExp "}"
                    | "{" "return" AExp "}"
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

  // ---------- REGRAS INTEIRO ------------

  rule T:VariableType .Ids = _:Int ; => Skip
  rule T:VariableType .Ids = _:Int => Skip
  // REGRA KOTLIN ASSIGN INTEIRO

  rule VariableType Id : Int => Skip
  //rule VariableType .Ids : Int ; => Skip

  // REGRA KOTLIN ASSIGN INTEIRO EM MEMÓRIA --------

  rule <k> T:VariableType I:Id : Int => Skip ... </k>
    <env> MEnv:Map => MEnv I |-> L </env>
    <store> MStore:Map => MStore L |-> 0 </store>
    <loc> L:Int => L +Int 1 </loc>
  /*
  rule <k> T:VariableType I:Id : Int ; => Skip ... </k>
    <env> MEnv:Map => MEnv I |-> L </env>
    <store> MStore:Map => MStore L |-> 0 </store>
    <loc> L:Int => L +Int 1 </loc>

  rule <k> T:VariableType (I:Id, Xs:Ids) : Int ; => T Xs : Int; ... </k>
    <env> MEnv:Map => MEnv I |-> L </env>
    <store> MStore:Map => MStore L |-> 0 </store>
    <loc> L:Int => L +Int 1 </loc>
  rule <k> T:VariableType (I:Id, Xs:Ids) : Int => T Xs : Int ... </k>
    <env> MEnv:Map => MEnv I |-> L </env>
    <store> MStore:Map => MStore L |-> 0 </store>
    <loc> L:Int => L +Int 1 </loc>
    */
  // -------------------------------------------------

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


  // ------- REGRAS BOOLEANO ---------------------------




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

  // REGRAS PARA FUNÇÕES KOTLIN

  rule <k> fun I:Id () : FunReturnType S:Statement => Skip ... </k>
      ( .Bag => <funThread>
          <id> I </id>
          <scope> S </scope>
          <localEnv> .Map </localEnv>
        </funThread>
      )
      //<funThread> M:Map => M I |-> FB </funThread>

  rule <k> I:Id () => local(I, S) ... </k>
      <funThread>
          ...
          <id> I </id>
          <scope> S:Statement </scope>
          ...
      </funThread>

  rule <k> local(FunId:Id, VariableType I:Id = V:Int;) => Skip ... </k>
      <funThread>
          ...
          <id> FunId </id>
          <localEnv> FunM:Map => FunM I |-> L </localEnv>
          ...
      </funThread>
      <store> SMap:Map => SMap L |-> V </store>
      <loc> L => L +Int 1 </loc>


  //rule <k> I:Id (); => FB ... </k>
      //<funThread> ... I |-> FB ... </funThread>
//-----------------------------------------

  rule fun main() { B:Block } => B [structural]

  //rule fun Id () : FB:FunBlock => FB [structural]
  // Resolve e empilha dois statements. Ex: x = 2 y = 4.
  // Separa (empilha) x=2, resolve, depois resolve y.
  rule S1:Statement S2:Statement => S1 ~> S2 [structural]
  //rule FB1:FunBlock FB2:FunBlock => FB1 ~> FB2 [structural]

  rule local(I:Id, S1:Statement S2:Statement) => local(I, S1) ~> local(I, S2) [structural]
  rule Skip => .

  // ------ REGRAS PARA DECLARAÇÃO DE VARIÁVEIS EM MEMÓRIA (utiliza syntax <>) -----

  /*

  Depois de declarar uma variável ex: boolean x; retirar da pilha (ir pra próxima instrução)

  Usando o Map das configuração, renomeando de MapEnv,
  e mapeando o id I para posição de memória L

  Variáveis do tipo int quando alocadas iniciam com valor 0,
  logo, guardar na posição L (que é 1) de memória o valor 0

  */
  rule while ( B:BExp ) { S:Statement } => if (B) { S while ( B ) { S } } [structural]
  rule for (A:Assigner ; B:BExp ; AEM:AExpMod ) {S}
    => A ~> while (B) { S AEM } [structural]

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

    <function>
        <funThread multiplicity="*" color="blue" type="Map">
            <id> .K </id>
            <scope> .K </scope>
            <localEnv> .Map </localEnv>
        </funThread>
    </function>
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
