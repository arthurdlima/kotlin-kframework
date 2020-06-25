
module KT-SYNTAX
  import DOMAINS

  syntax MainKotlinProg ::= FunctionMain

  syntax FunctionMain ::= "fun main()" "{" Block "}"

  syntax Block ::= "{" Statement "}"
                | Statement

  //----------- KOTLIN FUNCTIONS ------------- //
  syntax Params ::= List{Id, ","}

  syntax FunReturnType ::= "Int"

  syntax FunReturn ::= AExp


  syntax KotlinFun ::= "fun" Id "(" Params ")" ":" FunReturnType Block

  syntax Values ::= List{Int, ","}

  //Botei o ";" por enquanto, resolver ambiguidade ao tirar..
  syntax KotlinFunCall ::= Id "(" Values ")" ";"


  /*Declarando e usando internamente ao .K, para
  para busca de funções pelo seu Id, retornando também
  seu statmente */
  syntax LocalFun ::= "local" "(" Id "," Statement ")"

  // ------------------------------------------
  //Obs: LEMBRAR DE USAR: syntax values ::= Int | Bool

  // ------ DECLARAÇÃO DE VARIÁVEIS -------

  syntax VariableType ::= "var" | "val"

  //KOTLIN VARIABLE ASSIGN INT : var x: Int
  syntax KotIntVarAssign ::= VariableType Id ":" "Int"
                      //    | VariableType Id ":" "Int" ";"

  syntax Ids ::= List{Id, ","}

  syntax DeclaredVar ::= VariableType Ids


  syntax UniqueIdAssigner ::= Id "=" AExp  [strict(2)]


  syntax Assigner ::= DeclaredVar "=" AExp [strict(2)]
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
                    | KotIntVarAssign
                    | KotlinFun
                    | KotlinFunCall
                    | "return" FunReturn
                    | LocalResInt
                    | LocalResString
                    | LocalResId
                    | "print" "(" AExps ")"
                    | Statement Statement [left]


  // ---------- para fazer print ---------------- //

  syntax Printable ::= Int | String

  syntax AExps ::= List{AExp,","}

  context print(HOLE:AExp, AEs:AExps)

  rule print(.AExps) => .  [structural]

  rule <k> print(P:Printable, AEs => AEs) ...</k>
     <out> ... .List => ListItem(P) </out>  [print]
// ----------------------------------------------- //




  syntax ElseIfStatement ::= "else if" "(" BExp ")" Block
                          | "else if" "(" BExp ")" Block ElseIfStatement
                          | "else" Block


  syntax BExp ::= Bool
                | AExp "==" AExp [strict]
                | AExp "<" AExp [strict]
                | AExp "<=" AExp [strict]
                | AExp ">" AExp [strict]
                | AExp ">=" AExp [strict]
                | "(" BExp ")" [bracket]

  syntax AExpMod ::= Id "++"
                  | Id "--"
  syntax AExp ::= Int | String
                | Id
                > left: AExp "*" AExp [seqstrict]
                > left: AExp "/" AExp [seqstrict]
                | AExp "-" AExp [seqstrict]
                | AExp "+" AExp [seqstrict]
                | "(" AExp ")" [bracket]
                | Printable



  syntax Skip ::= "Skip"

  // ---------- REGRAS INTEIRO ------------

  rule T:VariableType .Ids = _:Int => Skip

  // REGRA KOTLIN ASSIGN INTEIRO
  rule T:VariableType Id : Int => Skip

  // REGRA KOTLIN ASSIGN INTEIRO EM MEMÓRIA --------

  rule <k> T:VariableType I:Id : Int => Skip ... </k>
    <env> MEnv:Map => MEnv I |-> L </env>
    <store> MStore:Map => MStore L |-> 0 </store>
    <mults> Mults:Map => Mults L |-> 1 </mults>
    <loc> L:Int => L +Int 1 </loc>

// REGRAS PARA CONSTANTES E VARIÁVEIS -> Int

  rule <k> var I:Id = V:Int => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <mults> Mults:Map => Mults L |-> 1 </mults>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> val I:Id = V:Int => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <mults> Mults:Map => Mults L |-> 0 </mults>
      <loc> L:Int => L +Int 1 </loc>

  // Para: var x,y,z = n

  rule <k> var (I:Id, Is:Ids) = V:Int => var Is = V ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <mults> Mults:Map => Mults L |-> 1 </mults>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> val (I:Id, Is:Ids) = V:Int => val Is = V ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> V </store>
      <mults> Mults:Map => Mults L |-> 0 </mults>
      <loc> L:Int => L +Int 1 </loc>



  // ------ Alteração de variavies em memória -> Int -----------

  syntax LocalResInt ::= "localResInt" "(" Id "," Int ")"
  syntax LocalResId ::= "localResId" "(" Id "," Id ")"

  rule <k> I:Id = V:Int => if( S>0 ) {localResInt ( I , V )} ...  </k>
      <env> ... I |-> L ... </env>
      <mults> ... L |-> S ...  </mults>


    rule <k> localResInt(I:Id , V:Int) => Skip ... </k>
    <env> ... I |-> L ... </env>
    <store> ... L |-> (_ => V) ... </store>


  rule <k> I1:Id = I2:Id => if( S>0 ) {localResId ( I1 , I2 )} ... </k>
      <env> ... I1 |-> L ... </env>
      <mults> ... L |-> S ...  </mults>

  rule <k> localResId(I1:Id , I2:Id) => I1 = V ... </k>
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



  // ------- REGRAS STRINGS -------------------------- //
  syntax LocalResString ::= "localResString" "(" Id "," String ")"

  rule T:VariableType .Ids = _:String => Skip

  rule <k> var I:Id = S:String => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> S </store>
      <mults> Mults:Map => Mults L |-> 1 </mults>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> var (I:Id, Is:Ids) = S:String => var Is = S ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> S </store>
      <mults> Mults:Map => Mults L |-> 1 </mults>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> val I:Id = Stg:String => Skip ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> Stg </store>
      <mults> Mults:Map => Mults L |-> 0 </mults>
      <loc> L:Int => L +Int 1 </loc>

  rule <k> val (I:Id, Is:Ids) = Stg:String => val Is = Stg ... </k>
      <env> MEnv:Map => MEnv I |-> L </env>
      <store> MStore:Map => MStore L |-> Stg </store>
      <mults> Mults:Map => Mults L |-> 0 </mults>
      <loc> L:Int => L +Int 1 </loc>


  rule <k> I:Id = Stg:String => if( S>0 ) {localResString ( I , Stg )} ...  </k>
      <env> ... I |-> L ... </env>
      <mults> ... L |-> S ...  </mults>


    rule <k> localResString(I:Id , Stg:String) => Skip ... </k>
    <env> ... I |-> L ... </env>
    <store> ... L |-> (_ => Stg) ... </store>




  // REGRAS PARA FUNÇÕES KOTLIN

  rule <k> fun I:Id ( P:Params ): VariableType { S:Statement } => Skip ... </k>
      ( .Bag => <funThread>
          <id> I </id>
          <scope> S </scope>
          <params> P </params>
          <temp> P </temp>
          <localEnv> .Map </localEnv>
        </funThread>
      )


  rule <k> I:Id (.Values) ;  => local(I, S) ... </k>
      <funThread>
          ...
          <id> I </id>
          <scope> S:Statement </scope>
          <params> P </params>
          <temp> (.Params => P) </temp>
          ...
      </funThread>

      //guardar param em memoria, recursivo
  rule <k> I:Id ((V:Int, Vs:Values)) ;  => I:Id (Vs) ; ... </k>
      <funThread>
          ...
          <id> I </id>
          <temp> ((P:Id, Ps:Params) => Ps) </temp>
          <localEnv> FunM:Map => FunM P |-> L </localEnv>
          ...
      </funThread>
      <store> SMap:Map => SMap L |-> V </store>
      <loc> L => L +Int 1 </loc>


  rule <k> local(FunId:Id, var I:Id = V:Int) => Skip ... </k>
      <funThread>
          ...
          <id> FunId </id>
          <localEnv> FunM:Map => FunM I |-> L </localEnv>
          ...
      </funThread>
      <store> SMap:Map => SMap L |-> V </store>
      <loc> L => L +Int 1 </loc>



  rule fun main() { B:Block } => B [structural]

  //rule fun Id () : FB:FunBlock => FB [structural]
  // Resolve e empilha dois statements. Ex: x = 2 y = 4.
  // Separa (empilha) x=2, resolve, depois resolve y.
  rule S1:Statement S2:Statement => S1 ~> S2 [structural]

  //?? talvez precsise para classes
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

  rule I1:Int == I2:Int => I1 ==Int I2
  rule I1:Int + I2:Int => I1 +Int I2
  rule I1:Int - I2:Int => I1 -Int I2
  rule I1:Int * I2:Int => I1 *Int I2
  rule I1:Int / I2:Int => I1 /Int I2
  rule I1:Int < I2:Int => I1 <Int I2
  rule I1:Int <= I2:Int => I1 <=Int I2
  rule I1:Int > I2:Int => I1 >Int I2

  rule S1:String + S2:String => S1 +String S2

  // ----------- Configurações -------------

  configuration <T>
    <k> $PGM:MainKotlinProg </k>
    <function color="green">
        <funThread multiplicity="*" color="red" type="Map">
            <id> .K </id>
            <scope> .K </scope>
            <params> .K </params>
            <temp> .K </temp>
            <localEnv> .Map </localEnv>
        </funThread>
    </function>
    <env> .Map </env>
    <store> .Map </store>
    <mults> .Map </mults>
    <out color="blue"> .List </out>
    <loc> 1 </loc>
  </T>
  /* O "*" significa que pode ser guardadas quantas celulas
    forem necessárias. Neste caso, quantos threads de objetos,
    classes, e funções forem necessárias
  */
  endmodule


module KT
    import DOMAINS
    import KT-SYNTAX
    syntax KResult ::= Int | Bool | String
endmodule
