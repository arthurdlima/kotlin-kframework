// Variaveis e constantes
fun main() {
  val x = "arthur"
  x = "..."

  var y = x
}

// kotlin assign
fun main() {
  var y: Int
  var x: String
  x = "arthur"
  y = 20
}

// var e constantes
fun main() {
  val x = "arthur"
  var y = " esdras"
  x = y
}

// Concatenação de string;

fun main() {
  var x = "arthur"
  var y = "esdras"
  x + y
}

// Print statmente

fun main() {
  var x = "arthur"
  var y = "esdras"
  print(x+y)
}

// kotlin when
fun main() {
  val nome = "arthur"
  when(nome){
    "lucas" -> print("eh lucas!");
    "arthur" -> print("eh Arthur!");
  }
}

// kotlin fun
fun main() {
  fun soma(): Int {
    var x = 2
    var y = 3
    return 1
  }
  soma();
}
