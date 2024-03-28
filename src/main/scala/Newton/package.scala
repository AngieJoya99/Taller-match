/* Angie Joya - 2322609
Emily Nuñez - 2240156*/
package object Newton{
    trait Expr
    case class Numero (d:Double) extends Expr
    case class Atomo (x:Char) extends Expr
    case class Suma (e1:Expr, e2:Expr) extends Expr
    case class Prod () extends Expr
    case class Resta (e1:Expr, e2:Expr) extends Expr
    case class Div (e1:Expr, e2:Expr) extends Expr
    case class Expo (e1:Expr, e2:Expr) extends Expr
    case class Logaritmo (e1:Expr) extends Expr

    /**
      * Convierte una expresión e en una cadena de texto con la definición simbólica
      * de esta, sin ambigüedades
      * @param e Expresión a mostrar
      * @return Cadena con definición simbólica
      */
    def mostrar (e:Expr):String = {
        val a = "retorno"
        a
    }

    /**
      * 
      *
      * @param f
      * @param a
      * @return
      */
    def derivar (f:Expr, a:Atomo): Expr = {
        val a = Numero(5)
        a
    }

    /**
      * 
      *
      * @param f
      * @param a
      * @param v
      * @return
      */
    def evaluar (f:Expr, a:Atomo, v:Double): Double ={
        val a = 5
        a
    }

    /**
      * 
      *
      * @param f
      * @return
      */
    def limpiar (f:Expr): Expr = {
        val a = Numero(5)
        a
    }

    /**
      * 
      *
      * @param f
      * @param a
      * @param x0
      * @param ba
      * @return
      */
    def raizNewton(f:Expr, a:Atomo, x0:Double, ba:(Expr, Atomo, Double) => Boolean): Double = {
        val a = 5
        a
    }
}



