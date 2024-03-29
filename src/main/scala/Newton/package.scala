/* Angie Joya - 2322609
Emily Nuñez - 2240156*/
package object Newton{
    trait Expr
    case class Numero (d:Double) extends Expr
    case class Atomo (x:Char) extends Expr
    case class Suma (e1:Expr, e2:Expr) extends Expr
    case class Prod (e1:Expr, e2:Expr) extends Expr
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
        e match{
            case Numero(n) => n.toString()
            case Atomo (x) => x.toString()
            case Suma (e1, e2) => "(" + mostrar(e1) + " + " + mostrar(e2) + ")"
            case Prod (e1, e2) => "(" + mostrar(e1) + " * " + mostrar(e2) + ")"
            case Resta (e1, e2) => "(" + mostrar(e1) + " - " + mostrar(e2) + ")"
            case Div (e1, e2) => "(" + mostrar(e1) + " / " + mostrar(e2) + ")"
            case Expo (e1, e2) => "(" + mostrar(e1) + " ^ " + mostrar(e2) + ")"
            case Logaritmo (e1) => "(Ln (" + mostrar(e1) + "))"
        }
    }

    /**
      * Dadas una función f y una variable a, muestra una cadena de texto
      * con la expresión correspondiente a la derivada de f respecto a a
      * @param f Función a derivar
      * @param a Variable de la función
      * @return Derivada de f respecto a 'a'
      */
    def derivar (f:Expr, a:Atomo): Expr = {
        f match{
            case Numero(n) => Numero(0)
            case Atomo (x) => {
                if (Atomo(x) == a) (Numero(1))
                else (Numero(0))
            }
            case Suma (e1, e2) => Suma(derivar(e1, a) , derivar(e2, a))
            case Prod (e1, e2) => Suma(Prod(derivar(e1, a),e2) , Prod(e1,derivar(e2, a)))
            case Resta (e1, e2) => Resta(derivar(e1, a) , derivar(e2, a))
            case Div (e1, e2) => Div((Resta(Prod(derivar(e1, a),e2) , Prod(derivar(e2, a),e1))),Expo(e2,Numero(2)))
            case Expo (e1, e2) => Prod(Expo(e1,e2),Suma(Div(Prod(derivar(e1,a),e2),e1),Prod(derivar(e2,a),Logaritmo(e1))))
            case Logaritmo (e1) => Div(derivar(e1, a),e1)
        }
    }

    /**
      * Sea la función f que depende de la variable a, calcula el resultado de
      * reemplazar el valor v en todas las instancias de a
      * @param f Función a Evaluar
      * @param a Variable de la función
      * @param v Valor a reemplazar
      * @return Resultado de evaluar f(v)
      */
    def evaluar (f:Expr, a:Atomo, v:Double): Double ={
        f match{
            case Numero(n) => n
            case Atomo (x) => v
            case Suma (e1, e2) => evaluar(e1,a,v) + evaluar(e2,a,v)
            case Prod (e1, e2) => evaluar(e1,a,v) * evaluar(e2,a,v)
            case Resta (e1, e2) => evaluar(e1,a,v) - evaluar(e2,a,v)
            case Div (e1, e2) => evaluar(e1,a,v) / evaluar(e2,a,v)
            case Expo (e1, e2) => math.pow(evaluar(e1,a,v) , evaluar(e2,a,v))
            case Logaritmo (e1) => math.log(evaluar(e1,a,v))
        }
    }

    /**
      * Elimina unos y ceros innecesarios de una expresión f
      * @param f Función a limpiar
      * @return Expresión de f sin 1 ni 0 innecesarios
      */
    def limpiar (f:Expr): Expr = {
        //La última suma no funciona
        f match{
            case Numero(n) => Numero(n)
            case Atomo (x) => Atomo(x)
            case (Suma(e1, e2)) =>{
                if ((limpiar(e1) == Numero(0))&&(limpiar(e2) == Numero(0))) (Numero(0))
                else if (limpiar(e2) == Numero(0)) (limpiar(e1))
                else if (limpiar(e1) == Numero(0)) (limpiar(e2))
                else (Suma(limpiar(e1),limpiar(e2)))
            }
            case (Prod(e1, e2)) =>{
                if ((limpiar(e1) == Numero(0))||(limpiar(e2) == Numero(0))) (Numero(0))
                else if (limpiar(e2) == Numero(1)) (limpiar(e1))
                else if (limpiar(e1) == Numero(1)) (limpiar(e2))
                else (Prod(limpiar(e1), limpiar(e2)))
            }
            case Resta (e1, e2) =>{
                if ((limpiar(e1) == Numero(0))&&(limpiar(e2) == Numero(0))) (Numero(0))
                else if (limpiar(e2) == Numero(0)) (limpiar(e1))
                else if (limpiar(e1) == Numero(0)) (limpiar(e2))
                else (Resta(limpiar(e1), limpiar(e2)))
            }
            case Div (e1, e2) => {
                if (limpiar(e2) == Numero(1)) (limpiar(e1))
                else (Div(limpiar(e1),limpiar(e2)))
            }
            case Expo (e1, e2) =>{
                if (limpiar(e2) == Numero(0)) (Numero(1))
                else if (limpiar(e2) == Numero(1)) (limpiar(e1))
                else (Expo(limpiar(e1),limpiar(e2)))
            }
            
            case Logaritmo (e1) => {
                if (limpiar(e1) == Numero(1)) (Numero(0))
                else(Logaritmo(limpiar(e1)))
            }
        }
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



