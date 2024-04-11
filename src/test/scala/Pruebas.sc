/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

import Newton._
import Newton.Logaritmo
import Newton.Resta

val expr1 = Suma(Atomo('x'), Numero(2.0))
val expr2 = Prod(Resta(Numero(0),Numero(17.45)),Expo(Numero(1480.0),Atomo('Y')))
val expr3 = Suma(Suma(Expo(Atomo('x'),Numero(2.0)),Prod(Numero(2.0),Atomo('x'))),Numero(-8.0))
val expr4 = Resta(Logaritmo(Atomo('@')),Expo(Atomo('e'),Numero(14782395.7453)))
val expr5 = Div(Suma(Atomo('Z'),Div(Atomo('ç'),Numero(-16.24))),Prod(Atomo('q'),Numero(0.0)))

mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)

mostrar(derivar(expr1, Atomo('x')))
mostrar(derivar(expr2, Atomo('Y')))
mostrar(derivar(expr3, Atomo('x')))
mostrar(derivar(expr4, Atomo('@')))
mostrar(derivar(expr5, Atomo('ç')))

val expr6 = Suma(Suma(Expo(Atomo('x'),Numero(2.0)),Prod(Numero(2.0),Atomo('x'))),Numero(-8.0))
val expr7 = Resta(expr6,Logaritmo(Atomo('x')))
val expr8 = Div(Expo(Atomo('Y'),Numero(2.0)),Numero(12437.81))
val expr9 = derivar(Div(Suma(Numero(30.0),Atomo('x')),Resta(Prod(Numero(5),Atomo('x')),Numero(10.0))),Atomo('x'))
val expr10 = Logaritmo(Div(Expo(Atomo('x'),Numero(2.0)),Numero(8.0)))

mostrar(expr6)
evaluar(expr6, Atomo('x'), -3.5)
mostrar(expr7)
evaluar(expr7, Atomo('x'), 2.0)
mostrar(expr8)
evaluar(expr8, Atomo('Y'), 1.0)
mostrar(expr9)
evaluar(expr9, Atomo('x'), 5.0)
mostrar(expr10)
evaluar(expr10, Atomo('x'), 0.0)

val expr11 = derivar (Suma(Atomo( 'k' ) , Prod(Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x' ) ) 
mostrar(limpiar(expr11))
val expr12 = Suma(Prod(Resta(Numero(0),Atomo('k')),Numero(1.0)),Numero(0.0))
mostrar(limpiar(expr12))
val expr13 = Div(Atomo('D'),Expo(Numero(-74.258),Numero(0)))
mostrar(limpiar(expr13))
val expr14 = Suma(Logaritmo(Numero(1)),Resta(Atomo('x'),Numero(0)))
mostrar(limpiar(expr14))
val expr15 = Resta(Expo(Atomo('z'),Numero(1)),Div(Numero(0),Atomo('y')))
mostrar(limpiar(expr15))

def buenaAprox ( f : Expr , a :Atomo , d : Double ) : Boolean = {
    evaluar ( f , a , d) < 0.001
}

def muyBuenaAprox ( f : Expr , a :Atomo , d : Double ) : Boolean = {
    evaluar ( f , a , d) < 0.00001
}

val e1= Suma(Suma(Expo(Atomo('x'),Numero(2.0)),Prod(Numero(2.0),Atomo('x'))),Numero(-8.0))
val e2= Resta (Prod(Atomo('x') ,Atomo('x') ) ,Numero ( 4.0 ) )
val e3 = Suma(Resta (Prod(Atomo( 'x' ) ,Atomo( 'x' ) ) , Numero ( 4.0 ) ) , Prod(Numero ( 3.0 ) ,Atomo( 'x' ) ) )
val e4 = Div(Expo(Numero(8.0),Atomo('y')),Suma(Logaritmo(Numero(1)),Numero(4)))
val e5 = derivar(Prod(Resta(Numero(2),Numero(17.45)),Expo(Numero(14.0),Atomo('Y'))),Atomo('Y'))

raizNewton ( e1 , Atomo( 'x' ) , 7.5 , muyBuenaAprox )
raizNewton ( e2 , Atomo( 'x') , 5.0 , buenaAprox )
raizNewton ( e3 , Atomo( 'x' ) , 2.0 , buenaAprox )
raizNewton ( e4 , Atomo( 'y' ) , 20.0 , muyBuenaAprox )
raizNewton ( e5 , Atomo( 'Y' ) , 1.0 , buenaAprox )

