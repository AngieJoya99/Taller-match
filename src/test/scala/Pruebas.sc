/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

import Newton._

val expr1 = Suma(Atomo('x'), Numero(2))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2,Numero(5)))
val expr4 = Logaritmo (Atomo('x'))
val expr5 =Prod(Div ( expr1 , expr2 ) , Resta ( expr3 , expr4 ) )
val expr6 = Expo(Atomo('x') ,Numero ( 3 ) )

mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)

mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0),Atomo('x'))), Atomo('x')))

mostrar(Numero(5))
evaluar(Numero(5), Atomo('x'), 1.0)
mostrar(Atomo('x'))
evaluar(Atomo('x'), Atomo('x'), 5.0)
mostrar (Suma( expr1 , expr2 ) )
evaluar (Suma( expr1 , expr2 ) ,Atomo( 'x' ) , 5.0 )
mostrar (Prod( expr1 , expr2 ) )
evaluar (Prod( expr1 , expr2 ) ,Atomo( 'x' ) , 5.0 )
mostrar (Resta ( expr1 , expr2 ) )
evaluar (Resta ( expr1 , expr2 ) ,Atomo( 'x' ) , 5.0 )
mostrar (Div ( expr1 , expr2 ) )
evaluar (Div ( expr1 , expr2 ) ,Atomo( 'x' ) , 5.0 )
mostrar (Expo( expr1 , expr2 ) )
evaluar (Expo( expr1 , expr2 ) ,Atomo( 'x' ) , 5.0 )
mostrar ( Logaritmo ( expr1 ) )
evaluar ( Logaritmo ( expr1 ) ,Atomo( 'x' ) , 5.0 )

limpiar ( derivar (Suma(Atomo( 'k' ) , Prod(Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x' ) ) )
mostrar ( limpiar ( derivar (Suma(Atomo( 'k' ) , Prod(Numero ( 3.0 ) , Atomo( 'x' ) ) ) , Atomo( 'x' ) ) ) )

limpiar(Suma(Numero(8.0),Numero(0)))
limpiar(Suma(Prod(Numero(10),Numero(1)),Numero(0)))
limpiar(Suma(Prod(Atomo('x'),Numero(1)),Numero(0)))
limpiar(derivar(Suma(Prod(Atomo('x'),Numero(1)),Numero(0)),Atomo('x')))
limpiar(Prod(Suma(Numero(8.0),Numero(0)),Prod(Atomo('x'),Numero(1))))
limpiar(Logaritmo(Numero(1)))

def buenaAprox ( f : Expr , a :Atomo , d : Double ) : Boolean = {
    evaluar ( f , a , d) < 0.001
}

val e1= Resta (Prod(Atomo( 'x' ) ,Atomo( 'x' ) ) , Numero ( 2.0 ) )
val e2= Resta (Prod(Atomo( 'x' ) ,Atomo( 'x' ) ) , Numero ( 4.0 ) )
val e3 = Suma(Resta (Prod(Atomo( 'x' ) ,Atomo( 'x' ) ) , Numero ( 4.0 ) ) , Prod(Numero ( 3.0 ) ,Atomo( 'x' ) ) )
val e4 = Numero(0)
val e5 = Atomo( 'y' )

raizNewton ( e1 , Atomo( 'x' ) , 2.0 , buenaAprox )
raizNewton ( e2 , Atomo( 'x') , 2.0 , buenaAprox )
raizNewton ( e3 , Atomo( 'x' ) , 2.0 , buenaAprox )
raizNewton ( e4 , Atomo( 'x' ) , 2.0 , buenaAprox )
raizNewton ( e5 , Atomo( 'x' ) , 2.0 , buenaAprox )