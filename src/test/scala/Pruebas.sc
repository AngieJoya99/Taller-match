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