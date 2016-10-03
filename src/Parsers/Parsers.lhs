Encabezado y algunas importaciones

> module Parsers where
> import Data.Char (isDigit)


El tipo de un parser:

> type Parser a = String -> [(a,String)]



Podemos programar parsers para reconocer caracteres particulares, por ejemplo
parsear un dígito, retornando el entero correspondiente: 

> pDigit :: Parser Int   --  ~ String -> [(Int, String)] 
> pDigit []     = []
> pDigit (c:cs) = if isDigit c
>                 then [(read [c], cs)]
>                 else []


Parsear un paréntesis izquierdo, retornandolo:

> pLParen :: Parser Char
> pLParen [] = []
> pLParen (c:cs) = if c == '('
>                  then [('(', cs)]
>                  else []


Lo anterior se puede generalizar a cualquier carácter:

> pToken :: Char -> Parser Char
> pToken t []     = []
> pToken t (c:cs) = if c == t
>                   then [(c,cs)]
>                   else []

Entonces podemos parsear un paréntesis derecho reutilizando esta función:

> pRParen :: Parser Char
> pRParen = pToken ')'

Podríamos redefinir lo anterior:

< pLParen = pToken '('


> --------

Lo que tenemos hasta ahora son parsers de tokens, tenemos que generar algún
mecanismo más poderoso para parsear cosas más útiles.

Un combinador de parsers, es una función que genera parsers a partir de parsers.



El primer ejemplo de combinador es la selección, el or lógico.
Dados dos parsers, utilizar o bien uno, o el otro:

> (<|>) :: Parser a -> Parser a -> Parser a
> p <|> q = \cs -> p cs ++ q cs


Por ejemplo, para parsear cualquier paréntesis:

> pParen = pLParen <|> pRParen



Otro combinador interesante es la secuenciación.

Dados dos parsers, aplicar uno y luego el otro, en donde el segundo utiliza
la como entrada el String resultante de haber aplicado el primero.
Se retorna el par (a,b), con los valores de retorno de uno y otro parser. 

> (<+>) :: Parser a -> Parser b -> Parser (a,b)
> p <+> q = \cs -> [ ((c,c'), cs'') | (c, cs') <- p cs, (c',cs'') <- q cs']


(más sobre ésta sintaxis: https://wiki.haskell.org/List_comprehension)

Éste combinador no es escalable.
Si queremos aplicar n veces un parser, el mismo va a tener tipo

< Parser ((..((a,b),c),d),...)

Extraer los valores de retorno y hacer cosas con ellos es complicado.

En general queremos hacer algo con el tipo de retorno, en lugar de acumular
los resultados en pares anidados..


Surge el siguiente combinador:

> (<*>) :: Parser (a -> b) -> Parser a -> Parser b
> p <*> q = \cs -> [ (f c, cs'') | (f, cs') <- p cs, (c,cs'') <- q cs']


Antes de ver un ejemplo, veamos otro combinador, trivial,
que dado un valor lo retorna, siempre terminando exitosamente,
sin modificar la entrada:

> pSucceed :: a -> Parser a
> pSucceed a = \cs -> [(a,cs)]


A modo de ejemplo, puede implementarse <+> a partir de <*>:

> p <++> q = pSucceed (,) <*> p <*> q


El combinador <*> es muy poderoso, veamos cómo utilizarlo.

Por ejemplo, con lo que tenemos hasta ahora podemos definir un combinador que
dado un parser lo aplique múltiples veces, (reconociendo una lista de cosas):

> pList :: Parser a -> Parser [a]
> pList p =     pSucceed (:) <*> p <*> pList p
>           <|> pSucceed [] 


Por ejemplo

< pList pDigit $ "123"

reduce a:

[([1,2,3],""),([1,2],"3"),([1],"23"),([],"123")]





Yendo al problema del práctico
Cómo parseamos un entero?

Una opción es usar "pList pDigit" y combinar el resultado con una función que,
dada una lista de digitos arme el entero:

La forma más fácil es considerando la lista al revés, podemos hacerlo de esa
forma (es un buen ejercicio pensar cómo hacerlo sin el reverse)

> listToInt :: [Int] -> Int
> listToInt = listToIntRev . reverse

> listToIntRev :: [Int] -> Int
> listToIntRev [] = 0
> listToIntRev (d:ds) = d + 10 * listToIntRev ds


Entonces:

> pInt = pSucceed listToInt <*> pList pDigit


Para parsear los operadores:

> pPlus :: Parser (Int-> Int -> Int)
> pPlus  = pSucceed (\_ -> (+)) <*> pToken '+'


> pMinus = pSucceed (\_ -> (-)) <*> pToken '-'
> pTimes = pSucceed (\_ -> (*)) <*> pToken '*'
> pDiv   = pSucceed (\_ -> div) <*> pToken '/'

> pOpt =    pPlus
>       <|> pMinus
>       <|> pTimes
>       <|> pDiv

Para probar pPlus e imprimirlo se puede definir esto
(habilitando -XFlexibleInstances):

< instance Show (Int-> Int -> Int)where
<   show _ = "algo"

> pSimpleExpr :: Parser Int
> pSimpleExpr = pSucceed (\l op r -> op l r ) <*> pInt <*> pOpt <*> pInt

Como este patrón que consiste en usar pSucceed para "inyectar" una función
en el contexto de los parsers se utiliza mucho, podemos definir lo siguiente:

> (<$>) :: (a -> b) -> Parser a -> Parser b
> f <$> p = pSucceed f <*> p

Así, podemos redefinir el parser de expresiones simples:

> pSimpleExpr' = (\l op r -> op l r ) <$> pInt <*> pOpt <*> pInt


El parser de expresiones, puede modelarse así:

> pExpr :: Parser Int
> pExpr =    pInt
>        <|> pSimpleExpr 
>        <|> ((\lp l op r rp -> op l r )
>             <$> pLParen <*> pExpr <*> pOpt <*> pExpr <*> pRParen) 


Hasta ahora ignoramos los espacios, ya que no es necesario considerarlos,
podríamos incluirlos en la gramática, o podemos directamente eliminarlos antes
de parsear:
En la teoría de compiladores, 

> lexer :: String -> String
> lexer = filter (/=' ')

> calculator :: String -> String
> calculator = show . fst . head . filter (\(c,cs) -> cs == "") . pExpr . lexer

  https://wiki.haskell.org/Pointfree





Bibliotecas con combinadores:

https://hackage.haskell.org/package/parsec
https://hackage.haskell.org/package/parsers
https://hackage.haskell.org/package/uu-parsinglib
  






Ejercicio (opcional, si les gustó el tema):

Dados los siguientes tipo de datos, que son un posible modelo del árbol de
sintaxis abstracta del lenguaje de expresiones del práctico
(si quieren pueden modificarlo como crean conveniente):

> data Op = Plus | Minus | Times | Div deriving (Show)

> data Expr = Leaf Int
>           | Node Op Expr Expr


Implementar una función <parse :: String -> Expr> que dada una expresión,
construya el tipo de datos correspondiente (usando parsers, obvio!), por ejemplo:

parse "(1+2)*5"

reduce a:  Node Times (Node Plus (Leaf 1) (Leaf 2)) (Leaf 5)


Luego implementar <eval::Expr -> Int> que evalúe las expresiones.

Discutir qué ventajas puede tener trabajar sobre el árbol en haskell al evaluar
en lugar de hacerlo directamente, como se hizo el ejemplo de este texto.
