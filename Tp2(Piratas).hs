type Pirata = (String,[Botin])
type Botin =(String, Int)

cantidadTesoros ::[Botin]-> Int
cantidadTesoros botin =length (map (head) (map (fst) (botin)))

pirataAfortunado:: [Botin]-> Bool
pirataAfortunado botin = (sum (map (snd)(botin)))>10000


piratamismotesoro:: Pirata->Pirata-> Bool
piratamismotesoro pirata1 pirata2=  any (loTiene pirata2)(tesoros pirata1)
loTiene::Pirata-> Botin->Bool
loTiene pirata2 tesoro= any (coincide tesoro) (tesoros pirata2)
coincide:: Botin->Botin->Bool
coincide tesoro1 tesoro2 = nombre tesoro1 ==nombre tesoro2 && valor tesoro1 /=valor tesoro2
tesoros:: Pirata-> [Botin]
tesoros pirata = snd pirata
nombre :: Botin-> String
nombre tesoro = fst tesoro 
valor:: Botin-> Int
valor tesoro= snd tesoro

valorTesoroMas :: Pirata-> Int
valorTesoroMas =  maximum.valores.tesoros
valores::[Botin]->[Int]
valores tesoro = map (snd)tesoro
