% Predicado para verificar si un caracter es '0' o '1'
Digito_Binario('0').
Digito_Binario('1').

% Predicado para verificar si una lista es una cadena binaria de longitud 8
valid_binary(BinaryString) :-
    length(BinaryString, 8),
    maplist(Digito_Binario, BinaryString).

% Predicado para comparar dos cadenas binarias de longitud 8
Comprar_Binarios(BinaryString1, BinaryString2) :-
    valid_binary(BinaryString1),
    valid_binary(BinaryString2),
    BinaryString1 == BinaryString2.
