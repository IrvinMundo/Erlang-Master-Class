-module(master).
-compile(export_all).


%%ESTRUCTURA
-type expr() :: {'num',integer()} 
	      | {'var',atom()} 
	      | {'add', expr(), expr()} 
	      | {'mul', expr(), expr()}
	      | {'divi', expr(), expr()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%print o pretty printing, de listas a string
-spec print(expr()) -> string().
print({num, N}) ->
    integer_to_list(N);
print({var, A}) ->
    atom_to_list(A);
print({add, E1, E2}) ->
    "("++ print(E1) ++ "+" ++ print(E2) ++")";
print({mul,E1,E2}) ->
    "("++ print(E1) ++ "*" ++ print(E2) ++")";
print({divi,E1,E2}) ->
    "("++ print(E1) ++ "/" ++ print(E2) ++")".


%%master:print({add,{num,2},{mul,{num,3},{num,4}}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%EVAL para expresiones sin variables
-spec eval(expr()) -> integer().
eval({num, N}) ->
    N;
eval({add, E1, E2}) ->
    eval(E1) + eval(E2);
eval({mul, E1, E2}) ->
    eval(E1) * eval(E2);
eval({divi, E1, E2}) ->
    eval(E1) div eval(E2).
    


%%master:eval({add,{num,2},{mul,{num,3},{num,4}}}).



%%EVAL para expresiones con variable
%%env sirve para dar la variable y su valor
-type env() :: [{atom(), integer()}].
%%evalua expresion con valores de variables
-spec eval(env(), expr()) -> integer().
eval(_Env, {num, N}) ->
    N;
eval(Env, {var, A}) ->
    lookup(A, Env);
eval(Env, {add, E1, E2}) ->
    eval(Env, E1) + eval(Env, E2);
eval(Env, {mul, E1, E2}) ->
    eval(Env, E1) * eval(Env, E2);
eval(Env, {divi, E1, E2}) ->
    eval(Env, E1) div eval(Env, E2).
%%Busca el valor de la variable
-spec lookup(atom(), env()) -> integer().
lookup(A, [{A, V} | _]) ->
    V;
lookup(A, [_ | Rest]) ->
    lookup(A, Rest).


%%Var = [{a,2},{b,4}].
%%master:eval(Var,{add,{var,a},{mul,{num,3},{var,b}}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%VIRTUAL MACHINE
%%push añade un numero a la pila
%%fetch añade el valor de una variable a la pila
%%add2 suma los dos numeros de arriba de la pila
%%mul2 multiplica los dos números de arriba de la pila

-type instr() :: {'push', integer()}
              |  {'fetch', atom()}
              |  {'add2'}
              |  {'mul2'}
	      |  {'divi2'}.

-type program() :: [instr()].

-type stack() :: [integer()].

%%revisa la expresión y la convierte en programa que es la lista de instrucciones
-spec compile(expr()) -> program().
compile({num, N}) ->
    [{push, N}];
compile({var, A}) ->
    [{fetch, A}];
compile({add, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}];
compile({divi, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{divi2}].



%%prepara el programa,crea la pila y llama al run con tres parámetros
-spec run(program(),env()) -> integer().   
run(Code,Env) ->
    run(Code,Env,[]).


%%mete valores a la pila, hace operaciones y recorre el programa
-spec run(program(), env(), stack()) -> integer().
run([{push, N} | Continue], Env, Stack) ->
    run(Continue, Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
    run(Continue, Env, [lookup(A, Env) | Stack]);
run([{add2} | Continue], Env, [N1, N2 | Stack]) ->
    run(Continue, Env, [(N1+N2) | Stack]);
run([{mul2} | Continue], Env ,[N1, N2 | Stack]) ->
    run(Continue, Env, [(N1*N2) | Stack]);
run([{divi2} | Continue], Env ,[N1, N2 | Stack]) ->
    run(Continue, Env, [(N2 div N1) | Stack]);
run([], _Env, [N]) ->
    N.




%%Var = [{a,2},{b,4}].
%%Programa = master:compile({add,{var,a},{mul,{num,3},{var,b}}}).
%%master:run(Programa,Var).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%PARSE para convertir un string en una expresión
-spec parse(string()) -> {expr(), string()}.
parse([$( | Rest]) ->                          
      {E1, Rest1}     = parse(Rest),
      [Op | Rest2]     = Rest1,
      {E2, Rest3}     = parse(Rest2),         
      [$) | RestFinal] = Rest3,
      {case Op of
	  $+ -> {add, E1, E2};
	  $* -> {mul, E1, E2}
        end,
       RestFinal};
parse([Ch | Rest]) when ($0 =< Ch andalso Ch =< $9) orelse Ch==$- ->
    {Succeeds, Remainder} = get_while(fun is_digit/1, Rest),
    {{num, list_to_integer([Ch | Succeeds])}, Remainder};
parse([Ch | Rest])  when $a =< Ch andalso Ch =< $z ->
    {Succeeds, Remainder} = get_while(fun is_alpha/1, Rest),
    {{var, list_to_atom([Ch | Succeeds])}, Remainder}.


-spec get_while(fun((T) -> boolean()), [T]) -> {[T], [T]}.		 
get_while(P, [Ch | Rest]) ->
    case P(Ch) of
	true ->
	    {Succeeds, Remainder} = get_while(P, Rest),
	    {[Ch | Succeeds], Remainder};
	false ->
	    {[], [Ch | Rest]}
    end;
get_while(_P, []) ->
    {[], []}.


-spec is_digit(integer()) -> boolean().
is_digit(Ch) ->
    $0 =< Ch andalso Ch =< $9.

-spec is_alpha(integer()) -> boolean().
is_alpha(Ch) ->
    $a =< Ch andalso Ch =< $z.


%%master:parse("(2+(3*4))").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%SIMPLIFICATION
%%Simplifica cuando trata de sumar algo con 0
zeroA({add, E, {num, 0}}) ->
    E;
zeroA({add, {num, 0}, E}) ->
    E;
zeroA(E) ->
    E.

%%SImplifica cuando trata de multiplicar algo por 1
mulO({mul, E, {num, 1}}) ->
    E;
mulO({mul, {num, 1}, E}) ->
    E;
mulO(E) ->
    E.

%%Simplifica cuando trata de multiplicar algo por 0
mulZ({mul, _, {num, 0}}) ->
    {num, 0};
mulZ({mul, {num, 0}, _}) ->
    {num, 0};
mulZ(E) ->
    E.

%%Aplica las reglas
compose([]) ->
    fun (E) -> E end;
compose([Rule | Rules]) ->
    fun (E) -> (compose(Rules))(Rule(E)) end.

%%Define la lista de reglas
rules() ->
    [ fun zeroA/1, fun mulO/1, fun mulZ/1].

%%Consideramos todos los casos a simplificar
simp(F, {add, E1, E2}) ->
    F({add, simp(F, E1), simp(F, E2)});
simp(F, {mul, E1, E2}) ->
    F({mul, simp(F, E1), simp(F, E2)});
simp(_F, E) -> E.

simplify(E) ->
    simp(compose(rules()), E).


%%Expr2 = {add,{mul,{num,1},{var,b}},{mul,{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}},{num,0}}}.
%%master:print(master:simplify(Expr2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
