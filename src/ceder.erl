%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    A walk among Erlang forms
%%% @end
%%% Created : 22 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(ceder).

-compile(export_all).
-export([start/1, file/1]).

%% -define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A))).

%% Parse forms install and traverse
start([Filename]) when is_atom(Filename) -> 
    file_(atom_to_list(Filename)).

file([Filename]) when is_atom(Filename) -> 
    file_(atom_to_list(Filename));
file(Filename) when is_list(Filename) ->
    file(Filename).

file_(Filename) ->
    case file:read_file(Filename) of
	{ok,Bin} ->
	    start_prog(binary_to_list(Bin)),
	    halt(0);
	Error -> 
	    Error
    end.
    
start_prog(String) ->
    Forms = parse(String),
    init_id(),
    ID = next_id(),
    {Rs,Map1} = pack_forms(ID, Forms, #{}),
    {Prog,Map2} = install_node(ID, prog, {prog,ID,Rs}, 0, Map1),
    TTY = tty:open(),
    loop(TTY,{ID,Map2#{ root => Prog }}, ".").

%% Parse expression, install and traverse
start_expr(String) ->
    Expr = parse_expr(String),
    init_id(),
    {R,Map} = pack(0, expr, Expr, #{}),
    Root = {R, Map#{ root => R }},
    TTY = tty:open(),
    loop(TTY,Root, ".").

parse_file(Filename) ->
    case file:read_file(Filename) of
	{ok,Bin} -> parse(Bin);
	Error -> Error
    end.

parse(Binary) when is_binary(Binary) ->
    parse(binary_to_list(Binary));
parse(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    parse_(Ts, [], []).
parse_([D={'dot',_}|Ts],Qs,Fs) ->
    {ok,F} = erl_parse:parse_form(lists:reverse([D|Qs])),
    parse_(Ts,[],[F|Fs]);
parse_([T|Ts],Qs,Fs) ->
    parse_(Ts,[T|Qs],Fs);
parse_([],[],Fs) ->
    lists:reverse(Fs).

%% parse expression form string
parse_expr(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    {ok,[Expr]} = erl_parse:parse_exprs(Ts++[{dot,0}]),
    Expr.

parse_form(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    {ok,F} = erl_parse:parse_form(Ts),
    F.

pp_expr(Expr) ->
    String1 = format_(Expr),
    lists:flatten(String1).

pp_string(String) ->
    Expr = parse_expr(String),
    pp_expr(Expr).

remove_white_space(Cs) ->
    case rws(Cs) of
	[$\s|Cs1] -> Cs1;
	Cs1 -> Cs1
    end.

rws([$\s|Cs]) -> rws_(Cs);
rws([$\t|Cs]) -> rws_(Cs);
rws([$\n|Cs]) -> rws_(Cs);
rws([$\r|Cs]) -> rws_(Cs);
rws([C|Cs]) -> [C|rws(Cs)];
rws([]) -> [].

rws_([$\s|Cs]) -> rws_(Cs);
rws_([$\t|Cs]) -> rws_(Cs);
rws_([$\n|Cs]) -> rws_(Cs);
rws_([$\r|Cs]) -> rws_(Cs);
rws_([C|Cs]) -> [$\s,C|rws(Cs)];
rws_([]) -> [].


%% "node" interface

%% extract a list of child nodes
kids({I,Map}) when is_integer(I), is_map(Map) ->
    kids(maps:get({rule,I},Map),maps:get(I,Map)).

unpack({I,Map}) ->
    unpack(maps:get({rule,I},Map),I,Map).

rule({I,Map}) when is_integer(I), is_map(Map) ->
    maps:get({rule,I}, Map).

root({_,Map}) when is_map(Map) ->
    {maps:get(root,Map), Map}.

get_node({I,Map}) when is_integer(I), is_map(Map) ->
    maps:get(I,Map).
get_node(I,Map) when is_integer(I), is_map(Map) ->
    maps:get(I,Map).

lookup_node(Key,{N,Map}) ->
    lookup_node(Key,{N,Map},undefined).
lookup_node(Key,{N,Map},Default) ->
    lookup_node(Key,N,Map,Default).
lookup_node(Key,N,Map,Default) ->
    maps:get({Key,N},Map,Default).

pack_forms(_P, [], Tab) ->
    {[],Tab};
pack_forms(P, [Form|Fs], Tab0) ->
    {X,Tab1} = pack(P, form, Form, Tab0),
    {Xs,Tab2} = pack_forms(P, Fs, Tab1),
    {[X|Xs],Tab2}.

install_node(ID, Rule, Form, Parent, Map) ->
    Map1 = Map#{ ID => Form, {rule,ID} => Rule, {parent,ID} => Parent },
    Map2 = set_siblings(kids(Rule,Form), Map1),
    {ID, Map2}.

%% set siblings chain
set_siblings([], Tab) ->   Tab;
set_siblings([_], Tab) ->  Tab;
set_siblings([A|Bs=[B|_]], Tab) ->
    Tab1 = Tab#{ {next, A} => B,
		 {prev, B} => A },
    set_siblings(Bs, Tab1).

loop(TTY,Node,Default) ->
    tty:outputf(TTY, "~w> ", [element(1,Node)]), %% output node number
    case tty:input(TTY) of
	eof ->
	    Node;
	Key ->
	    if is_integer(Key) ->
		    tty:output(TTY, [Key,$\r,$\n]);
	       is_atom(Key) ->
		    tty:output(TTY, [atom_to_list(Key),$\r,$\n])
	    end,
	    dispatch(TTY,Node,Key,Default)
    end.

dispatch(TTY,Node,Key,Default) ->
    case Key of
	up -> loop(TTY,up(Node),     $u);
	$u -> loop(TTY,up(Node),     $u);
	down -> loop(TTY,down(Node), $d);
	$d -> loop(TTY,down(Node),   $d);
	left -> loop(TTY,r_prev(Node), $p);
	$p -> loop(TTY,r_prev(Node), $p);
	right -> loop(TTY,r_next(Node), $n);
	$n -> loop(TTY,r_next(Node), $n);
	$t -> loop(TTY,root(Node),   $.);
	$f -> loop(TTY,first(Node),  $n);
	$l -> loop(TTY,last(Node),   $p);
	$? -> help(TTY,Node,Default);
	$. -> loop(TTY,Node,".");
	$q ->
	    tty:outputf(TTY, "~p\n", [unpack(Node)]),
	    halt(0);
	$P -> 
	    Term = unpack(Node),
	    tty:outputf(TTY, "~s: |~s|\n", [rule(Node),format_(Term)]),
	    loop(TTY,Node, Default);
	$L ->
	    lint(TTY, Node, Default);
	$\r -> 
	    dispatch(TTY,Node,Default,Default);
	_ ->
	    tty:outputf(TTY, "? [~w] h for help\n", [Key]),
	    loop(TTY,Node,$.)
    end.

lint(TTY, Node, Default) ->
    case get_node(Node) of %% just unpack one level to check
	{prog,_,_} ->
	    {_,_,Fs} = unpack(Node), %% it's a program unpack and lint
	    lint_(TTY, Node, Node, Default, Fs);
	{'function',_,_Name,_Arity,_Cs} -> 
	    %% add some attributes to make it a module
	    {X3,Map} = Node,
	    {X1,Map1} = pack(0,form,{'attribute',1,module,none}, Map),
	    {X2,Map2} = pack(0,form,{'attribute',2,compile,export_all},Map1),
	    ID = next_id(),
	    %% this is a dummy node for showing warnings/errors
	    Node1 = install_node(ID, prog, {prog,ID,[X1,X2,X3]},0,Map2),
	    {_,_,Fs} = unpack(Node1), %% it's a program unpack and lint
	    lint_(TTY, Node1, Node, Default, Fs);
	_ -> %% not a function (add more dummy structures?)
	    loop(TTY,Node,Default)
    end.

lint_(TTY, Node, Node0, Default, Forms) -> 
    %% strange for some reason sys_core_fold is not called? so
    %% the badarith is not printed when trying the test_fun in ceder.erl
    case erl_lint:module(Forms, "nofile", [strong_validation]) of
	{ok,Ws} ->
	    show_warnings(TTY,Ws,Node),
	    loop(TTY,Node0,Default);
	{error,Es,Ws} ->
	    show_warnings(TTY,Ws,Node),
	    show_errors(TTY,Es,Node),
	    loop(TTY,Node0,Default)
    end.
    

help(TTY, Node, Default) ->
    tty:outputf(TTY,
		"u | up    : up\n"
		"d | down  : down\n"
		"p | left  : left (previous)\n"
		"n | right : right (next)\n"
		"t | home  : top\n"
		"f |       : first\n"
		"l | end   : last\n"
		"P |       : print current node\n"
		"L |       : lint node (function/program)\n"
		"q        : quit\n"),
    loop(TTY,Node,Default).


show_warnings(TTY,[{File,Ws}|FWs],Node) ->
    lists:foreach(
      fun(W) ->
	      show_mesg(TTY,File,warn,W,Node)
      end, Ws),
    show_warnings(TTY,FWs, Node);
show_warnings(_TTY,[],_Node) ->
    ok.

show_errors(TTY,[{File,Es}|FEs],Node) ->
    lists:foreach(
      fun(E) ->
	      show_mesg(TTY,File,error,E,Node)
      end, Es),
    show_errors(TTY,FEs,Node);
show_errors(_TTY,[],_Node) ->
    ok.

show_mesg(TTY,File,What,{Line,Mod,Message},Node) ->
    tty:outputf(TTY,
		"~s:~w: ~w, ~s\n",
		[File, Line,What,erlang:apply(Mod, format_error, [Message])]),
    WNode = goto(Line, Node),
    Term = unpack(WNode),    
    tty:outputf(TTY,"|~s|\n", [format_(Term)]).
    

goto(N,{_,Map}) ->
    {N,Map}.

down(Node) -> %% down left
    case kids(Node) of
	[] -> Node;  %% bottom
	[First|_Rest] -> goto(First,Node)
    end.

up(Node) ->
    case lookup_node(parent,Node) of
	0 -> Node;
	Parent -> goto(Parent,Node)
    end.

prev(Node) ->
    case lookup_node(prev,Node) of
	undefined -> Node;
	Prev -> goto(Prev,Node)
    end.

first(Node) ->
    case lookup_node(prev,Node) of
	undefined -> Node;
	Next -> first(goto(Next,Node))
    end.

next(Node) ->
    case lookup_node(next,Node) of
	undefined -> Node;
	Next -> goto(Next,Node)
    end.

last(Node) ->
    case lookup_node(next,Node) of
	undefined -> Node;
	Next -> last(goto(Next,Node))
    end.

r_prev(Node) ->
    case lookup_node(prev,Node) of
	undefined -> prev(up(Node));
	Prev -> goto(Prev,Node)
    end.

r_next(Node) ->
    case lookup_node(next,Node) of
	undefined -> next(up(Node));
	Next -> goto(Next,Node)
    end.

init_id() ->
    put(next_id, 1).

next_id() ->
    ID = case get(next_id) of
	     undefined -> 1;
	     I -> I
	 end,
    put(next_id, ID+1),
    ID.


format(Node) ->
    format_(unpack(Node)).

format_(Expr) ->
    case Expr of
	E = {bin_element,_ID,_E,_S,_T} ->
	    String = lists:flatten(erl_pp:expr({'bin',0,[E]})),
	    string:substr(String,3,length(String)-4);
	E = {map_field_exact,_ID,_K,_V} ->
	    String = lists:flatten(erl_pp:expr({'map',0,[E]})),
	    string:substr(String,3,length(String)-3);
	E = {map_field_assoc,_ID,_K,_V} ->
	    String = lists:flatten(erl_pp:expr({'map',0,[E]})),
	    string:substr(String,3,length(String)-3);
	E = {record_field,_ID,_K,_V} ->
	    String = lists:flatten(erl_pp:expr({'record',0,r,[E]})),
	    string:substr(String,4,length(String)-4);
	E = {clause,_ID,[],_G,_B} ->
	    String = lists:flatten(erl_pp:expr({'if',0,[E]})),
	    String1 = remove_white_space(String),
	    string:substr(String1,4,length(String1)-7);
	E = {clause,_ID,[_],_G,_B} ->
	    String = lists:flatten(erl_pp:expr({'case',0,{integer,0,0},[E]})),
	    String1 = remove_white_space(String),
	    string:substr(String1,11,length(String1)-14);
	E = {clause,_ID,_H,_G,_B} ->
	    String = lists:flatten(erl_pp:expr({'fun',0,{clauses,[E]}})),
	    String1 = remove_white_space(String),
	    string:substr(String1,4,length(String1)-7);
	E = {'generate',_ID,_P,_L} ->
	    String = lists:flatten(erl_pp:expr({'lc',0,{integer,0,0},[E]})),
	    String1 = remove_white_space(String),
	    string:substr(String1,8,length(String1)-9);
	E = {'b_generate',_ID,_P,_B} ->
	    String = lists:flatten(erl_pp:expr({'lc',0,{integer,0,0},[E]})),
	    String1 = remove_white_space(String),
	    string:substr(String1,8,length(String1)-9);
	P = {'function',_,_Name,_Arity,_Cs} ->
	    erl_pp:form(P);
	A = {'attribute',_,_Name,_Value} ->
	    erl_pp:form(A);
	{prog,_,Fs} ->
	    [erl_pp:form(F) || F <- Fs];
	_ ->
	    erl_pp:expr(Expr)
    end.

kids(Rule, Tuple) when is_tuple(Tuple) ->
    Tag = element(1,Tuple),
    Size = tuple_size(Tuple),
    kids_(Rule, Tag, Size, Tuple).

kids_(Rule, Tag, Size, Tuple) ->
    kids__(spec_(Rule,Tag,Size),1,Tuple).

kids__([T|Ts],I,Tuple) ->
    kid(T,I,Tuple) ++ kids__(Ts,I+1,Tuple);
kids__([],_,_) ->
    [].

kid({rule,_},I,Tuple) ->
    [element(I,Tuple)];
kid({list,{rule,_}},I,Tuple) ->
    element(I,Tuple);
kid({list,{list,{rule,_}}},I,Tuple) ->
    lists:flatten(element(I,Tuple));
kid({part,R},I,Tuple) ->
    TupleI = element(I,Tuple),
    Spec = spec(R,TupleI),
    if is_list(Spec) ->
	    kids__(Spec,1,TupleI);
       true ->
	    kid(Spec,1,TupleI)
    end;
kid(_, _, _) ->
    [].

%%
%% packing of forms/expr
%%

pack_form(String) ->
    Form = parse_form(String),
    init_id(),
    pack(0, form, Form, #{}).

pack_expr(String) ->
    Expr = parse_expr(String),
    init_id(),
    pack(0, expr, Expr, #{}).

pack(Parent,Rule,Tuple,Map) ->
    ID = next_id(),
    {Tuple1,Map1} = pack_part(ID,Rule,Tuple,Map),
    install_node(ID, Rule, Tuple1, Parent, Map1).

pack_part(Parent,Rule,Tuple,Map) ->
    Spec = spec(Rule,Tuple),
    if is_list(Spec) ->
	    pack_elems(Parent,Spec,1,Tuple,Map,[]);
       true ->
	    pack_elem(Parent,Spec,Tuple,Map)
    end.

pack_elem(_ID,{lit,L},_Elem,Map) ->  %% match Elem and L?
    {L,Map};
pack_elem(ID,anno,_Elem,Map) ->
    {ID,Map};
pack_elem(ID,{rule,R},Elem,Map) ->
    pack(ID, R, Elem, Map);    
pack_elem(ID,{part,R},Elem,Map) ->
    pack_part(ID,R,Elem,Map);
pack_elem(ID,{list,R},Elem,Map) ->
    pack_list(ID, R, Elem, Map, []);
pack_elem(_ID,_T,Elem,Map) -> %% check type?
    {Elem,Map}.

pack_elems(ID,[T|Ts],I,Tuple,Map,Acc) ->
    {E,Map1} = pack_elem(ID,T,element(I,Tuple),Map),
    pack_elems(ID,Ts,I+1,Tuple,Map1,[E|Acc]);
pack_elems(_ID,[],_I,_Tuple,Map,Acc) ->
    {list_to_tuple(lists:reverse(Acc)), Map}.

pack_list(ID,Rule={rule,R},[E|Es],Map,Acc) ->
    {X,Map1} = pack(ID, R, E, Map),
    pack_list(ID,Rule,Es,Map1,[X|Acc]);
pack_list(ID,Rule={part,R},[E|Es],Map,Acc) ->
    {X,Map1} = pack_part(ID, R, E, Map),
    pack_list(ID,Rule,Es,Map1,[X|Acc]);
pack_list(ID,Rule={list,R},[E|Es],Map,Acc) ->
    {X,Map1} = pack_list(ID, R, E, Map, []),
    pack_list(ID,Rule,Es,Map1,[X|Acc]);
pack_list(_ID,_Rule,[],Map,Acc) ->
    {lists:reverse(Acc),Map}.

%%
%% Unpacking of forms/expr
%%
unpack_expr(I,Map) ->
    unpack(expr,I,Map).

unpack_form(I,Map) ->
    unpack(form,I,Map).

unpack(Rule,I,Map) ->
    Elem = get_node(I,Map),
    unpack_part(Rule,Elem,Map).

unpack_part(Rule,Elem,Map) ->
    Spec = spec(Rule,Elem),
    if is_list(Spec) ->
	    unpack_elems(Spec,1,Elem,Map,[]);
       true ->
	    unpack_elem(Spec,Elem,Map)
    end.

unpack_elems([T|Ts],I,Tuple,Map,Acc) ->
    E = unpack_elem(T,element(I,Tuple),Map),
    unpack_elems(Ts,I+1,Tuple,Map,[E|Acc]);
unpack_elems([],_I,_Tuple,_Map,Acc) ->
    list_to_tuple(lists:reverse(Acc)).

%% unpack
unpack_elem({lit,L}, _Elem, _Map) -> %% math Elem and L?
    L;
unpack_elem(anno, Elem, _Map) ->
    Elem;
%%  0;
unpack_elem({rule,R},Elem,Map) ->
    unpack(R, Elem, Map);
unpack_elem({part,R},Elem,Map) ->
    unpack_part(R, Elem, Map);
unpack_elem({list,R},Elem,Map) ->
    unpack_list(R, Elem, Map);
unpack_elem(_, Elem, _Map) ->  %% check type?
    Elem.

unpack_list(Rule={rule,R}, [H|T], Map) ->
    [unpack(R,H,Map) | unpack_list(Rule,T,Map)];
unpack_list(Rule={part,R}, [H|T], Map) ->
    [unpack_part(R,H,Map) | unpack_list(Rule,T,Map)];
unpack_list(Rule={list,R}, [H|T], Map) ->
    [unpack_list(R,H,Map) | unpack_list(Rule,T,Map)];
unpack_list(_R, [], _Map) ->
    [].

spec(Rule,Tuple) when is_tuple(Tuple) ->
    spec_(Rule,element(1,Tuple),tuple_size(Tuple));
spec(Rule,Term) ->
    spec_(Rule,Term,0).

spec_(Rule,Tag,Size) ->
    erl(Rule,Tag,Size).

%%
%% FIXME:
%%   edit simple nodes, literal
%%   allow literal nodes to be updated
%%   and changed.
%%      
%%   add comments - as attribute? annotation?
%%   add types
%%
%%  test to add for C code

%% prog
erl(prog,_,_) ->
    [{lit,prog},anno,{list,{rule,form}}];

%% form
erl(form,'function',5) ->
    [{lit,function},anno,atom,integer,{list,{rule,clause}}];
erl(form,'attribute',4) ->
    [{lit,attribute},anno,atom,{part,attr}];

%% attr
erl(attr,A,0) when is_atom(A) -> {lit,A};
erl(attr,L,0) when is_list(L) -> {lit,L};
%% fixme: import!
erl(attr,R,2) when is_atom(R) -> [{lit,R},{list,{rule,record_field}}];
erl(attr,F, 2) when is_list(F) -> [{lit,F},integer];

%% record_field
erl(record_field,'record_field',3) ->
    [{lit,record_field},anno,{rule,expr}];  %% expr=atom!?
erl(record_field,'record_field',4) ->
    [{lit,record_field},anno,{part,atom_expr},{rule,expr}];
erl(record_field,'typed_record_field',3) ->
    [{lit,typed_record_field},{part,record_field},{rule,type}];

%% atom_expr atom|expr
erl(atom_expr,F,0) -> {lit,F};
erl(atom_expr,_Expr,_) -> {rule,expr};

%% expr - most of erlang
erl(expr,'char',3)    -> [{lit,char},anno,char];
erl(expr,'integer',3) -> [{lit,integer},anno,integer];
erl(expr,'float',3)   -> [{lit,float},anno,float];
erl(expr,'atom',3)    -> [{lit,atom},anno,atom];
erl(expr,'string',3)  -> [{lit,string},anno,string];
erl(expr,'match',4)   -> [{lit,match},anno,{rule,pattern},{rule,expr}];
erl(expr,'var',3)     -> [{lit,var},anno,{part,var}];
erl(expr,'tuple',3)   -> [{lit,tuple},anno,{list,{rule,expr}}];
erl(expr,'call',4)    -> [{lit,call},anno,{rule,expr},{list,{rule,expr}}];
erl(expr,'remote',4)  -> [{lit,remote},anno,{rule,expr},{rule,expr}];
erl(expr,'nil',2)     -> [{lit,nil},anno];
erl(expr,'cons',4)    -> [{lit,cons},anno,{rule,expr},{rule,expr}];
erl(expr,'op',4)      -> [{lit,op},anno,{part,unary_op},{rule,expr}];
erl(expr,'op',5)      -> [{lit,op},anno,{part,binary_op},{rule,expr},{rule,expr}];
erl(expr,'map',3)     -> [{lit,map},anno,{list,{rule,map_elem_assoc}}];
erl(expr,'map',4)     -> [{lit,map},anno,{rule,expr},{list,{rule,map_elem_assoc}}];
erl(expr,'if',3)      -> [{lit,'if'},anno,{list,{rule,clause}}];
erl(expr,'case',4)    -> [{lit,'case'},anno,{rule,expr},{list,{rule,clause}}];
erl(expr,'try',6)     -> [{lit,'try'},anno,{list,{rule,expr}},
			  {list,{rule,clause}},{list,{rule,clause}},
			  {list,{rule,expr}}];
erl(expr,'receive',3) -> [{lit,'receive'},anno,{list,{rule,clause}}];
erl(expr,'receive',5) -> [{lit,'receive'},anno,{list,{rule,clause}},
			  {rule,expr},{list,{rule,expr}}];
erl(expr,'catch',3)   -> [{lit,'catch'},anno,{rule,expr}];
erl(expr,'block',3)   -> [{lit,block},anno,{list,{rule,expr}}];
erl(expr,'fun',3)     -> [{lit,'fun'},anno,{part,fun_expr}];
erl(expr,'named_fun',4) -> [{lit,named_fun},anno,atom,{list,{rule,clause}}];
erl(expr,'lc',4)      -> [{lit,lc},anno,{rule,expr},{list,{rule,generator}}];
erl(expr,'bc',4)      -> [{lit,bc},anno,{rule,expr},{list,{rule,generator}}];
erl(expr,'bin',3)     -> [{lit,bin},anno,{list,{rule,bin_element_expr}}];
erl(expr,'record',4) -> [{lit,record},anno,atom,{list,{rule,record_field}}];
erl(expr,'record',5) -> [{lit,record},anno,{rule,expr},atom,
			 {list,{rule,record_field}}];
erl(expr,'record_index',4) -> [{lit,record_index},anno,atom,atom];

%% Here we can check if variable name is well formed
erl(var,Name, 0) ->
    {lit,Name};

%% Here we can check that operator is a valid unary operator
erl(unary_op,Op,0) ->
    {lit,Op};

%% Here we can check that operator is a valid binay operator
erl(binary_op,Op,0) ->
    {lit,Op};

erl(bin_element_expr,'bin_element',5) ->
    [{lit,bin_element},anno,{rule,expr},{part,bin_size},{part,bin_type}];

%% pattern
erl(pattern,'char',3)    -> [{lit,char},anno,char];
erl(pattern,'integer',3) -> [{lit,integer},anno,integer];
erl(pattern,'float',3)   -> [{lit,float},anno,float];
erl(pattern,'atom',3)    -> [{lit,atom},anno,atom];
erl(pattern,'string',3)  -> [{lit,string},anno,string];
erl(pattern,'match',4)   -> [{lit,match},anno,{rule,pattern},{rule,pattern}];
erl(pattern,'var',3)     -> [{lit,var},anno,{part,var_pattern}];
erl(pattern,'tuple',3)   -> [{lit,tuple},anno,{list,{rule,pattern}}];
erl(pattern,'nil',2)     -> [{lit,nil},anno];
erl(pattern,'cons',4)    -> [{lit,cons},anno,{rule,pattern},{rule,pattern}];
erl(pattern,'bin',3)     -> [{lit,bin},anno,{list,{rule,bin_element_pattern}}];
erl(pattern,'op',4)      -> [{lit,op},anno,{part,unary_op},{rule,pattern}];
erl(pattern,'op',5)      -> [{lit,op},anno,{part,binary_op},
			     {rule,pattern},{rule,pattern}];
erl(pattern,'record_index',4) -> [{lit,record_index},anno,atom,atom];
erl(pattern,'record',4) -> [{lit,record},anno,atom,{list,{rule,record_field}}];
erl(pattern,'map', 3) -> [{lit,map},anno, {list,{rule,map_elem_exact}}];

%% Here we can check if variable name is well formed
erl(var_pattern,Name, 0) ->
    {lit,Name};

erl(bin_element_pattern,'bin_element',5) ->
    [{lit,bin_element},anno,{rule,pattern},
     {part,bin_size},{part,bin_type}];

%% fixme: specialize guard?
erl(clause,'clause',5) ->
    [{lit,clause},anno,{list,{rule,pattern}},
     {list,{list,{rule,expr}}},{list,{rule,expr}}];

%% binary/list comprehension generators
erl(generator,'generate',4) ->
    [{lit,generate},anno,{rule,pattern},{rule,expr}];
erl(generator,'b_generate',4) ->
    [{lit,b_generate},anno,{rule,pattern},{rule,expr}];
    
erl(map_elem_assoc,'map_field_assoc',4) ->
    [{lit,map_field_assoc},anno,{rule,expr},{rule,expr}];

erl(map_elem_exact,'map_field_exact',4) ->
    [{lit,map_field_exact},anno,{rule,pattern},{rule,expr}];

erl(bin_size,default,0) -> {lit,default};
erl(bin_size,_Expr,_) -> {rule,expr};

erl(bin_type,default,0) -> {lit,default};
erl(bin_type,Type,_) -> {lit,Type};

erl(fun_expr,'function',3) -> [{lit,function},atom,integer];
erl(fun_expr,'function',4) -> [{lit,function},{rule,expr},
			   {rule,expr},{rule,expr}];
erl(fun_expr,'clauses',2) -> [{lit,clauses},{list,{rule,clause}}].


test_fun(X) ->
    B = 1,
    case X of
	(2+1) ->
	    foo;
	Z ->
	    X = foo + 1,
	    N = erlang:now(),
	    %% A+3 = B+2,
	    %%A
	    N + X + Z
    end.
