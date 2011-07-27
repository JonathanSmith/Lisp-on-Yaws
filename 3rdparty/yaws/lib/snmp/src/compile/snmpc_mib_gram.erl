-module(snmpc_mib_gram).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("snmpc_mib_gram.yrl", 729).
%%----------------------------------------------------------------------

-include("snmp_types.hrl").
-include("snmpc_lib.hrl").
-include("snmpc.hrl").

% value
val(Token) -> element(3, Token).

line_of(Token) -> element(2, Token).

%% category
cat(Token) -> element(1, Token). 

statusv1(Tok) ->
    case val(Tok) of
        mandatory -> mandatory;
        optional -> optional;
        obsolete -> obsolete;
        deprecated -> deprecated;
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

statusv2(Tok) ->
    case val(Tok) of
        current -> current;
        deprecated -> deprecated;
        obsolete -> obsolete;
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

ac_status(Tok) ->
    case val(Tok) of
        current -> current;
        obsolete -> obsolete;
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

accessv1(Tok) ->
    case val(Tok) of
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'write-only' -> 'write-only';
        'not-accessible' -> 'not-accessible';
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

accessv2(Tok) ->
    case val(Tok) of
        'not-accessible' -> 'not-accessible';
        'accessible-for-notify' -> 'accessible-for-notify';
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'read-create' -> 'read-create';
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

ac_access(Tok) ->
    case val(Tok) of
        'not-implemented' -> 'not-implemented'; % only for notifications
        'accessible-for-notify' -> 'accessible-for-notify';
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'read-create' -> 'read-create';
        'write-only' -> 'write-only'; % for backward-compatibility only
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

%% ---------------------------------------------------------------------
%% Various basic record build functions
%% ---------------------------------------------------------------------

make_module_identity(Name, LU, Org, CI, Desc, Revs, NA) ->
    #mc_module_identity{name         = Name,
                        last_updated = LU,
	                organization = Org,
	                contact_info = CI,
	                description  = Desc,
	                revisions    = Revs, 
	                name_assign  = NA}.

make_revision(Rev, Desc) ->
    #mc_revision{revision    = Rev,
	         description = Desc}.

make_object_type(Name, Syntax, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax,
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_object_type(Name, Syntax, Units, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax, 
                    units       = Units, 
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_new_type(Name, Macro, Syntax) ->
    #mc_new_type{name   = Name, 
	         macro  = Macro,
                 syntax = Syntax}.

make_new_type(Name, Macro, DisplayHint, Status, Desc, Ref, Syntax) ->
    #mc_new_type{name         = Name, 
	         macro        = Macro,
                 status       = Status,
                 description  = Desc,
                 reference    = Ref,
	         display_hint = DisplayHint,
                 syntax       = Syntax}.

make_trap(Name, Ent, Vars, Desc, Ref, Num) ->
    #mc_trap{name        = Name,
             enterprise  = Ent,
             vars        = Vars,
             description = Desc,
	     reference   = Ref,
	     num         = Num}.

make_notification(Name, Vars, Status, Desc, Ref, NA) ->
    #mc_notification{name        = Name,
                     vars        = Vars,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_agent_capabilities(Name, ProdRel, Status, Desc, Ref, Mods, NA) ->
    #mc_agent_capabilities{name            = Name,
                           product_release = ProdRel,
                           status          = Status,
                           description     = Desc,
	                   reference       = Ref,
                           modules         = Mods,
	                   name_assign     = NA}.

make_ac_variation(Name, 
		  undefined = _Syntax, 
		  undefined = _WriteSyntax, 
		  Access, 
		  undefined = _Creation, 
		  undefined = _DefVal, 
		  Desc) ->
%%     io:format("make_ac_variation -> entry with"
%% 	      "~n   Name:        ~p"
%% 	      "~n   Access:      ~p"
%% 	      "~n   Desc:        ~p"
%% 	      "~n", [Name, Access, Desc]),
    #mc_ac_notification_variation{name        = Name, 
 				  access      = Access,
 				  description = Desc};

make_ac_variation(Name, Syntax, WriteSyntax, Access, Creation, DefVal, Desc) ->
%%     io:format("make_ac_variation -> entry with"
%% 	      "~n   Name:        ~p"
%% 	      "~n   Syntax:      ~p"
%% 	      "~n   WriteSyntax: ~p"
%% 	      "~n   Access:      ~p"
%% 	      "~n   Creation:    ~p"
%% 	      "~n   DefVal:      ~p"
%% 	      "~n   Desc:        ~p"
%% 	      "~n", [Name, Syntax, WriteSyntax, Access, Creation, DefVal, Desc]),
    #mc_ac_object_variation{name          = Name, 
			    syntax        = Syntax, 
			    write_syntax  = WriteSyntax, 
			    access        = Access,
			    creation      = Creation,
			    default_value = DefVal,
			    description   = Desc}.

make_ac_module(Name, Grps, Var) ->
    #mc_ac_module{name      = Name, 
		  groups    = Grps,
		  variation = Var}.


make_module_compliance(Name, Status, Desc, Ref, Mods, NA) ->
    #mc_module_compliance{name        = Name,
                          status      = Status,
                          description = Desc,
	                  reference   = Ref,
                          modules     = Mods,
	                  name_assign = NA}.

make_mc_module(Name, Mand, Compl) ->
    #mc_mc_module{name       = Name, 
		  mandatory  = Mand,
		  compliance = Compl}.

make_mc_compliance_group(Name, Desc) ->
    #mc_mc_compliance_group{name        = Name,
			    description = Desc}.

make_mc_object(Name, Syntax, WriteSyntax, Access, Desc) ->
    #mc_mc_object{name         = Name,
		  syntax       = Syntax,
		  write_syntax = WriteSyntax,
		  access       = Access, 
		  description  = Desc}.

make_object_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_object_group{name        = Name,
                     objects     = Objs,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_notification_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_notification_group{name        = Name,
                           objects     = Objs,
                           status      = Status,
                           description = Desc,
	                   reference   = Ref,
	                   name_assign = NA}.

make_sequence(Name, Fields) ->
    #mc_sequence{name   = Name, 
                 fields = Fields}.

make_internal(Name, Macro, Parent, SubIdx) ->
    #mc_internal{name      = Name, 
                 macro     = Macro, 
                 parent    = Parent, 
                 sub_index = SubIdx}.



%% ---------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Purpose: Find how much room needs to be allocated for the data type
%%          (when sending it in a PDU (the maximum difference will be 
%%           the size allocated)).
%%          This is applicable for OCTET STRINGs and OBJECT IDENTIFIERs.
%%
%%     Or : Find the range of integers in the integer list.
%%          This is applicable for INTEGERs
%%
%% Arg: A list of integers.
%%----------------------------------------------------------------------

make_range_integer(RevHexStr, h) ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevHexStr, 'H') ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevBitStr, b) ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevBitStr, 'B') ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevStr, Base) ->
    throw({error, {invalid_base, Base, lists:reverse(RevStr)}}).

make_range(XIntList) ->
    IntList = lists:flatten(XIntList),
    {range, lists:min(IntList), lists:max(IntList)}.

make_defval_for_string(Line, Str, Atom) ->
    case lists:member(Atom, [h, 'H', b, 'B']) of
	true ->
	    case catch make_defval_for_string2(Str, Atom) of
		Defval when is_list(Defval) ->
		    Defval;
		{error, ErrStr} ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p - ~s",
						 [Atom, Str, ErrStr],
						 Line),
		    "";
		_Else ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p",
						 [Atom, Str],
						 Line),
		    ""
	    end;
	false ->
	    snmpc_lib:print_error("Bad DEFVAL string type ~w for ~p",
					 [Atom, Str],
					 Line),
	    ""
    end.
	    

make_defval_for_string2([], h) -> [];
make_defval_for_string2([X16,X|HexString], h) ->
    lists:append(hex_to_bytes(snmpc_misc:to_upper([X16,X])),
		 make_defval_for_string2(HexString, h));
make_defval_for_string2([_Odd], h) ->
    throw({error, "odd number of bytes in hex string"});
make_defval_for_string2(HexString, 'H') ->
    make_defval_for_string2(HexString,h);

make_defval_for_string2(BitString, 'B') ->
    bits_to_bytes(BitString);
make_defval_for_string2(BitString, b) ->
    make_defval_for_string2(BitString, 'B').

bits_to_bytes(BitStr) ->
    lists:reverse(bits_to_bytes(lists:reverse(BitStr), 1, 0)).

bits_to_bytes([], 1, _Byte) ->   % empty bitstring
    [];
bits_to_bytes([], 256, _Byte) -> % correct; multiple of 8
    [];
% If we are to support arbitrary length of bitstrings.  This migth
% be needed in the new SMI.
%bits_to_bytes([], N, Byte) ->
%    [Byte];
bits_to_bytes([], _N, _Byte) ->
    throw({error, "not a multiple of eight bits in bitstring"});
bits_to_bytes(Rest, 256, Byte) ->
    [Byte | bits_to_bytes(Rest, 1, 0)];
bits_to_bytes([$1 | T], N, Byte) ->
    bits_to_bytes(T, N*2, N + Byte);
bits_to_bytes([$0 | T], N, Byte) ->
    bits_to_bytes(T, N*2, Byte);
bits_to_bytes([_BadChar | _T], _N, _Byte) ->
    throw({error, "bad character in bit string"}).

%%----------------------------------------------------------------------
%% These HEX conversion routines are stolen from module asn1_bits by 
%% klacke@erix.ericsson.se
%% I didn't want to ship the entire asn1-compiler so I used cut-and-paste.
%%----------------------------------------------------------------------

%% hex_to_bytes(HexNumber) when is_atom(HexNumber) ->
%%     hex_to_bytes(atom_to_list(HexNumber));

hex_to_bytes(HexNumber) ->
    case length(HexNumber) rem 2 of
	1 ->  %% Odd
	    hex_to_bytes(lists:append(HexNumber,[$0]),[]);
	0 ->  %% even
	    hex_to_bytes(HexNumber,[])
    end.

hex_to_bytes([],R) ->
    lists:reverse(R);
hex_to_bytes([Hi,Lo|Rest],Res) ->
    hex_to_bytes(Rest,[hex_to_byte(Hi,Lo)|Res]).

hex_to_four_bits(Hex) ->
    if
	Hex == $0 -> 0;
	Hex == $1 -> 1;
	Hex == $2 -> 2;
	Hex == $3 -> 3;
	Hex == $4 -> 4;
	Hex == $5 -> 5;
	Hex == $6 -> 6;
	Hex == $7 -> 7;
	Hex == $8 -> 8;
	Hex == $9 -> 9;
	Hex == $A -> 10;
	Hex == $B -> 11;
	Hex == $C -> 12;
	Hex == $D -> 13;
	Hex == $E -> 14;
	Hex == $F -> 15;
	true -> throw({error, "bad hex character"})
    end.

hex_to_byte(Hi,Lo) ->
    (hex_to_four_bits(Hi) bsl 4) bor hex_to_four_bits(Lo).

kind(DefValPart,IndexPart) ->
    case DefValPart of
	undefined ->
	    case IndexPart of
		{indexes, undefined} -> {variable, []};
		{indexes, Indexes}  ->
		    {table_entry, {indexes, Indexes}};
		{augments,Table} ->
		    {table_entry,{augments,Table}}
	    end;
	{defval, DefVal} -> {variable, [{defval, DefVal}]}
    end.    

display_hint(Val) ->
    case val(Val) of
        Str when is_list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_display_hint, Val}})
    end.

units(Val) ->
    case val(Val) of
        Str when is_list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_units, Val}})
    end.

ensure_ver(Ver, Line, What) ->
    case get(snmp_version) of
	Ver -> ok;
	_Other ->
	    snmpc_lib:print_error(
	      "~s is only allowed in SNMPv~p.",[What,Ver],Line)
    end.


ensure_ver(Ver,Token) ->
    ensure_ver(Ver,line_of(Token), atom_to_list(cat(Token))).

filter_v2imports(2,'Integer32')  -> {builtin, 'Integer32'};
filter_v2imports(2,'Counter32')  -> {builtin, 'Counter32'};
filter_v2imports(2,'Gauge32')    -> {builtin, 'Gauge32'};
filter_v2imports(2,'Unsigned32') -> {builtin, 'Unsigned32'};
filter_v2imports(2,'Counter64')  -> {builtin, 'Counter64'};
filter_v2imports(_,Type)         -> {type, Type}.
    
w(F, A) ->
    ?vwarning(F, A).

lreverse(_Tag, L) when is_list(L) ->
    lists:reverse(L);
lreverse(Tag, X) ->
    exit({bad_list, Tag, X}).


%% i(F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).


-file("/net/isildur/ldisk/daily_build/r14b03_prebuild_opu_o.2011-05-24_13/otp_src_R14B03/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./snmpc_mib_gram.erl", 628).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_437(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(S, 'DEFINITIONS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_mibname(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_4(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, '::=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_5(S, 'BEGIN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_implies(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_implies(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(S, 'IMPORTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_11(11, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_12(S, 'AGENT-CAPABILITIES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'FROM', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(S, 'AGENT-CAPABILITIES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_imports(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_listofimports(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_import_stuff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_imports(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_53: see yeccpars2_12

yeccpars2_54(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_imports_from_one_mib(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_listofimports(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(S, 'END', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_4

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccpars2_113(113, Cat, [65 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_66(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_67(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_v1orv2(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_listofdefinitions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_69_MODULE-IDENTITY'(Stack),
 yeccgoto_mibid(hd(Ss), 'MODULE-IDENTITY', Ss, NewStack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_objectname(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_newtypename(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_listofdefinitions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_objectname(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_73(S, 'LAST-UPDATED', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_74(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_75(S, 'ORGANIZATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_last_updated(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(S, 'CONTACT-INFO', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_organization(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_80(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_81(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_contact_info(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2_84(84, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_84(S, 'REVISION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 yeccpars2_4(87, Cat, [84 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_descriptionfield(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_86(S, 'REVISION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_revisionpart(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_87: see yeccpars2_4

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_revisions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_89(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_90(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_revision_string(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_revision(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_revision_desc(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_moduleidentity(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_97(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_fatherobjectname(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_101(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_objectname(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_parentintegers(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_parentintegers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_105(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_106(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_107(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_parentintegers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_parentintegers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_nameassign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_nameassign(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_revisions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_v1orv2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(S, 'AGENT-CAPABILITIES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_123: see yeccpars2_4

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_listofdefinitionsv2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definitionv2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_128(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_128(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'BIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'OCTET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_128(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_129(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_syntax(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_syntax(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_newtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_133(S, 'STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_134(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_139(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(S, 'IDENTIFIER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_145(S, 'STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_150(S, 'OF', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(S, 'DISPLAY-HINT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccpars2_162(162, Cat, [154 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_usertype(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_163(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_displaypart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_166(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccpars2_168(168, Cat, [166 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_statusv2(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccpars2_171(171, Cat, [168 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_169(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_description(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_172(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_referpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_128(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 yeccgoto_textualconvention(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(S, 'OF', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_177(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_180(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_181(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_128(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_fieldname(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_fsyntax(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_fsyntax(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_186(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_187(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_188(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_189(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_190(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_namedbits(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_192(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_195(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_196(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_namedbits(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_198: see yeccpars2_179

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_tableentrydefinition(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_200: see yeccpars2_181

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_204: see yeccpars2_186

yeccpars2_205(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, 'SIZE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_210(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_211(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_sizedescr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_range_num(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_range_num(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_range_num(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_218(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_219(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_220: see yeccpars2_217

yeccpars2_221(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_sizedescr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_size(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_224: see yeccpars2_217

yeccpars2_225(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_sizedescr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_226(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_sizedescr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_size(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(S, 'PRODUCT-RELEASE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_230(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_231(S, 'NOTIFICATIONS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_232(S, 'OBJECTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccpars2_311(311, Cat, [232 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_233(S, 'IDENTIFIER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_234(S, 'OBJECTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_234_(Stack),
 yeccpars2_296(296, Cat, [234 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_235(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 290, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_236(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_237: see yeccpars2_174

yeccpars2_238(S, 'UNITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 yeccpars2_239(239, Cat, [238 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_239(S, 'MAX-ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_240(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_unitspart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_243(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_accessv2(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_245: see yeccpars2_165

yeccpars2_246(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_247(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_(Stack),
 yeccpars2_248(248, Cat, [247 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_248(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 yeccpars2_249(249, Cat, [248 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_249(S, 'AUGMENTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccpars2_250(250, Cat, [249 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_250(S, 'DEFVAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_250_(Stack),
 yeccpars2_4(267, Cat, [250 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_251(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_252(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_253(S, 'IMPLIED', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_index(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_indextypesv2(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_256(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indextypev2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_258(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_indextypev2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_260: see yeccpars2_253

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_indexpartv2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_262_(Stack),
 yeccgoto_indextypesv2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_263: see yeccpars2_258

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_entry(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_265(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_indexpartv2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_267: see yeccpars2_4

yeccpars2_268(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_269(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_270(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_270(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_271(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_272(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_273(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_274(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 yeccpars2_275(275, Cat, [274 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_275(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_276(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_defbitsvalue(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_277_(Stack),
 yeccgoto_defbitsnames(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_278(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_defbitsnames(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_284(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_287_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_defvalpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_289_(Stack),
 yeccgoto_objecttypev2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_290: see yeccpars2_165

yeccpars2_291(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_292(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_292(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_293(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_293_(Stack),
 yeccpars2_4(294, Cat, [293 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_294: see yeccpars2_4

yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_295_(Stack),
 yeccgoto_objectidentity(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_296(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_296(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_297(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_298: see yeccpars2_258

yeccpars2_299(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccgoto_objects(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_301: see yeccpars2_258

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_302_(Stack),
 yeccgoto_objectspart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_303_(Stack),
 yeccgoto_objects(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_304: see yeccpars2_165

yeccpars2_305(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_305_(Stack),
 yeccpars2_306(306, Cat, [305 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_306(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_306_(Stack),
 yeccpars2_4(307, Cat, [306 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_307: see yeccpars2_4

yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_308_(Stack),
 yeccgoto_objectgroup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_309: see yeccpars2_4

yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_310_(Stack),
 yeccgoto_objectidentifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_311(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_311(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_312: see yeccpars2_165

yeccpars2_313(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_314(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_314_(Stack),
 yeccpars2_315(315, Cat, [314 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_315(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_315_(Stack),
 yeccpars2_4(316, Cat, [315 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_4

yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_notification(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_318(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_318(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_319: see yeccpars2_258

yeccpars2_320(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_321(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_322: see yeccpars2_165

yeccpars2_323(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2_324(324, Cat, [323 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_324(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccpars2_4(325, Cat, [324 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_325: see yeccpars2_4

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_326_(Stack),
 yeccgoto_notificationgroup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_327: see yeccpars2_165

yeccpars2_328(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_328_(Stack),
 yeccpars2_329(329, Cat, [328 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_329(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_329_(Stack),
 yeccpars2_330(330, Cat, [329 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_330(S, 'MODULE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_330_(Stack),
 yeccpars2_4(332, Cat, [330 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_mc_modulepart(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_332: see yeccpars2_4

yeccpars2_333(S, 'MODULE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_333_(Stack),
 yeccgoto_mc_modules(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_334(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_334_(Stack),
 yeccpars2_336(336, Cat, [334 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mc_modulenamepart(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_336(S, 'MANDATORY-GROUPS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_336_(Stack),
 yeccpars2_337(337, Cat, [336 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_337(S, 'GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_337_(Stack),
 yeccpars2_344(_S, Cat, [337 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_338(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_339: see yeccpars2_258

yeccpars2_340(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 yeccgoto_mc_mandatorypart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mc_compliance(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_mc_compliancepart(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_344_(Stack),
 yeccgoto_mc_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mc_compliance(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_346(S, 'GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_346(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr);
yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_346_(Stack),
 yeccgoto_mc_compliances(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_347: see yeccpars2_258

%% yeccpars2_348: see yeccpars2_258

yeccpars2_349(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_349_(Stack),
 yeccpars2_350(350, Cat, [349 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_350(S, 'WRITE-SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 yeccpars2_353(353, Cat, [350 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_351: see yeccpars2_174

yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_352_(Stack),
 yeccgoto_syntaxpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_353(S, 'MIN-ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_353_(Stack),
 yeccpars2_356(356, Cat, [353 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_354: see yeccpars2_174

yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_writesyntaxpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_356(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 yeccpars2_359(_S, Cat, [356 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_357: see yeccpars2_242

yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_mc_accesspart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_mc_object(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_360(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_360_(Stack),
 yeccpars2_361(_S, Cat, [360 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_361_(Stack),
 yeccgoto_mc_compliancegroup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_mc_compliances(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_mc_modules(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_364_(Stack),
 yeccgoto_modulecompliance(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_365(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_366(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 368, Ss, Stack, T, Ts, Tzr);
yeccpars2_366(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_367(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_367_(Stack),
 yeccgoto_prodrel(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_368(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_369(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_369_(Stack),
 yeccpars2_371(371, Cat, [369 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 yeccgoto_ac_status(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_371(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 yeccpars2_372(372, Cat, [371 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_372(S, 'SUPPORTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccpars2_4(374, Cat, [372 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_373_(Stack),
 yeccgoto_ac_modulepart(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_374: see yeccpars2_4

yeccpars2_375(S, 'SUPPORTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_375_(Stack),
 yeccgoto_ac_modules(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_376(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_376_(Stack),
 yeccpars2_378(378, Cat, [376 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_ac_modulenamepart(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_378(S, 'INCLUDES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_379(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_379(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_380: see yeccpars2_258

yeccpars2_381(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 382, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_382(S, 'VARIATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_382_(Stack),
 yeccpars2_384(_S, Cat, [382 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_ac_variationpart(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_384_(Stack),
 yeccgoto_ac_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_385(S, 'VARIATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_(Stack),
 yeccgoto_ac_variations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_386: see yeccpars2_258

yeccpars2_387(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccpars2_388(388, Cat, [387 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_388(S, 'WRITE-SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_388_(Stack),
 yeccpars2_389(389, Cat, [388 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_389(S, 'ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 yeccpars2_390(390, Cat, [389 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_390(S, 'CREATION-REQUIRES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 395, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_390_(Stack),
 yeccpars2_394(394, Cat, [390 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_391(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_392_(Stack),
 yeccgoto_ac_accesspart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_393_(Stack),
 yeccgoto_ac_access(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_394(S, 'DEFVAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 yeccpars2_399(399, Cat, [394 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_395(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_396: see yeccpars2_258

yeccpars2_397(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_397(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 398, Ss, Stack, T, Ts, Tzr);
yeccpars2_397(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_ac_creationpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_399_(Stack),
 yeccpars2_400(_S, Cat, [399 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_ac_variation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_401_(Stack),
 yeccgoto_ac_variations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_402_(Stack),
 yeccgoto_ac_modules(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_403_(Stack),
 yeccgoto_agentcapabilities(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_404(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_128(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_405(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_406(S, 'ENTERPRISE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_407: see yeccpars2_258

yeccpars2_408(S, 'VARIABLES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_408_(Stack),
 yeccpars2_409(409, Cat, [408 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_409(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_409_(Stack),
 yeccpars2_417(417, Cat, [409 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_410(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 411, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_411: see yeccpars2_258

yeccpars2_412(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr);
yeccpars2_412(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_412(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_variables(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_414: see yeccpars2_258

yeccpars2_415(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_415_(Stack),
 yeccgoto_varpart(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_416_(Stack),
 yeccgoto_variables(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_417(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_417_(Stack),
 yeccpars2_4(418, Cat, [417 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_418: see yeccpars2_4

yeccpars2_419(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 420, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_420_(Stack),
 yeccgoto_traptype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_421: see yeccpars2_174

yeccpars2_422(S, 'ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 423, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_423(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 425, Ss, Stack, T, Ts, Tzr);
yeccpars2_423(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_424(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 426, Ss, Stack, T, Ts, Tzr);
yeccpars2_424(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_425_(Stack),
 yeccgoto_accessv1(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_426(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 428, Ss, Stack, T, Ts, Tzr);
yeccpars2_426(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_427(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 429, Ss, Stack, T, Ts, Tzr);
yeccpars2_427(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_428_(Stack),
 yeccgoto_statusv1(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_429(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 yeccpars2_430(430, Cat, [429 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_430(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_430_(Stack),
 yeccpars2_431(431, Cat, [430 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_431(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 433, Ss, Stack, T, Ts, Tzr);
yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_431_(Stack),
 yeccpars2_432(432, Cat, [431 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_432(S, 'DEFVAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_432_(Stack),
 yeccpars2_4(441, Cat, [432 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_433(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_433(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_434: see yeccpars2_258

yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_435_(Stack),
 yeccgoto_indextypesv1(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_436(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indextypev1(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_438: see yeccpars2_258

yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_439_(Stack),
 yeccgoto_indexpartv1(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_440_(Stack),
 yeccgoto_indextypesv1(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_441: see yeccpars2_4

yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_442_(Stack),
 yeccgoto_objecttypev1(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_443_(Stack),
 yeccgoto_mib(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_ac_access(391=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_accesspart(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_creationpart(390, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(394, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_module(372, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(375, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ac_module(375, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(375, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_modulenamepart(376, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(378, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_modulepart(372, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(374, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_modules(372=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ac_modules(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_status(368, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(369, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_variation(382, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(385, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ac_variation(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(385, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_variationpart(382=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ac_variations(382=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ac_variations(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_accessv1(423, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(424, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_accessv2(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_accessv2(357=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_agentcapabilities(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contact_info(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_defbitsnames(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(276, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_defbitsvalue(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_definition(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_definition(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_definitionv2(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_defvalpart(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(267, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_defvalpart(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(399, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_defvalpart(432, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(441, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_description(166, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(168, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(305, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(306, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(324, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(328, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(369, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(371, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(399=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_description(409, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(417, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_descriptionfield(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_descriptionfield(247, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(248, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_descriptionfield(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(315, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_descriptionfield(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(430, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_displaypart(154, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_entry(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fatherobjectname(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fieldname(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldname(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(200, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fields(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fsyntax(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fsyntax(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_implies(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(404, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(267, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(294, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(418, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(419, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implies(441, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_import(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_import_stuff(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_stuff(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_stuff(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_imports(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_imports(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_imports_from_one_mib(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_imports_from_one_mib(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_index(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index(438=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indexpartv1(431, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(432, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indexpartv2(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(250, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indextypesv1(434, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(436, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indextypesv2(253, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(256, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indextypev1(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indextypev1(438=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indextypev2(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indextypev2(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_last_updated(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(75, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_listofdefinitions(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_listofdefinitionsv2(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_listofimports(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_listofimports(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_accesspart(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(356, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_compliance(337, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(346, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_compliance(346, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(346, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_compliancegroup(337=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_compliancegroup(346=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_compliancepart(337=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_compliances(337=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_compliances(346=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_mandatorypart(336, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(337, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_module(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(333, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_module(333, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(333, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_modulenamepart(334, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(336, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_modulepart(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(332, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_modules(330=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_modules(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mc_object(337=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mc_object(346=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mib(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mibid(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mibname(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mibname(334=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mibname(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modulecompliance(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_moduleidentity(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_nameassign(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(267=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(294=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(307=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(309=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(325=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_nameassign(441=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_namedbits(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(187, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_namedbits(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_newtype(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtype(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_newtypename(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtypename(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_newtypename(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(123, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notification(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationgroup(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objectgroup(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objectidentifier(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectidentifier(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectidentifier(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objectidentity(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objectname(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(339=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(347, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(360, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(348, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(386, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(407, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(408, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(411=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(414=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectname(438=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objects(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objects(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(320, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objects(339, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(340, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objects(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(381, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objects(396, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(397, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objectspart(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objectspart(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(296, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objecttypev1(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_objecttypev1(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_objecttypev2(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_organization(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parentintegers(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(98, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parentintegers(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parentintegers(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parentintegers(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_prodrel(365, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(366, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_range_num(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_range_num(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_range_num(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_range_num(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_range_num(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_referpart(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(171, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(249, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(294, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(306, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(307, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(315, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(316, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(324, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(325, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(329, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(330, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(371, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(372, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(417, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(418, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_referpart(430, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(431, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_revision(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_revision(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_revision_desc(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_revision_string(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_revisionpart(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(87, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_revisions(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_size(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_size(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sizedescr(209, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(210, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sizedescr(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sizedescr(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sizedescr(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statusv1(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(427, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statusv2(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(166, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(245, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(246, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(290, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(291, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(305, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(312, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(322, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(323, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statusv2(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(328, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_syntax(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(200=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(404=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntax(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(422, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_syntaxpart(349, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(350, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_syntaxpart(387, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(388, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_tableentrydefinition(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tableentrydefinition(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tableentrydefinition(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_textualconvention(113=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_traptype(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_traptype(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(351, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(404, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unitspart(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_usertype(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(200, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(351, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(404, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_usertype(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_v1orv2(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_variables(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(412, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_varpart(408, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(409, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_writesyntaxpart(350, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(353, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_writesyntaxpart(388, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(389, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_3_/1}).
-file("snmpc_mib_gram.yrl", 475).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("snmpc_mib_gram.yrl", 427).
yeccpars2_9_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   w ( "Sloppy asignment on line ~p" , [ line_of ( __1 ) ] ) , __1
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("snmpc_mib_gram.yrl", 242).
yeccpars2_10_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_14_/1}).
-file("snmpc_mib_gram.yrl", 245).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("snmpc_mib_gram.yrl", 251).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("snmpc_mib_gram.yrl", 270).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'AGENT-CAPABILITIES' }
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("snmpc_mib_gram.yrl", 292).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'AutonomousType' }
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("snmpc_mib_gram.yrl", 259).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'Counter' }
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("snmpc_mib_gram.yrl", 306).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DateAndTime' }
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("snmpc_mib_gram.yrl", 282).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DisplayString' }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("snmpc_mib_gram.yrl", 260).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'Gauge' }
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("snmpc_mib_gram.yrl", 294).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'InstancePointer' }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("snmpc_mib_gram.yrl", 258).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'IpAddress' }
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("snmpc_mib_gram.yrl", 272).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-COMPLIANCE' }
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("snmpc_mib_gram.yrl", 266).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-IDENTITY' }
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("snmpc_mib_gram.yrl", 286).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MacAddress' }
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("snmpc_mib_gram.yrl", 274).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-GROUP' }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("snmpc_mib_gram.yrl", 268).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-TYPE' }
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("snmpc_mib_gram.yrl", 256).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'NetworkAddress' }
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("snmpc_mib_gram.yrl", 276).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-GROUP' }
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("snmpc_mib_gram.yrl", 278).
yeccpars2_32_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-IDENTITY' }
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("snmpc_mib_gram.yrl", 254).
yeccpars2_33_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'OBJECT-TYPE' }
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("snmpc_mib_gram.yrl", 261).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'Opaque' }
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("snmpc_mib_gram.yrl", 284).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'PhysAddress' }
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("snmpc_mib_gram.yrl", 298).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowPointer' }
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("snmpc_mib_gram.yrl", 300).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowStatus' }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("snmpc_mib_gram.yrl", 308).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'StorageType' }
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("snmpc_mib_gram.yrl", 312).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TAddress' }
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("snmpc_mib_gram.yrl", 310).
yeccpars2_40_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TDomain' }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("snmpc_mib_gram.yrl", 280).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TEXTUAL-CONVENTION' }
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("snmpc_mib_gram.yrl", 255).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'TRAP-TYPE' }
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("snmpc_mib_gram.yrl", 290).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TestAndIncr' }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("snmpc_mib_gram.yrl", 304).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeInterval' }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("snmpc_mib_gram.yrl", 302).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeStamp' }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("snmpc_mib_gram.yrl", 257).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { builtin , 'TimeTicks' }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("snmpc_mib_gram.yrl", 288).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TruthValue' }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("snmpc_mib_gram.yrl", 296).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'VariablePointer' }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("snmpc_mib_gram.yrl", 263).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { node , val ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("snmpc_mib_gram.yrl", 262).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   filter_v2imports ( get ( snmp_version ) , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("snmpc_mib_gram.yrl", 243).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("snmpc_mib_gram.yrl", 246).
yeccpars2_52_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("snmpc_mib_gram.yrl", 249).
yeccpars2_55_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __3 ) , lreverse ( imports_from_one_mib , __1 ) } , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("snmpc_mib_gram.yrl", 252).
yeccpars2_56_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("snmpc_mib_gram.yrl", 529).
yeccpars2_65_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_67_/1}).
-file("snmpc_mib_gram.yrl", 231).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { v1_mib , lreverse ( v1orv2_list , __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("snmpc_mib_gram.yrl", 239).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,'yeccpars2_69_MODULE-IDENTITY'/1}).
-file("snmpc_mib_gram.yrl", 501).
'yeccpars2_69_MODULE-IDENTITY'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("snmpc_mib_gram.yrl", 474).
yeccpars2_69_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("snmpc_mib_gram.yrl", 477).
yeccpars2_70_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("snmpc_mib_gram.yrl", 240).
yeccpars2_71_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("snmpc_mib_gram.yrl", 474).
yeccpars2_72_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("snmpc_mib_gram.yrl", 502).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( last_updated , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("snmpc_mib_gram.yrl", 503).
yeccpars2_79_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( organization , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("snmpc_mib_gram.yrl", 504).
yeccpars2_82_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( contact_info , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_83_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_84_/1}).
-file("snmpc_mib_gram.yrl", 506).
yeccpars2_84_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_85_/1}).
-file("snmpc_mib_gram.yrl", 428).
yeccpars2_85_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( descriptionfield , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("snmpc_mib_gram.yrl", 507).
yeccpars2_86_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( revisionpart , __1 )
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("snmpc_mib_gram.yrl", 509).
yeccpars2_88_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("snmpc_mib_gram.yrl", 514).
yeccpars2_91_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( revision_string , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("snmpc_mib_gram.yrl", 512).
yeccpars2_93_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_revision ( __2 , __4 )
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("snmpc_mib_gram.yrl", 515).
yeccpars2_94_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( revision_desc , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("snmpc_mib_gram.yrl", 497).
yeccpars2_95_(__Stack0) ->
 [__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   MI = make_module_identity ( __1 , __4 , __6 , __8 ,
    __10 , __11 , __12 ) ,
    { MI , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("snmpc_mib_gram.yrl", 474).
yeccpars2_101_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("snmpc_mib_gram.yrl", 448).
yeccpars2_102_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ val ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("snmpc_mib_gram.yrl", 450).
yeccpars2_103_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ val ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("snmpc_mib_gram.yrl", 449).
yeccpars2_107_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ val ( __3 ) ]
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("snmpc_mib_gram.yrl", 451).
yeccpars2_108_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ val ( __3 ) | __5 ]
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("snmpc_mib_gram.yrl", 417).
yeccpars2_110_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __3 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("snmpc_mib_gram.yrl", 418).
yeccpars2_111_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { root , __3 }
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("snmpc_mib_gram.yrl", 510).
yeccpars2_112_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("snmpc_mib_gram.yrl", 230).
yeccpars2_113_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { v2_mib , [ __1 | lreverse ( v1orv2_mod , __2 ) ] }
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("snmpc_mib_gram.yrl", 530).
yeccpars2_126_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("snmpc_mib_gram.yrl", 358).
yeccpars2_129_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { type , val ( __1 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("snmpc_mib_gram.yrl", 359).
yeccpars2_130_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { type , cat ( __1 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("snmpc_mib_gram.yrl", 340).
yeccpars2_131_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   NT = make_new_type ( __1 , dummy , __3 ) ,
    { NT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("snmpc_mib_gram.yrl", 404).
yeccpars2_132_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("snmpc_mib_gram.yrl", 395).
yeccpars2_135_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("snmpc_mib_gram.yrl", 411).
yeccpars2_136_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("snmpc_mib_gram.yrl", 399).
yeccpars2_137_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("snmpc_mib_gram.yrl", 396).
yeccpars2_138_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-file("snmpc_mib_gram.yrl", 405).
yeccpars2_140_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("snmpc_mib_gram.yrl", 401).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-file("snmpc_mib_gram.yrl", 400).
yeccpars2_147_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-file("snmpc_mib_gram.yrl", 407).
yeccpars2_148_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("snmpc_mib_gram.yrl", 408).
yeccpars2_149_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_151_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("snmpc_mib_gram.yrl", 414).
yeccpars2_152_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("snmpc_mib_gram.yrl", 413).
yeccpars2_153_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("snmpc_mib_gram.yrl", 434).
yeccpars2_154_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_155_/1}).
-file("snmpc_mib_gram.yrl", 403).
yeccpars2_155_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-file("snmpc_mib_gram.yrl", 410).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_157_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-file("snmpc_mib_gram.yrl", 402).
yeccpars2_159_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("snmpc_mib_gram.yrl", 406).
yeccpars2_160_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("snmpc_mib_gram.yrl", 433).
yeccpars2_164_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   display_hint ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_166_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_167_/1}).
-file("snmpc_mib_gram.yrl", 708).
yeccpars2_167_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statusv2 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_168_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_170_/1}).
-file("snmpc_mib_gram.yrl", 430).
yeccpars2_170_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   lreverse ( description , val ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-file("snmpc_mib_gram.yrl", 483).
yeccpars2_173_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   lreverse ( referpart , val ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_175_/1}).
-file("snmpc_mib_gram.yrl", 534).
yeccpars2_175_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   NT = make_new_type ( __1 , 'TEXTUAL-CONVENTION' , __4 ,
    __6 , __7 , __8 , __10 ) ,
    { NT , line_of ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("snmpc_mib_gram.yrl", 368).
yeccpars2_178_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { sequence_of , val ( __3 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("snmpc_mib_gram.yrl", 349).
yeccpars2_184_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { val ( __1 ) , __2 } ]
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("snmpc_mib_gram.yrl", 353).
yeccpars2_185_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { bits , [ { dummy , 0 } ] } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_191_/1}).
-file("snmpc_mib_gram.yrl", 383).
yeccpars2_191_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { val ( __1 ) , val ( __3 ) } ]
  end | __Stack].

-compile({inline,yeccpars2_193_/1}).
-file("snmpc_mib_gram.yrl", 365).
yeccpars2_193_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_ver ( 2 , __1 ) ,
    { { bits , __3 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-file("snmpc_mib_gram.yrl", 385).
yeccpars2_197_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { val ( __3 ) , val ( __5 ) } | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-file("snmpc_mib_gram.yrl", 344).
yeccpars2_199_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Seq = make_sequence ( __1 , lreverse ( tableentrydefinition , __5 ) ) ,
    { Seq , line_of ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("snmpc_mib_gram.yrl", 351).
yeccpars2_201_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { val ( __3 ) , __4 } | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-file("snmpc_mib_gram.yrl", 389).
yeccpars2_202_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'OCTET STRING' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("snmpc_mib_gram.yrl", 391).
yeccpars2_203_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'OBJECT IDENTIFIER' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("snmpc_mib_gram.yrl", 363).
yeccpars2_206_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { integer_with_enum , 'INTEGER' , __3 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-file("snmpc_mib_gram.yrl", 390).
yeccpars2_207_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'BIT STRING' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("snmpc_mib_gram.yrl", 360).
yeccpars2_208_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { { type_with_size , cat ( __1 ) , __2 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-file("snmpc_mib_gram.yrl", 376).
yeccpars2_211_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-file("snmpc_mib_gram.yrl", 379).
yeccpars2_213_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_215_/1}).
-file("snmpc_mib_gram.yrl", 380).
yeccpars2_215_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-file("snmpc_mib_gram.yrl", 381).
yeccpars2_216_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("snmpc_mib_gram.yrl", 377).
yeccpars2_221_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("snmpc_mib_gram.yrl", 371).
yeccpars2_222_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_range ( __4 )
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-file("snmpc_mib_gram.yrl", 374).
yeccpars2_225_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __4 ]
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-file("snmpc_mib_gram.yrl", 375).
yeccpars2_226_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __4 | __5 ]
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("snmpc_mib_gram.yrl", 370).
yeccpars2_227_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_range ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("snmpc_mib_gram.yrl", 361).
yeccpars2_228_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { { type_with_size , val ( __1 ) , __2 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-file("snmpc_mib_gram.yrl", 719).
yeccpars2_232_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_234_/1}).
-file("snmpc_mib_gram.yrl", 719).
yeccpars2_234_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_238_/1}).
-file("snmpc_mib_gram.yrl", 705).
yeccpars2_238_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_241_/1}).
-file("snmpc_mib_gram.yrl", 706).
yeccpars2_241_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   units ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-file("snmpc_mib_gram.yrl", 710).
yeccpars2_244_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   accessv2 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_247_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_248_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_248_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_249_/1}).
-file("snmpc_mib_gram.yrl", 695).
yeccpars2_249_(__Stack0) ->
 [begin
   { indexes , undefined }
  end | __Stack0].

-compile({inline,yeccpars2_250_/1}).
-file("snmpc_mib_gram.yrl", 466).
yeccpars2_250_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_255_/1}).
-file("snmpc_mib_gram.yrl", 697).
yeccpars2_255_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-file("snmpc_mib_gram.yrl", 700).
yeccpars2_259_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { implied , __2 }
  end | __Stack].

-compile({inline,yeccpars2_261_/1}).
-file("snmpc_mib_gram.yrl", 693).
yeccpars2_261_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { indexes , lreverse ( indexpartv2 , __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_262_/1}).
-file("snmpc_mib_gram.yrl", 698).
yeccpars2_262_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_266_/1}).
-file("snmpc_mib_gram.yrl", 694).
yeccpars2_266_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { augments , __3 }
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-file("snmpc_mib_gram.yrl", 469).
yeccpars2_274_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_277_/1}).
-file("snmpc_mib_gram.yrl", 471).
yeccpars2_277_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ val ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-file("snmpc_mib_gram.yrl", 472).
yeccpars2_279_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ val ( __3 ) | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_281_/1}).
-file("snmpc_mib_gram.yrl", 455).
yeccpars2_281_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , __4 }
  end | __Stack].

-compile({inline,yeccpars2_282_/1}).
-file("snmpc_mib_gram.yrl", 465).
yeccpars2_282_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , lreverse ( defvalpart_string , val ( __3 ) ) }
  end | __Stack].

-compile({inline,yeccpars2_285_/1}).
-file("snmpc_mib_gram.yrl", 461).
yeccpars2_285_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) ,
    lreverse ( defvalpart_quote_variable , val ( __3 ) ) ,
    val ( __4 ) ) }
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-file("snmpc_mib_gram.yrl", 457).
yeccpars2_286_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) ,
    lreverse ( defvalpart_quote_atom , val ( __3 ) ) ,
    val ( __4 ) ) }
  end | __Stack].

-compile({inline,yeccpars2_287_/1}).
-file("snmpc_mib_gram.yrl", 453).
yeccpars2_287_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , val ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-file("snmpc_mib_gram.yrl", 454).
yeccpars2_288_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { defval , val ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_289_/1}).
-file("snmpc_mib_gram.yrl", 688).
yeccpars2_289_(__Stack0) ->
 [__15,__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Kind = kind ( __14 , __13 ) ,
    OT = make_object_type ( __1 , __4 , __5 , __7 , __9 ,
    __11 , __12 , Kind , __15 ) ,
    { OT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_293_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_293_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_295_/1}).
-file("snmpc_mib_gram.yrl", 540).
yeccpars2_295_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Parent , SubIndex } = __8 ,
    Int = make_internal ( __1 , 'OBJECT-IDENTITY' ,
    Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_300_/1}).
-file("snmpc_mib_gram.yrl", 721).
yeccpars2_300_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_302_/1}).
-file("snmpc_mib_gram.yrl", 718).
yeccpars2_302_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lreverse ( objectspart , __3 )
  end | __Stack].

-compile({inline,yeccpars2_303_/1}).
-file("snmpc_mib_gram.yrl", 722).
yeccpars2_303_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_305_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_305_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_306_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_306_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_308_/1}).
-file("snmpc_mib_gram.yrl", 547).
yeccpars2_308_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   OG = make_object_group ( __1 , __3 , __5 , __6 , __7 , __8 ) ,
    { OG , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_310_/1}).
-file("snmpc_mib_gram.yrl", 322).
yeccpars2_310_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Parent , SubIndex } = __4 ,
    Int = make_internal ( __1 , dummy , Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_314_/1}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_314_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_315_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_315_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_317_/1}).
-file("snmpc_mib_gram.yrl", 715).
yeccpars2_317_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Not = make_notification ( __1 , __3 , __5 , __7 , __8 , __9 ) ,
    { Not , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_323_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_323_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_324_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_324_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_326_/1}).
-file("snmpc_mib_gram.yrl", 553).
yeccpars2_326_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   NG = make_notification_group ( __1 , __5 , __8 , __9 ,
    __10 , __11 ) ,
    { NG , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_328_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_328_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_329_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_329_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_330_/1}).
-file("snmpc_mib_gram.yrl", 630).
yeccpars2_330_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_331_/1}).
-file("snmpc_mib_gram.yrl", 632).
yeccpars2_331_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( mc_modulepart , __1 )
  end | __Stack].

-compile({inline,yeccpars2_333_/1}).
-file("snmpc_mib_gram.yrl", 635).
yeccpars2_333_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_334_/1}).
-file("snmpc_mib_gram.yrl", 643).
yeccpars2_334_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_336_/1}).
-file("snmpc_mib_gram.yrl", 648).
yeccpars2_336_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_337_/1}).
-file("snmpc_mib_gram.yrl", 653).
yeccpars2_337_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_341_/1}).
-file("snmpc_mib_gram.yrl", 646).
yeccpars2_341_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lreverse ( mc_mandatorypart , __3 )
  end | __Stack].

-compile({inline,yeccpars2_343_/1}).
-file("snmpc_mib_gram.yrl", 651).
yeccpars2_343_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( mc_compliancepart , __1 )
  end | __Stack].

-compile({inline,yeccpars2_344_/1}).
-file("snmpc_mib_gram.yrl", 640).
yeccpars2_344_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_mc_module ( __2 , __3 , __4 )
  end | __Stack].

-compile({inline,yeccpars2_346_/1}).
-file("snmpc_mib_gram.yrl", 656).
yeccpars2_346_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_349_/1}).
-file("snmpc_mib_gram.yrl", 672).
yeccpars2_349_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_350_/1}).
-file("snmpc_mib_gram.yrl", 675).
yeccpars2_350_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_352_/1}).
-file("snmpc_mib_gram.yrl", 671).
yeccpars2_352_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_353_/1}).
-file("snmpc_mib_gram.yrl", 678).
yeccpars2_353_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_355_/1}).
-file("snmpc_mib_gram.yrl", 674).
yeccpars2_355_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_356_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_356_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_358_/1}).
-file("snmpc_mib_gram.yrl", 677).
yeccpars2_358_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_359_/1}).
-file("snmpc_mib_gram.yrl", 669).
yeccpars2_359_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_mc_object ( __2 , __3 , __4 , __5 , __6 )
  end | __Stack].

-compile({inline,yeccpars2_360_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_360_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_361_/1}).
-file("snmpc_mib_gram.yrl", 666).
yeccpars2_361_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_mc_compliance_group ( __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_362_/1}).
-file("snmpc_mib_gram.yrl", 658).
yeccpars2_362_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_363_/1}).
-file("snmpc_mib_gram.yrl", 637).
yeccpars2_363_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_364_/1}).
-file("snmpc_mib_gram.yrl", 567).
yeccpars2_364_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   MC = make_module_compliance ( __1 , __4 , __5 , __6 ,
    __7 , __8 ) ,
   
   
   
    { MC , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_367_/1}).
-file("snmpc_mib_gram.yrl", 583).
yeccpars2_367_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( prodrel , val ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_369_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_369_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_370_/1}).
-file("snmpc_mib_gram.yrl", 585).
yeccpars2_370_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ac_status ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_371_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_371_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_372_/1}).
-file("snmpc_mib_gram.yrl", 590).
yeccpars2_372_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_373_/1}).
-file("snmpc_mib_gram.yrl", 588).
yeccpars2_373_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( ac_modulepart , __1 )
  end | __Stack].

-compile({inline,yeccpars2_375_/1}).
-file("snmpc_mib_gram.yrl", 593).
yeccpars2_375_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_376_/1}).
-file("snmpc_mib_gram.yrl", 601).
yeccpars2_376_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_382_/1}).
-file("snmpc_mib_gram.yrl", 604).
yeccpars2_382_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_383_/1}).
-file("snmpc_mib_gram.yrl", 606).
yeccpars2_383_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lreverse ( ac_variationpart , __1 )
  end | __Stack].

-compile({inline,yeccpars2_384_/1}).
-file("snmpc_mib_gram.yrl", 598).
yeccpars2_384_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_ac_module ( __2 , __5 , __7 )
  end | __Stack].

-compile({inline,yeccpars2_385_/1}).
-file("snmpc_mib_gram.yrl", 609).
yeccpars2_385_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_387_/1}).
-file("snmpc_mib_gram.yrl", 672).
yeccpars2_387_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_388_/1}).
-file("snmpc_mib_gram.yrl", 675).
yeccpars2_388_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_389_/1}).
-file("snmpc_mib_gram.yrl", 620).
yeccpars2_389_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_390_/1}).
-file("snmpc_mib_gram.yrl", 627).
yeccpars2_390_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_392_/1}).
-file("snmpc_mib_gram.yrl", 619).
yeccpars2_392_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_393_/1}).
-file("snmpc_mib_gram.yrl", 622).
yeccpars2_393_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ac_access ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_394_/1}).
-file("snmpc_mib_gram.yrl", 466).
yeccpars2_394_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_398_/1}).
-file("snmpc_mib_gram.yrl", 625).
yeccpars2_398_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lreverse ( ac_creationpart , __3 )
  end | __Stack].

-compile({inline,yeccpars2_399_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_399_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_400_/1}).
-file("snmpc_mib_gram.yrl", 617).
yeccpars2_400_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_ac_variation ( __2 , __3 , __4 , __5 , __6 , __7 , __8 )
  end | __Stack].

-compile({inline,yeccpars2_401_/1}).
-file("snmpc_mib_gram.yrl", 611).
yeccpars2_401_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_402_/1}).
-file("snmpc_mib_gram.yrl", 595).
yeccpars2_402_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_403_/1}).
-file("snmpc_mib_gram.yrl", 579).
yeccpars2_403_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   AC = make_agent_capabilities ( __1 , __4 , __6 , __7 ,
    __8 , __9 , __10 ) ,
    { AC , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_408_/1}).
-file("snmpc_mib_gram.yrl", 421).
yeccpars2_408_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_409_/1}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_409_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_413_/1}).
-file("snmpc_mib_gram.yrl", 423).
yeccpars2_413_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_415_/1}).
-file("snmpc_mib_gram.yrl", 422).
yeccpars2_415_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_416_/1}).
-file("snmpc_mib_gram.yrl", 424).
yeccpars2_416_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_417_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_417_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_420_/1}).
-file("snmpc_mib_gram.yrl", 316).
yeccpars2_420_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Trap = make_trap ( __1 , __4 , lreverse ( traptype , __5 ) ,
    __6 , __7 , val ( __9 ) ) ,
    { Trap , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_425_/1}).
-file("snmpc_mib_gram.yrl", 479).
yeccpars2_425_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   accessv1 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_428_/1}).
-file("snmpc_mib_gram.yrl", 481).
yeccpars2_428_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statusv1 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_429_/1}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_429_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_430_/1}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_430_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_431_/1}).
-file("snmpc_mib_gram.yrl", 439).
yeccpars2_431_(__Stack0) ->
 [begin
   { indexes , undefined }
  end | __Stack0].

-compile({inline,yeccpars2_432_/1}).
-file("snmpc_mib_gram.yrl", 466).
yeccpars2_432_(__Stack0) ->
 [begin
   undefined
  end | __Stack0].

-compile({inline,yeccpars2_435_/1}).
-file("snmpc_mib_gram.yrl", 441).
yeccpars2_435_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_439_/1}).
-file("snmpc_mib_gram.yrl", 438).
yeccpars2_439_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { indexes , lreverse ( indexpartv1 , __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_440_/1}).
-file("snmpc_mib_gram.yrl", 442).
yeccpars2_440_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,yeccpars2_442_/1}).
-file("snmpc_mib_gram.yrl", 334).
yeccpars2_442_(__Stack0) ->
 [__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Kind = kind ( __13 , __12 ) ,
    OT = make_object_type ( __1 , __4 , __6 , __8 , __10 ,
    __11 , Kind , __14 ) ,
    { OT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_443_/1}).
-file("snmpc_mib_gram.yrl", 223).
yeccpars2_443_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Version , Defs } = __6 ,
    # pdata { mib_version = Version ,
    mib_name = __1 ,
    imports = __5 ,
    defs = Defs }
  end | __Stack].


-file("snmpc_mib_gram.yrl", 1170).
