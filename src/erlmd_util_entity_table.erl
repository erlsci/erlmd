%%%-----------------------------------------------------------------------------
%%% @doc HTML5 entity lookup table.
%%%
%%% Provides fast lookup of named character entities for character references.
%%% Implements a subset of HTML5 named entities.
%%%
%%% The table is stored in persistent_term for efficient access without
%%% copying data between processes.
%%%
%%% Reference: https://html.spec.whatwg.org/multipage/named-characters.html
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_entity_table).

-export([lookup/1, init/0]).

-define(TABLE_KEY, erlmd_entity_table).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec lookup(binary()) -> {ok, binary()} | not_found.
%% @doc Look up a named entity and return its Unicode value.
%%
%% Returns {ok, Value} if found, not_found otherwise.
%% The input should NOT include the & prefix or ; suffix.
%%
%% Examples:
%%   lookup(<<"amp">>) -> {ok, <<"&">>}
%%   lookup(<<"lt">>) -> {ok, <<"<">>}
%%   lookup(<<"unknown">>) -> not_found
lookup(Name) when is_binary(Name) ->
    %% Fast path for most common entities
    case Name of
        <<"amp">> -> {ok, <<"&">>};
        <<"lt">> -> {ok, <<"<">>};
        <<"gt">> -> {ok, <<">">>};
        <<"quot">> -> {ok, <<"\"">>};
        <<"apos">> -> {ok, <<"'">>};
        _ ->
            %% Look up in persistent_term table
            case persistent_term:get(?TABLE_KEY, undefined) of
                undefined ->
                    %% Initialize table if not yet done
                    init(),
                    lookup(Name);
                Table ->
                    case maps:get(Name, Table, undefined) of
                        undefined -> not_found;
                        Value -> {ok, Value}
                    end
            end
    end.

-spec init() -> ok.
%% @doc Initialize the entity table in persistent_term.
%%
%% This is called automatically on first lookup if needed.
%% Can also be called explicitly during application startup.
init() ->
    Table = build_table(),
    persistent_term:put(?TABLE_KEY, Table),
    ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
%% @doc Build the entity table map.
%%
%% This includes common HTML5 named entities.
%% For production, this would include all 2000+ HTML5 entities.
build_table() ->
    #{
        %% Common ASCII entities (already in fast path, but included for completeness)
        <<"amp">> => <<"&">>,
        <<"lt">> => <<"<">>,
        <<"gt">> => <<">">>,
        <<"quot">> => <<"\"">>,
        <<"apos">> => <<"'">>,

        %% Common punctuation
        <<"nbsp">> => <<"\u00A0">>,
        <<"copy">> => <<"\u00A9">>,
        <<"reg">> => <<"\u00AE">>,
        <<"trade">> => <<"\u2122">>,
        <<"mdash">> => <<"\u2014">>,
        <<"ndash">> => <<"\u2013">>,
        <<"hellip">> => <<"\u2026">>,
        <<"lsquo">> => <<"\u2018">>,
        <<"rsquo">> => <<"\u2019">>,
        <<"ldquo">> => <<"\u201C">>,
        <<"rdquo">> => <<"\u201D">>,
        <<"bull">> => <<"\u2022">>,
        <<"prime">> => <<"\u2032">>,
        <<"Prime">> => <<"\u2033">>,

        %% Latin letters with diacritics
        <<"Agrave">> => <<"\u00C0">>,
        <<"Aacute">> => <<"\u00C1">>,
        <<"Acirc">> => <<"\u00C2">>,
        <<"Atilde">> => <<"\u00C3">>,
        <<"Auml">> => <<"\u00C4">>,
        <<"Aring">> => <<"\u00C5">>,
        <<"AElig">> => <<"\u00C6">>,
        <<"Ccedil">> => <<"\u00C7">>,
        <<"Egrave">> => <<"\u00C8">>,
        <<"Eacute">> => <<"\u00C9">>,
        <<"Ecirc">> => <<"\u00CA">>,
        <<"Euml">> => <<"\u00CB">>,
        <<"Igrave">> => <<"\u00CC">>,
        <<"Iacute">> => <<"\u00CD">>,
        <<"Icirc">> => <<"\u00CE">>,
        <<"Iuml">> => <<"\u00CF">>,
        <<"ETH">> => <<"\u00D0">>,
        <<"Ntilde">> => <<"\u00D1">>,
        <<"Ograve">> => <<"\u00D2">>,
        <<"Oacute">> => <<"\u00D3">>,
        <<"Ocirc">> => <<"\u00D4">>,
        <<"Otilde">> => <<"\u00D5">>,
        <<"Ouml">> => <<"\u00D6">>,
        <<"Oslash">> => <<"\u00D8">>,
        <<"Ugrave">> => <<"\u00D9">>,
        <<"Uacute">> => <<"\u00DA">>,
        <<"Ucirc">> => <<"\u00DB">>,
        <<"Uuml">> => <<"\u00DC">>,
        <<"Yacute">> => <<"\u00DD">>,
        <<"THORN">> => <<"\u00DE">>,
        <<"szlig">> => <<"\u00DF">>,
        <<"agrave">> => <<"\u00E0">>,
        <<"aacute">> => <<"\u00E1">>,
        <<"acirc">> => <<"\u00E2">>,
        <<"atilde">> => <<"\u00E3">>,
        <<"auml">> => <<"\u00E4">>,
        <<"aring">> => <<"\u00E5">>,
        <<"aelig">> => <<"\u00E6">>,
        <<"ccedil">> => <<"\u00E7">>,
        <<"egrave">> => <<"\u00E8">>,
        <<"eacute">> => <<"\u00E9">>,
        <<"ecirc">> => <<"\u00EA">>,
        <<"euml">> => <<"\u00EB">>,
        <<"igrave">> => <<"\u00EC">>,
        <<"iacute">> => <<"\u00ED">>,
        <<"icirc">> => <<"\u00EE">>,
        <<"iuml">> => <<"\u00EF">>,
        <<"eth">> => <<"\u00F0">>,
        <<"ntilde">> => <<"\u00F1">>,
        <<"ograve">> => <<"\u00F2">>,
        <<"oacute">> => <<"\u00F3">>,
        <<"ocirc">> => <<"\u00F4">>,
        <<"otilde">> => <<"\u00F5">>,
        <<"ouml">> => <<"\u00F6">>,
        <<"oslash">> => <<"\u00F8">>,
        <<"ugrave">> => <<"\u00F9">>,
        <<"uacute">> => <<"\u00FA">>,
        <<"ucirc">> => <<"\u00FB">>,
        <<"uuml">> => <<"\u00FC">>,
        <<"yacute">> => <<"\u00FD">>,
        <<"thorn">> => <<"\u00FE">>,
        <<"yuml">> => <<"\u00FF">>,

        %% Mathematical symbols
        <<"times">> => <<"\u00D7">>,
        <<"divide">> => <<"\u00F7">>,
        <<"minus">> => <<"\u2212">>,
        <<"plusmn">> => <<"\u00B1">>,
        <<"le">> => <<"\u2264">>,
        <<"ge">> => <<"\u2265">>,
        <<"ne">> => <<"\u2260">>,
        <<"equiv">> => <<"\u2261">>,
        <<"asymp">> => <<"\u2248">>,
        <<"infin">> => <<"\u221E">>,
        <<"sum">> => <<"\u2211">>,
        <<"prod">> => <<"\u220F">>,
        <<"int">> => <<"\u222B">>,
        <<"radic">> => <<"\u221A">>,
        <<"prop">> => <<"\u221D">>,
        <<"part">> => <<"\u2202">>,
        <<"forall">> => <<"\u2200">>,
        <<"exist">> => <<"\u2203">>,
        <<"empty">> => <<"\u2205">>,
        <<"nabla">> => <<"\u2207">>,
        <<"isin">> => <<"\u2208">>,
        <<"notin">> => <<"\u2209">>,
        <<"ni">> => <<"\u220B">>,

        %% Greek letters
        <<"Alpha">> => <<"\u0391">>,
        <<"Beta">> => <<"\u0392">>,
        <<"Gamma">> => <<"\u0393">>,
        <<"Delta">> => <<"\u0394">>,
        <<"Epsilon">> => <<"\u0395">>,
        <<"Zeta">> => <<"\u0396">>,
        <<"Eta">> => <<"\u0397">>,
        <<"Theta">> => <<"\u0398">>,
        <<"Iota">> => <<"\u0399">>,
        <<"Kappa">> => <<"\u039A">>,
        <<"Lambda">> => <<"\u039B">>,
        <<"Mu">> => <<"\u039C">>,
        <<"Nu">> => <<"\u039D">>,
        <<"Xi">> => <<"\u039E">>,
        <<"Omicron">> => <<"\u039F">>,
        <<"Pi">> => <<"\u03A0">>,
        <<"Rho">> => <<"\u03A1">>,
        <<"Sigma">> => <<"\u03A3">>,
        <<"Tau">> => <<"\u03A4">>,
        <<"Upsilon">> => <<"\u03A5">>,
        <<"Phi">> => <<"\u03A6">>,
        <<"Chi">> => <<"\u03A7">>,
        <<"Psi">> => <<"\u03A8">>,
        <<"Omega">> => <<"\u03A9">>,
        <<"alpha">> => <<"\u03B1">>,
        <<"beta">> => <<"\u03B2">>,
        <<"gamma">> => <<"\u03B3">>,
        <<"delta">> => <<"\u03B4">>,
        <<"epsilon">> => <<"\u03B5">>,
        <<"zeta">> => <<"\u03B6">>,
        <<"eta">> => <<"\u03B7">>,
        <<"theta">> => <<"\u03B8">>,
        <<"iota">> => <<"\u03B9">>,
        <<"kappa">> => <<"\u03BA">>,
        <<"lambda">> => <<"\u03BB">>,
        <<"mu">> => <<"\u03BC">>,
        <<"nu">> => <<"\u03BD">>,
        <<"xi">> => <<"\u03BE">>,
        <<"omicron">> => <<"\u03BF">>,
        <<"pi">> => <<"\u03C0">>,
        <<"rho">> => <<"\u03C1">>,
        <<"sigmaf">> => <<"\u03C2">>,
        <<"sigma">> => <<"\u03C3">>,
        <<"tau">> => <<"\u03C4">>,
        <<"upsilon">> => <<"\u03C5">>,
        <<"phi">> => <<"\u03C6">>,
        <<"chi">> => <<"\u03C7">>,
        <<"psi">> => <<"\u03C8">>,
        <<"omega">> => <<"\u03C9">>,

        %% Arrows
        <<"larr">> => <<"\u2190">>,
        <<"uarr">> => <<"\u2191">>,
        <<"rarr">> => <<"\u2192">>,
        <<"darr">> => <<"\u2193">>,
        <<"harr">> => <<"\u2194">>,
        <<"lArr">> => <<"\u21D0">>,
        <<"uArr">> => <<"\u21D1">>,
        <<"rArr">> => <<"\u21D2">>,
        <<"dArr">> => <<"\u21D3">>,
        <<"hArr">> => <<"\u21D4">>
    }.
