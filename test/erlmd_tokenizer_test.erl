%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for erlmd_tokenizer.
%%%
%%% Comprehensive tests for the tokenizer state machine driver.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_tokenizer_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").
-include("../src/tokenizer_internal.hrl").

%%%=============================================================================
%%% Creation Tests
%%%=============================================================================

new_tokenizer_test() ->
    T = erlmd_tokenizer:new(<<"hello">>, #{}),
    ?assertEqual(0, T#tokenizer.index),
    ?assertEqual(1, T#tokenizer.line),
    ?assertEqual(1, T#tokenizer.column),
    ?assertEqual([], T#tokenizer.events),
    ?assertEqual([], T#tokenizer.stack).

new_empty_test() ->
    T = erlmd_tokenizer:new(<<>>, #{}),
    ?assertEqual(0, T#tokenizer.index),
    ?assertEqual(<<>>, T#tokenizer.bytes).

new_with_options_test() ->
    Opts = #{gfm => true},
    T = erlmd_tokenizer:new(<<"test">>, Opts),
    ?assertEqual(Opts, T#tokenizer.options).

%%%=============================================================================
%%% Current/Consume Tests
%%%=============================================================================

current_on_first_byte_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

current_at_eof_test() ->
    T = erlmd_tokenizer:new(<<>>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    ?assertEqual(eof, erlmd_tokenizer:current(T1)).

consume_simple_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)),
    T2 = erlmd_tokenizer:consume(T1),
    ?assertEqual(1, T2#tokenizer.index),
    ?assertEqual(2, T2#tokenizer.column),
    ?assertEqual($a, T2#tokenizer.previous).

consume_sequence_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),

    ?assertEqual(3, T6#tokenizer.index),
    ?assertEqual(4, T6#tokenizer.column),
    ?assertEqual($c, T6#tokenizer.previous).

consume_to_eof_test() ->
    T = erlmd_tokenizer:new(<<"ab">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),
    T5 = erlmd_tokenizer:prepare_byte(T4),
    ?assertEqual(eof, erlmd_tokenizer:current(T5)).

%%%=============================================================================
%%% Line Ending Tests
%%%=============================================================================

consume_newline_test() ->
    T = erlmd_tokenizer:new(<<"a\nb">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % consume 'a'
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % consume '\n'

    ?assertEqual(2, T4#tokenizer.line),
    ?assertEqual(1, T4#tokenizer.column),
    ?assertEqual($\n, T4#tokenizer.previous).

consume_crlf_test() ->
    T = erlmd_tokenizer:new(<<"a\r\nb">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % consume 'a'
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % consume '\r' (ignored)
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),  % consume '\n'

    ?assertEqual(2, T6#tokenizer.line),
    ?assertEqual(1, T6#tokenizer.column).

consume_bare_cr_test() ->
    T = erlmd_tokenizer:new(<<"a\rb">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % consume 'a'
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % consume '\r' -> treated as '\n'

    ?assertEqual(2, T4#tokenizer.line),
    ?assertEqual(1, T4#tokenizer.column).

multiline_text_test() ->
    T = erlmd_tokenizer:new(<<"line1\nline2\nline3">>, #{}),

    %% Manually consume "line1\n" - 6 characters
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % l
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % i
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),  % n
    T7 = erlmd_tokenizer:prepare_byte(T6),
    T8 = erlmd_tokenizer:consume(T7),  % e
    T9 = erlmd_tokenizer:prepare_byte(T8),
    T10 = erlmd_tokenizer:consume(T9),  % 1
    T11 = erlmd_tokenizer:prepare_byte(T10),
    T12 = erlmd_tokenizer:consume(T11),  % \n

    ?assertEqual(2, T12#tokenizer.line),
    ?assertEqual(1, T12#tokenizer.column).

%%%=============================================================================
%%% Tab Expansion Tests
%%%=============================================================================

consume_tab_at_column_1_test() ->
    T = erlmd_tokenizer:new(<<"\tX">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),

    %% Tab at column 1 - current should return the tab or first virtual space
    Curr = erlmd_tokenizer:current(T1),
    ?assert(Curr =:= $\t orelse Curr =:= $\s),

    %% Consume the tab (may emit virtual spaces)
    T2 = erlmd_tokenizer:consume(T1),
    %% Column should have advanced
    ?assert(T2#tokenizer.column > 1).

consume_tab_at_column_3_test() ->
    T = erlmd_tokenizer:new(<<"ab\tX">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % a
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % b
    ?assertEqual(3, T4#tokenizer.column),

    %% Now consume tab
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),
    %% Column should have advanced
    ?assert(T6#tokenizer.column > 3).

%%%=============================================================================
%%% Event Tests
%%%=============================================================================

enter_exit_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:exit(T1, paragraph),

    ?assertEqual([], T2#tokenizer.stack),
    ?assertEqual(2, length(T2#tokenizer.events)),

    [Exit, Enter] = T2#tokenizer.events,  % Reversed
    ?assertEqual(enter, Enter#event.kind),
    ?assertEqual(paragraph, Enter#event.name),
    ?assertEqual(exit, Exit#event.kind),
    ?assertEqual(paragraph, Exit#event.name).

nested_events_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:enter(T1, emphasis),
    T3 = erlmd_tokenizer:exit(T2, emphasis),
    T4 = erlmd_tokenizer:exit(T3, paragraph),

    ?assertEqual([], T4#tokenizer.stack),
    ?assertEqual(4, length(T4#tokenizer.events)).

exit_mismatch_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    ?assertError({exit_mismatch, _}, erlmd_tokenizer:exit(T1, heading_atx)).

exit_without_enter_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    ?assertError({exit_without_enter, _}, erlmd_tokenizer:exit(T, paragraph)).

enter_with_link_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    Link = #link{content = text},
    T1 = erlmd_tokenizer:enter_with_link(T, paragraph, Link),

    ?assertEqual(1, length(T1#tokenizer.events)),
    [Event] = T1#tokenizer.events,
    ?assertEqual(Link, Event#event.link).

event_position_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),

    [Event] = T1#tokenizer.events,
    ?assertEqual(1, Event#event.point#point.line),
    ?assertEqual(1, Event#event.point#point.column),
    ?assertEqual(0, Event#event.point#point.offset).

%%%=============================================================================
%%% Attempt Tests
%%%=============================================================================

attempt_success_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:attempt(T, ok, nok),

    ?assertEqual(1, length(T1#tokenizer.attempts)),

    {ok, T2} = erlmd_tokenizer:handle_attempt_result(T1, ok),
    ?assertEqual(0, length(T2#tokenizer.attempts)).

attempt_failure_reverts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:attempt(T1, ok, nok),
    T3 = erlmd_tokenizer:enter(T2, emphasis),

    %% Should have 2 events and 2 stack items
    ?assertEqual(2, length(T3#tokenizer.events)),
    ?assertEqual(2, length(T3#tokenizer.stack)),

    %% Attempt fails - should revert to T1 state
    {nok, T4} = erlmd_tokenizer:handle_attempt_result(T3, nok),

    %% Should revert to 1 event and 1 stack item
    ?assertEqual(1, length(T4#tokenizer.events)),
    ?assertEqual(1, length(T4#tokenizer.stack)).

check_always_reverts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:check(T1, ok, nok),
    T3 = erlmd_tokenizer:enter(T2, emphasis),

    %% Check succeeds but still reverts
    {ok, T4} = erlmd_tokenizer:handle_attempt_result(T3, ok),

    %% Should revert to T1 state even though ok
    ?assertEqual(1, length(T4#tokenizer.events)),
    ?assertEqual(1, length(T4#tokenizer.stack)).

nested_attempts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:attempt(T, ok, nok),
    T2 = erlmd_tokenizer:attempt(T1, ok, nok),

    ?assertEqual(2, length(T2#tokenizer.attempts)),

    {ok, T3} = erlmd_tokenizer:handle_attempt_result(T2, ok),
    ?assertEqual(1, length(T3#tokenizer.attempts)),

    {ok, T4} = erlmd_tokenizer:handle_attempt_result(T3, ok),
    ?assertEqual(0, length(T4#tokenizer.attempts)).

attempt_with_position_changes_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:attempt(T1, ok, nok),
    T3 = erlmd_tokenizer:consume(T2),
    T4 = erlmd_tokenizer:prepare_byte(T3),
    T5 = erlmd_tokenizer:consume(T4),

    %% Should be at index 2
    ?assertEqual(2, T5#tokenizer.index),

    %% Revert should go back to index 0
    {nok, T6} = erlmd_tokenizer:handle_attempt_result(T5, nok),
    ?assertEqual(0, T6#tokenizer.index).

%%%=============================================================================
%%% Finalization Tests
%%%=============================================================================

finalize_empty_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    {ok, Events} = erlmd_tokenizer:finalize(T),
    ?assertEqual([], Events).

finalize_with_events_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:exit(T1, paragraph),

    {ok, Events} = erlmd_tokenizer:finalize(T2),
    ?assertEqual(2, length(Events)),

    [Enter, Exit] = Events,  % Corrected order
    ?assertEqual(enter, Enter#event.kind),
    ?assertEqual(exit, Exit#event.kind).

finalize_with_unclosed_tokens_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),

    {error, {unclosed_tokens, Stack}} = erlmd_tokenizer:finalize(T1),
    ?assertEqual([paragraph], Stack).

finalize_with_pending_attempts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:attempt(T, ok, nok),

    {error, {pending_attempts, Count}} = erlmd_tokenizer:finalize(T1),
    ?assertEqual(1, Count).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

simple_tokenize_test() ->
    %% This will use document dispatcher from erlmd_state
    %% Using empty input since most constructs are not yet implemented
    T = erlmd_tokenizer:new(<<>>, #{}),
    {ok, T1} = erlmd_tokenizer:feed(T, document, <<>>),
    {ok, _Events} = erlmd_tokenizer:finalize(T1).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Consume N bytes
consume_n(T, 0) -> T;
consume_n(T, N) ->
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),
    consume_n(T2, N - 1).

%% Consume until we hit a real byte (not virtual space)
consume_until_real_byte(T) ->
    consume_until_real_byte(T, 0).

consume_until_real_byte(T, Count) when Count > 10 ->
    %% Safety: don't loop forever
    T;
consume_until_real_byte(T, Count) ->
    case T#tokenizer.vs of
        0 ->
            %% On real byte or need to prepare
            case T#tokenizer.consumed of
                true ->
                    T1 = erlmd_tokenizer:prepare_byte(T),
                    case erlmd_tokenizer:current(T1) of
                        eof -> T1;
                        _ -> T1
                    end;
                false ->
                    T
            end;
        _ ->
            %% Still in virtual spaces
            T1 = erlmd_tokenizer:prepare_byte(T),
            T2 = erlmd_tokenizer:consume(T1),
            consume_until_real_byte(T2, Count + 1)
    end.
