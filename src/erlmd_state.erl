%%%-----------------------------------------------------------------------------
%%% @doc State dispatcher - Maps state names to construct functions.
%%%
%%% This module is THE central dispatch point for all state transitions.
%%% Each state name maps to a construct module and function.
%%%
%%% In Phase 2, we only implement stubs. Real constructs come in later phases.
%%%
%%% Based on markdown-rs src/state.rs
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_state).

-include("types.hrl").

-export([call/2]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec call(atom(), erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Dispatch to a state function by name.
%%
%% This is THE central dispatch point for all state transitions.
%% Each state name maps to a construct module and function.
%%
%% In Phase 2, we only implement stubs. Real constructs come in later phases.
call(StateName, Tokenizer) ->
    case StateName of
        %%=====================================================================
        %% Phase 4: Content Dispatchers
        %%=====================================================================

        %% Document dispatcher (top-level)
        document -> erlmd_cnstr_document:start(Tokenizer);

        %% Flow dispatcher (block-level)
        flow -> erlmd_cnstr_flow:start(Tokenizer);

        %% Text dispatcher (inline)
        text -> erlmd_cnstr_text:start(Tokenizer);

        %% String dispatcher (simplest)
        string -> erlmd_cnstr_string:start(Tokenizer);

        %%=====================================================================
        %% Phase 3: Simple Constructs
        %%=====================================================================

        %% Data construct (partial)
        data -> erlmd_cnstr_prtl_data:start(Tokenizer);
        data_start -> erlmd_cnstr_prtl_data:start(Tokenizer);
        data_at_break -> erlmd_cnstr_prtl_data:at_break(Tokenizer);
        data_inside -> erlmd_cnstr_prtl_data:inside(Tokenizer);

        %% Blank line construct
        blank_line -> erlmd_cnstr_blank_line:start(Tokenizer);
        blank_line_start -> erlmd_cnstr_blank_line:start(Tokenizer);
        parse_whitespace -> erlmd_cnstr_blank_line:parse_whitespace(Tokenizer);
        check_after_whitespace -> erlmd_cnstr_blank_line:check_after_whitespace(Tokenizer);
        blank_line_after -> erlmd_cnstr_blank_line:after_whitespace(Tokenizer);

        %% Space or tab construct (partial)
        space_or_tab -> erlmd_cnstr_prtl_space_or_tab:start(Tokenizer);
        space_or_tab_start -> erlmd_cnstr_prtl_space_or_tab:start(Tokenizer);
        space_or_tab_inside -> erlmd_cnstr_prtl_space_or_tab:inside(Tokenizer);
        space_or_tab_after -> erlmd_cnstr_prtl_space_or_tab:after_space_or_tab(Tokenizer);

        %%=====================================================================
        %% Stub Handlers for Unimplemented Constructs
        %%=====================================================================

        %% BOM (Byte Order Mark)
        bom -> stub_nok(bom, Tokenizer);

        %% Character escapes and references
        character_escape -> stub_nok(character_escape, Tokenizer);
        character_reference -> stub_nok(character_reference, Tokenizer);

        %% Block constructs (Phase 5+)
        %% Paragraph (implemented in Phase 5)
        paragraph -> erlmd_cnstr_paragraph:start(Tokenizer);
        paragraph_line_start -> erlmd_cnstr_paragraph:line_start(Tokenizer);
        paragraph_inside -> erlmd_cnstr_paragraph:inside(Tokenizer);
        paragraph_after_line -> erlmd_cnstr_paragraph:after_line(Tokenizer);

        %% ATX Heading (implemented in Phase 5)
        heading_atx -> erlmd_cnstr_heading_atx:start(Tokenizer);
        heading_atx_before -> erlmd_cnstr_heading_atx:before(Tokenizer);
        heading_atx_after_prefix -> erlmd_cnstr_heading_atx:after_prefix(Tokenizer);
        heading_atx_sequence_open -> erlmd_cnstr_heading_atx:sequence_open(Tokenizer);
        heading_atx_after_sequence -> erlmd_cnstr_heading_atx:after_sequence(Tokenizer);
        heading_atx_content_inside -> erlmd_cnstr_heading_atx:content_inside(Tokenizer);

        heading_setext -> stub_nok(heading_setext, Tokenizer);

        %% Thematic break (implemented in Phase 5)
        thematic_break -> erlmd_cnstr_thematic_break:start(Tokenizer);
        thematic_break_inside -> erlmd_cnstr_thematic_break:inside(Tokenizer);
        thematic_break_at_break -> erlmd_cnstr_thematic_break:at_break(Tokenizer);
        thematic_break_consume_whitespace -> erlmd_cnstr_thematic_break:consume_whitespace(Tokenizer);
        thematic_break_consume_prefix -> erlmd_cnstr_thematic_break:after_prefix(Tokenizer);

        code_indented -> stub_nok(code_indented, Tokenizer);
        raw_flow -> stub_nok(raw_flow, Tokenizer);
        html_flow -> stub_nok(html_flow, Tokenizer);
        definition -> stub_nok(definition, Tokenizer);

        %% Inline constructs (Phase 6+)
        label_start_image -> stub_nok(label_start_image, Tokenizer);
        label_start_link -> stub_nok(label_start_link, Tokenizer);
        label_end -> stub_nok(label_end, Tokenizer);
        raw_text -> stub_nok(raw_text, Tokenizer);
        attention -> stub_nok(attention, Tokenizer);
        autolink -> stub_nok(autolink, Tokenizer);
        html_text -> stub_nok(html_text, Tokenizer);
        hard_break_escape -> stub_nok(hard_break_escape, Tokenizer);

        %% GFM constructs (Phase 11+)
        gfm_task_list_item_check -> stub_nok(gfm_task_list_item_check, Tokenizer);
        gfm_autolink_literal -> stub_nok(gfm_autolink_literal, Tokenizer);
        gfm_label_start_footnote -> stub_nok(gfm_label_start_footnote, Tokenizer);
        gfm_table -> stub_nok(gfm_table, Tokenizer);

        %% MDX constructs (Phase 12+)
        mdx_jsx_text -> stub_nok(mdx_jsx_text, Tokenizer);
        mdx_jsx_flow -> stub_nok(mdx_jsx_flow, Tokenizer);
        mdx_expression_text -> stub_nok(mdx_expression_text, Tokenizer);
        mdx_expression_flow -> stub_nok(mdx_expression_flow, Tokenizer);
        mdx_esm -> stub_nok(mdx_esm, Tokenizer);

        %% Unknown state - error
        _ ->
            error({unknown_state, StateName})
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
%% Stub that returns nok - for constructs not yet implemented.
%% This allows dispatchers to try the construct, fail gracefully, and move to next.
stub_nok(_StateName, Tokenizer) ->
    {nok, Tokenizer}.
