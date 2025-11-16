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

-spec call(atom(), erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
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

        %% Space or tab with EOL construct (partial)
        space_or_tab_eol -> erlmd_cnstr_prtl_space_or_tab_eol:start(Tokenizer);
        space_or_tab_eol_start -> erlmd_cnstr_prtl_space_or_tab_eol:start(Tokenizer);
        space_or_tab_eol_inside -> erlmd_cnstr_prtl_space_or_tab_eol:inside(Tokenizer);
        space_or_tab_eol_after -> erlmd_cnstr_prtl_space_or_tab_eol:after_whitespace(Tokenizer);

        %%=====================================================================
        %% Stub Handlers for Unimplemented Constructs
        %%=====================================================================

        %% BOM (Byte Order Mark)
        bom -> stub_nok(bom, Tokenizer);

        %%=====================================================================
        %% Phase 6: Basic Inline Constructs
        %%=====================================================================

        %% Character escape (implemented in Phase 6)
        character_escape -> erlmd_cnstr_character_escape:start(Tokenizer);
        character_escape_inside -> erlmd_cnstr_character_escape:inside(Tokenizer);

        %% Hard break escape (implemented in Phase 6)
        hard_break_escape -> erlmd_cnstr_hard_break_escape:start(Tokenizer);
        hard_break_escape_after -> erlmd_cnstr_hard_break_escape:after_escape(Tokenizer);

        %% Code text / inline code (implemented in Phase 6)
        code_text -> erlmd_cnstr_code_text:start(Tokenizer);
        code_text_sequence_open -> erlmd_cnstr_code_text:sequence_open(Tokenizer);
        code_text_between -> erlmd_cnstr_code_text:between(Tokenizer);
        code_text_data -> erlmd_cnstr_code_text:data(Tokenizer);
        code_text_sequence_close -> erlmd_cnstr_code_text:sequence_close(Tokenizer);

        %% Label start image states (Phase 7)
        label_start_image_open -> erlmd_cnstr_label_start_image:open(Tokenizer);
        label_start_image_after_open -> erlmd_cnstr_label_start_image:after_open(Tokenizer);

        %% Partial label states (Phase 7)
        prtl_label_start -> erlmd_cnstr_prtl_label:start(Tokenizer);
        prtl_label_at_break -> erlmd_cnstr_prtl_label:at_break(Tokenizer);
        prtl_label_inside -> erlmd_cnstr_prtl_label:label_inside(Tokenizer);
        prtl_label_escape -> erlmd_cnstr_prtl_label:label_escape(Tokenizer);
        prtl_label_eol_after -> erlmd_cnstr_prtl_label:eol_after(Tokenizer);
        prtl_label_nok -> erlmd_cnstr_prtl_label:label_nok(Tokenizer);

        %% Partial destination states (Phase 7)
        prtl_destination_start -> erlmd_cnstr_prtl_destination:start(Tokenizer);
        prtl_destination_enclosed_before -> erlmd_cnstr_prtl_destination:enclosed_before(Tokenizer);
        prtl_destination_enclosed -> erlmd_cnstr_prtl_destination:enclosed(Tokenizer);
        prtl_destination_enclosed_escape -> erlmd_cnstr_prtl_destination:enclosed_escape(Tokenizer);
        prtl_destination_raw -> erlmd_cnstr_prtl_destination:raw(Tokenizer);
        prtl_destination_raw_escape -> erlmd_cnstr_prtl_destination:raw_escape(Tokenizer);

        %% Partial title states (Phase 7)
        prtl_title_start -> erlmd_cnstr_prtl_title:start(Tokenizer);
        prtl_title_begin -> erlmd_cnstr_prtl_title:title_begin(Tokenizer);
        prtl_title_at_break -> erlmd_cnstr_prtl_title:title_at_break(Tokenizer);
        prtl_title_inside -> erlmd_cnstr_prtl_title:title_inside(Tokenizer);
        prtl_title_escape -> erlmd_cnstr_prtl_title:title_escape(Tokenizer);
        prtl_title_after_eol -> erlmd_cnstr_prtl_title:title_after_eol(Tokenizer);
        prtl_title_nok -> erlmd_cnstr_prtl_title:title_nok(Tokenizer);

        %% Character reference (implemented in Phase 6)
        character_reference -> erlmd_cnstr_character_reference:start(Tokenizer);
        character_reference_open -> erlmd_cnstr_character_reference:open(Tokenizer);
        character_reference_numeric -> erlmd_cnstr_character_reference:numeric(Tokenizer);
        character_reference_value -> erlmd_cnstr_character_reference:value(Tokenizer);

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

        %%=====================================================================
        %% Phase 8: Complex Block Constructs
        %%=====================================================================

        %% Block quote (implemented in Phase 8.1)
        block_quote -> erlmd_cnstr_block_quote:start(Tokenizer);
        block_quote_cont_start -> erlmd_cnstr_block_quote:cont_start(Tokenizer);
        block_quote_cont_after -> erlmd_cnstr_block_quote:cont_after(Tokenizer);
        block_quote_cont_before -> erlmd_cnstr_block_quote:cont_before(Tokenizer);

        %% List item (implemented in Phase 8.2)
        list_item -> erlmd_cnstr_list_item:start(Tokenizer);
        list_item_cont_start -> erlmd_cnstr_list_item:cont_start(Tokenizer);
        list_item_before -> erlmd_cnstr_list_item:before(Tokenizer);
        list_item_before_unordered -> erlmd_cnstr_list_item:before_unordered(Tokenizer);
        list_item_value -> erlmd_cnstr_list_item:value(Tokenizer);
        list_item_marker -> erlmd_cnstr_list_item:marker(Tokenizer);
        list_item_after -> erlmd_cnstr_list_item:after_marker(Tokenizer);
        list_item_after_whitespace -> erlmd_cnstr_list_item:after_whitespace(Tokenizer);

        %% Indented code (implemented in Phase 5 - simplified)
        code_indented -> erlmd_cnstr_code_indented:start(Tokenizer);
        code_indented_after_prefix -> erlmd_cnstr_code_indented:after_prefix(Tokenizer);
        code_indented_begin_line -> erlmd_cnstr_code_indented:begin_line(Tokenizer);
        code_indented_content_inside -> erlmd_cnstr_code_indented:content_inside(Tokenizer);
        code_indented_further_start -> erlmd_cnstr_code_indented:further_start(Tokenizer);
        code_indented_further_end -> erlmd_cnstr_code_indented:further_end(Tokenizer);
        raw_flow -> stub_nok(raw_flow, Tokenizer);
        html_flow -> stub_nok(html_flow, Tokenizer);
        definition -> stub_nok(definition, Tokenizer);

        %% Inline constructs (Phase 6+)
        label_start_image -> erlmd_cnstr_label_start_image:start(Tokenizer);
        label_start_link -> erlmd_cnstr_label_start_link:start(Tokenizer);

        %% Label end states (Phase 7.4 + 7.5)
        label_end -> erlmd_cnstr_label_end:start(Tokenizer);
        label_end_after_marker -> erlmd_cnstr_label_end:after_marker(Tokenizer);
        label_end_ok -> erlmd_cnstr_label_end:label_end_ok(Tokenizer);
        label_end_nok -> erlmd_cnstr_label_end:label_end_nok(Tokenizer);

        %% Resource parsing (Phase 7.4)
        label_end_resource_start -> erlmd_cnstr_label_end:resource_start(Tokenizer);
        label_end_resource_before -> erlmd_cnstr_label_end:resource_before(Tokenizer);
        label_end_resource_open -> erlmd_cnstr_label_end:resource_open(Tokenizer);
        label_end_resource_destination_after -> erlmd_cnstr_label_end:resource_destination_after(Tokenizer);
        label_end_resource_destination_missing -> erlmd_cnstr_label_end:resource_destination_missing(Tokenizer);
        label_end_resource_between -> erlmd_cnstr_label_end:resource_between(Tokenizer);
        label_end_resource_title_after -> erlmd_cnstr_label_end:resource_title_after(Tokenizer);
        label_end_resource_end -> erlmd_cnstr_label_end:resource_end(Tokenizer);

        %% Reference parsing (Phase 7.5)
        label_end_reference_full_start -> erlmd_cnstr_label_end:reference_full_start(Tokenizer);
        label_end_reference_collapsed_start -> erlmd_cnstr_label_end:reference_collapsed_start(Tokenizer);
        label_end_reference_shortcut -> erlmd_cnstr_label_end:reference_shortcut(Tokenizer);
        raw_text -> stub_nok(raw_text, Tokenizer);
        attention -> erlmd_cnstr_attention:start(Tokenizer);
        attention_inside -> erlmd_cnstr_attention:inside(Tokenizer);
        autolink -> stub_nok(autolink, Tokenizer);
        html_text -> stub_nok(html_text, Tokenizer);

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
