%%%-----------------------------------------------------------------------------
%%% @doc Core type definitions for erlmd markdown parser.
%%%
%%% This header file defines the fundamental record types used throughout
%%% the parser:
%%% - Point: A single location in source (line, column, offset)
%%% - Position: A span from start point to end point
%%% - Event: The core unit of parsing (kind, name, point, link, content)
%%% - Link: Connects events for nested content
%%% - Message: Error/warning messages with location
%%%
%%% These types mirror the unist.rs and event.rs structures from markdown-rs.
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(ERLMD_TYPES_HRL).
-define(ERLMD_TYPES_HRL, true).

%%%=============================================================================
%%% Point - A single position in the source document
%%%=============================================================================

-record(point, {
    line = 1 :: pos_integer(),      % 1-indexed line number
    column = 1 :: pos_integer(),    % 1-indexed column number
    offset = 0 :: non_neg_integer() % 0-indexed byte position
}).

-type point() :: #point{}.

%%%=============================================================================
%%% Position - A span from start to end
%%%=============================================================================

-record(position, {
    start :: point(),
    'end' :: point()
}).

-type position() :: #position{}.

%%%=============================================================================
%%% Content Type - Hierarchy of content types
%%%=============================================================================

-type content_type() :: flow | content | string | text.

%%%=============================================================================
%%% Link - Connects events for nested content
%%%=============================================================================

-record(link, {
    previous = undefined :: non_neg_integer() | undefined,
    next = undefined :: non_neg_integer() | undefined,
    content :: content_type()
}).

-type link() :: #link{}.

%%%=============================================================================
%%% Event - Core parsing unit
%%%=============================================================================

%% Event names - the complete set from markdown-rs event.rs Name enum
%% Starting with the core set needed for Phase 1-6, will expand as needed
-type event_name() ::
    %% Core structural
    document | paragraph | heading_atx | heading_setext | thematic_break |
    code_fenced | code_indented | code_text |
    block_quote | list_item | list_ordered | list_unordered |

    %% Inline
    emphasis | strong | link | image | data |
    character_escape | character_reference |

    %% Markers and tokens
    heading_atx_text | heading_atx_sequence |
    heading_setext_text | heading_setext_underline |
    thematic_break_sequence |
    line_ending | space_or_tab |

    %% Code
    code_fenced_fence | code_fenced_fence_info | code_fenced_fence_meta |
    code_text_data | code_text_sequence | code_text_padding |

    %% Blockquote
    block_quote_prefix | block_quote_marker |

    %% Lists
    list_item_prefix | list_item_value | list_item_marker |

    %% Character references
    character_reference_marker | character_reference_value |

    %% Links and images
    link_text | link_label | link_marker | link_resource | link_destination |
    link_title | link_reference |
    image_text | image_label | image_marker | image_resource | image_destination |
    image_title | image_reference |

    %% Emphasis and strong
    emphasis_text | emphasis_sequence |
    strong_text | strong_sequence |

    %% HTML
    html_flow | html_text |

    %% Hard breaks
    hard_break | hard_break_escape | hard_break_trailing |

    %% GFM extensions (for future use)
    gfm_table | gfm_table_head | gfm_table_body | gfm_table_row | gfm_table_cell |
    gfm_strikethrough | gfm_strikethrough_text | gfm_strikethrough_sequence |
    gfm_autolink_literal | gfm_autolink_literal_protocol |
    gfm_autolink_literal_www | gfm_autolink_literal_email |
    gfm_footnote_call | gfm_footnote_definition |
    gfm_task_list_item_check |

    %% Frontmatter (for future use)
    frontmatter | frontmatter_sequence | frontmatter_data |

    %% Math (for future use)
    math_flow | math_text.

-record(event, {
    kind :: enter | exit,
    name :: event_name(),
    point :: point(),
    link = undefined :: link() | undefined,
    content = undefined :: content_type() | undefined
}).

-type event() :: #event{}.

%%%=============================================================================
%%% Message - Error and warning messages
%%%=============================================================================

-type place() :: {position, position()} | {point, point()}.

-record(message, {
    place = undefined :: place() | undefined,
    reason :: binary() | string(),
    rule_id :: binary() | atom(),
    source :: binary() | atom()
}).

-type message() :: #message{}.

%%%=============================================================================
%%% Type guards (for runtime checks)
%%%=============================================================================

%% These can be used in guards for pattern matching
-define(is_point(P), is_record(P, point)).
-define(is_position(P), is_record(P, position)).
-define(is_event(E), is_record(E, event)).
-define(is_link(L), is_record(L, link)).
-define(is_message(M), is_record(M, message)).

-endif. % ERLMD_TYPES_HRL
