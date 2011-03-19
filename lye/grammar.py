from pymeta.grammar import OMeta

grammar = """

lilypond ::= (<token "\\invalid">
    | <toplevel_expression> | <assignment> | <error>)*

toplevel_expression ::= <lilypond_header>
    | <book_block>
    | <bookpart_block>
    | <score_block>
    | <composite_music>
    | <full_markup>
    | <full_markup_list>
    | <output_def>

embedded_scm ::= <SCM_TOKEN> | <SCM_IDENTIFIER>

lilypond_header_body ::= <token "">
    | <lilypond_header_body> <assignment>

lilypond_header ::=
    <token "\\header"> <token "{"> <lilypond_header_body>:lhb <token "{">
    => lhb

assignment_id ::= <STRING>
    | <LYRICS_STRING>

assignment ::= <assignment_id> <token "="> <identifier_init>
    | <assignment_id> <property_path> <token "="> <identifier_init>
    | <embedded_scm>

identifier_init ::= <score_block>
    | <book_block>
    | <bookpart_block>
    | <output_def>
    | <context_def_spec_block>
    | <music>
    | <post_event>
    | <number_expression>
    | <string>
    | <embedded_scm>
    | <full_markup>
    | <full_markup_list>
    | <DIGIT>
    | <context_modification>

context_def_spec_block ::=
    <token "\\context"> <token "{"> <context_def_spec_body>:cdsb <token "}">
    => cdsb

context_def_spec_body ::= <token "">
    | <CONTEXT_DEF_IDENTIFIER>
    | <context_def_spec_body> <token "\\grobdescriptions"> <embedded_scm>
    | <context_def_spec_body> <context_mod>
    | <context_def_spec_body> <context_modification>

book_block ::= <token "\\book"> <token "{"> <book_body>:bb <token "}">
    => bb

book_body ::= <token "">
    | <BOOK_IDENTIFIER>
    | <book_body> <paper_block>
    | <book_body> <bookpart_block>
    | <book_body> <score_block>
    | <book_body> <composite_music>
    | <book_body> <full_markup>
    | <book_body> <full_markup_list>
    | <book_body> <lilypond_header>
    | <book_body> <error>

bookpart_block ::=
    <token "\\bookpart"> <token "{"> <bookpart_body>:bb <token "}">
    => bb

bookpart_body ::= <token "">
    | <BOOK_IDENTIFIER>
    | <bookpart_body> <paper_block>
    | <bookpart_body> <score_block>
    | <bookpart_body> <composite_music>
    | <bookpart_body> <full_markup>
    | <bookpart_body> <full_markup_list>
    | <bookpart_body> <lilypond_header>
    | <bookpart_body> <error>

score_block ::= <token "\\score"> <token "{"> <score_body>:sb <token "}">
    => sb

score_body ::= <music>
    | <SCORE_IDENTIFIER>
    | <score_body> <lilypond_header>
    | <score_body> <output_def>
    | <score_body> <error>

paper_block ::= <output_def>

output_def ::= <output_def_body> <token "}">

output_def_head ::= <token "\\paper">
    | <token "\\midi">
    | <token "\\layout">

output_def_body ::= <output_def_head> <token "{">
    | <output_def_head> <token "{"> <OUTPUT_DEF_IDENTIFIER>
    | <output_def_body> <assignment>
    | <output_def_body> <context_def_spec_block>
    | <output_def_body> <error>

tempo_event ::= <token "\\tempo"> <steno_duration> <token "="> <tempo_range>
    | <token "\\tempo"> <scalar> <steno_duration> <token "="> <tempo_range>
    | <token "\\tempo"> <scalar>

music_list ::= <token "">
    | <music_list> <music>
    | <music_list> <embedded_scm>
    | <music_list> <error>

music ::= <simple_music>
    | <composite_music>

alternative_music ::= <token "">
    | <token "\\alternative"> <token "{"> <music_list>:ml <token "}">
    => ml

repeated_music ::= <token "\\repeat">
    | <simple_string>
    | <unsigned_number>
    | <music>
    | <alternative_music>

sequential_music ::=
    | <token "\\sequential"> <token "{"> <music_list>:ml <token "}">
    | <token "{"> <music_list>:ml <token "}">
    => ml

simultaneous_music ::=
    | <token "\\simultaneous"> <token "{"> <music_list>:ml <token "}">
    | <token "<<"> <music_list>:ml <token ">>">
    => ml

simple_music ::= <event_chord>
    | <MUSIC_IDENTIFIER>
    | <music_property_def>
    | <context_change>

context_modification ::=
    <token "\\with"> <token "$"> <token "{"> <context_mod_list> <token "}">
    | <token "\\with"> <CONTEXT_MOD_IDENTIFIER>
    | <CONTEXT_MOD_IDENTIFIER>

optional_context_mod ::= <token ""> | <context_modification>

context_mod_list ::= <token "">
    | <context_mod_list> <context_mod>
    | <context_mod_list> <CONTEXT_MOD_IDENTIFIER>

composite_music ::= <prefix_composite_music>
    | <grouped_music_list>

grouped_music_list ::= <simultaneous_music>
    | <sequential_music>

function_scm_argument ::= <embedded_scm>
    | <simple_string>

function_arglist_music_last ::= <EXPECT_MUSIC> <function_arglist> <music>

function_arglist_nonmusic_last ::=
    <EXPECT_MARKUP> <function_arglist> <full_markup>
    | <EXPECT_MARKUP> <function_arglist> <simple_string>
    | <EXPECT_SCM> <function_arglist> <function_scm_argument>

function_arglist_nonmusic ::= <EXPECT_NO_MORE_ARGS>
    | <EXPECT_MARKUP> <function_arglist_nonmusic> <full_markup>
    | <EXPECT_MARKUP> <function_arglist_nonmusic> <simple_string>
    | <EXPECT_SCM> <function_arglist_nonmusic> <function_scm_argument>

function_arglist ::= <EXPECT_NO_MORE_ARGS>
    | <function_arglist_music_last>
    | <function_arglist_nonmusic_last>

generic_prefix_music_scm ::= <MUSIC_FUNCTION> <function_arglist>

"""
"""
Port of the Lilypond grammar to PyMeta. This is based on the 2.13 Ly grammar.
"""

class LyeGrammar(OMeta.makeGrammar(grammar, globals())):
    pass
